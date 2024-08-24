package contextus.service

import zio.*
import contextus.model.DomainError
import contextus.json.*
import contextus.model.DomainError.ValidationError
import contextus.util.ZipUtil

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.PosixFilePermission

trait UpdateService:
  def update(version: String, arch: UpdateService.Arch): IO[UpdateService.Error, String]
  def updateLatest(arch: UpdateService.Arch): IO[UpdateService.Error, String]

object UpdateService:
  type Error = HttpService.Error | DomainError.ValidationError | ContextusFileService.Error

  val live = ZLayer.fromFunction(Live.apply)

  enum Arch:
    case X64, ARM

    def text: String = this match
      case Arch.X64 => "x64"
      case Arch.ARM => "arm"

  object Arch:
    def parse(archString: String): Either[String, Arch] =
      archString.trim.toLowerCase match
        case "x64" => Right(Arch.X64)
        case "arm" => Right(Arch.ARM)
        case invalid => Left(s"Unsupported architecture type $invalid. Supported architectures are x64 and ARM")

    given Encoder[Arch] = Encoder.encodeString.contramap((a: Arch) => a.text)
    given Decoder[Arch] = Decoder.decodeString.emap(parse)

  final case class Release(
    assets: List[Release.Asset],
    name: String,
  ):
    def getUrl(arch: Arch): Option[String] =
      assets.find(_.name.contains(arch.text))
        .map(_.browser_download_url)

  object Release:
    given Decoder[Release] = deriveDecoder

    final case class Asset(name: String, browser_download_url: String)
    object Asset:
      given Decoder[Asset] = deriveDecoder

  final case class Live(
    httpService: HttpService,
    fileService: ContextusFileService,
  ) extends UpdateService:
    private def updateImpl(version: Option[String], arch: Arch): IO[Error, String] =
      for
        versionUrl <- getVersionUrl(version)
        releaseResponse <- httpService.get(versionUrl)
        _ <- ZIO.fail[DomainError.IOError.HttpIOError](DomainError.IOError.HttpIOError(versionUrl, DomainError.HttpMethod.Get, s"Could not retrieve release: status ${releaseResponse.status}", None))
          .when(releaseResponse.status != 200)
        releaseJson <- releaseResponse.body.mapError[DomainError.IOError.HttpIOError](e => DomainError.IOError.HttpIOError(versionUrl, DomainError.HttpMethod.Get, s"Could not parse body", None))
        releaseData <- releaseJson.decodeJsonZIO[Release]
        downloadUrl <- releaseData.getUrl(arch).fold(
          ZIO.fail(DomainError.ValidationError("release download url", Some(releaseJson), Some(s"Could not find release asset for ${arch} in release data"))),
        )(ZIO.succeed)
        tstRes <- httpService.get(downloadUrl)
        releaseAssetResponse <- httpService.getBinary(downloadUrl)
        _ <- ZIO.fail[DomainError.IOError.HttpIOError](DomainError.IOError.HttpIOError(downloadUrl, DomainError.HttpMethod.Get, s"Could not retrieve release payload: status ${releaseAssetResponse.status}", None))
          .when(releaseAssetResponse.status != 200)
        releaseAsset <- releaseAssetResponse.body.mapError[DomainError.IOError.HttpIOError](e => DomainError.IOError.HttpIOError(versionUrl, DomainError.HttpMethod.Get, s"Could not parse body", None))
        data <- ZipUtil.unzip(releaseAsset.toArray).runCollect
        _ <- fileService.useTemporaryFiles(data, true) { paths =>
           paths.find(_.filename.toString == "install.command") match {
            case Some(path) =>  fileService.runScript(path)
            case None => ZIO.fail[Error](ValidationError("release archive", None, Some("install.command does not exist in release archive")))
          }
        }
      yield releaseData.name

    override def updateLatest(arch: Arch): IO[Error, String] = updateImpl(None, arch)
    override def update(version: String, arch: Arch): IO[Error, String] = updateImpl(Some(version), arch)

    def getVersionUrl(version: Option[String]): IO[DomainError.ValidationError, String] =
      val VersionPattern = """v?(\d+.\d+.\d+)""".r

      val versionSegment = version match
        case None => ZIO.succeed("latest")
        case Some("") => ZIO.fail(DomainError.ValidationError("version", Some(""), Some("empty version")))
        case Some(VersionPattern(v)) => ZIO.succeed(s"tags/v$v")
        case Some(other) => ZIO.fail(DomainError.ValidationError("version", Some(other), Some("version must have form: v#.#.# or just #.#.#")))

      versionSegment.map(vs => s"https://api.github.com/repos/johnhungerford/contextus-cli/releases/$vs")


