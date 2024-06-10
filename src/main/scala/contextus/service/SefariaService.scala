package contextus.service

import zio.*
import io.circe.*
import io.circe.syntax.*
import contextus.model.DomainError.*
import contextus.model.DomainError.IOError.HttpIOError
import contextus.model.sefaria.{SefariaCategory, SefariaCategoryUpdate, SefariaError, SefariaIndex, SefariaIndexEntry, SefariaRef, SefariaTextSubmission}
import contextus.json.*


trait SefariaService:
	def addEntryToIndex(submission: SefariaIndexEntry): IO[SefariaService.Error, Unit]
	def addText(ref: SefariaRef, submission: SefariaTextSubmission): IO[SefariaService.Error, Unit]

	def getIndex: IO[SefariaService.Error, SefariaIndex]

	def getIndexEntry(ref: SefariaRef): IO[SefariaService.Error, SefariaIndexEntry]

	def updateCategories(update: SefariaCategoryUpdate): IO[SefariaService.Error, Unit]

	def getCategories: IO[SefariaService.Error, List[SefariaCategory]] =
		getIndex.map(index => index.categories)

	def getTitles(parentCategories: List[String] = Nil): IO[SefariaService.Error, List[String]] =
		getIndex.map(_.titles(parentCategories))

	def deleteDoc(title: String): IO[SefariaService.Error, Unit]


object SefariaService:
	type Error = IOError.HttpIOError | DecodingError | SefariaApiError

	val live = ZLayer.scoped(for {
		conf <- ZIO.service[Conf]
		httpService <- ZIO.service[HttpService]
		cache <- ZIO.serviceWithZIO[CachingService](_.cache[SefariaIndex])
	} yield Live(conf, httpService, cache))

	final case class Conf(
		baseUrl: String,
		apiKey: String,
	)

	object Conf:
		val live = ZLayer(for {
			configService <- ZIO.service[ConfigurationService]
			apiKeyOpt <- configService.getApiKey
			apiKey = apiKeyOpt.getOrElse("")
			url <- configService.getBaseUrl
		} yield Conf(url, apiKey))

	final case class Live(
		conf: Conf,
		httpService: HttpService,
		indexCache: Cache[SefariaIndex],
	) extends SefariaService:
		private val apiKey = conf.apiKey
		private val correctedBaseUrl = conf.baseUrl.stripSuffix("/")
		private val indexUrl = correctedBaseUrl + "/api/index"
		private val textUrl = correctedBaseUrl + "/api/texts"
		private val categoryUrl = correctedBaseUrl + "/api/category"

		private def formData[A: Encoder](value: A): Map[String, String] =
			val data = value.asJson.noSpaces
			Map(
				"json" -> data,
				"apikey" -> apiKey,
			)

		private def refToPath(title: String): String =
			title.trim.replace(' ', '_')


		private def getBodyHandlingError(response: HttpService.Response, url: String, method: HttpMethod, status: Option[Int], baseErrorMessage: String): IO[Error, Option[String]] =
			response.body.foldZIO(
				{
					case () => ZIO.succeed(None)
					case err: Throwable => ZIO.fail(HttpIOError(url, method, s"$baseErrorMessage: failure reading body", Some(err)))
				},
				{
					case SefariaError.IsError(message) =>
						ZIO.fail(SefariaApiError(url, method, Some(s"$baseErrorMessage: $message"), status, None))
					case other => ZIO.succeed(Some(other))
				}
			)

		override def addEntryToIndex(submission: SefariaIndexEntry): IO[Error, Unit] = indexCache.invalidating(()):
			val titlePath = refToPath(submission.title)
			val url = indexUrl + "/" + titlePath

			val baseErrorMessage = s"Failed to index ${submission.title}"

			httpService
				.postForm(url, formData(submission))
				.mapError {
					case err: HttpIOError => err.copy(problem = s"$baseErrorMessage: ${err.problem}")
					case err: DecodingError => err.copy(problem = s"$baseErrorMessage: ${err.problem}")
				}
				.flatMap {
					case HttpService.Response(status, body) if status < 200 || status > 201 =>
						val errorMessage = s"$baseErrorMessage: $status was not 200 or 201"
						body
							.mapError {
								case () => SefariaApiError(url, HttpMethod.Post, Some(errorMessage), Some(status), None)
								case err: Throwable => SefariaApiError(url, HttpMethod.Post, Some(errorMessage), Some(status), Some(err))
							}
							.flatMap(msg => ZIO.fail(SefariaApiError(url, HttpMethod.Post, Some(s"$errorMessage: $msg"), Some(status), None)))
					case res @ HttpService.Response(status, body) =>
						getBodyHandlingError(res, url, HttpMethod.Post, Some(status), baseErrorMessage)
							.unit
				}

		override def addText(ref: SefariaRef, submission: SefariaTextSubmission): IO[Error, Unit] =
			val refString = ref.refString
			val refPath = refToPath(refString)
			val url = textUrl + "/" + refPath + "?skip_links=True&count_after=1"
			
			val baseErrorMessage = s"Failed to add add text to $refString"

			httpService
				.postForm(url, formData(submission))
				.mapError {
					case err: HttpIOError => err.copy(problem = s"$baseErrorMessage: ${err.problem}")
					case err: DecodingError => err.copy(problem = s"$baseErrorMessage: ${err.problem}")
				}
				.flatMap {
					case HttpService.Response(status, body) if status < 200 || status > 201 =>
						body
							.mapError {
								case () => SefariaApiError(url, HttpMethod.Post, Some(s"$baseErrorMessage: failure status"), Some(status), None)
								case err: Throwable => SefariaApiError(url, HttpMethod.Post, Some(s"$baseErrorMessage: failure status"), Some(status), Some(err))
							}
							.flatMap(msg => ZIO.fail(SefariaApiError(url, HttpMethod.Post, Some(s"$baseErrorMessage: $msg"), Some(status), None)))
					case res @ HttpService.Response(status, _) =>
						getBodyHandlingError(
							res, url, HttpMethod
							  .Post, Some(status), baseErrorMessage
						).unit
				}

		override def getIndex: IO[Error, SefariaIndex] = indexCache.cached((), Some(3.minutes)):
			httpService.get(indexUrl)
				.mapError {
					case err: HttpIOError => err.copy(problem = s"Failed to retrieve index: ${err.problem}")
					case err: DecodingError => err.copy(problem = s"Failed to retrieve index: ${err.problem}")
				}
				.flatMap {
					case HttpService.Response(status, body) if status != 200 =>
						body
							.mapError {
								case () => SefariaApiError(indexUrl, HttpMethod.Get, None, Some(status), None)
								case err: Throwable => SefariaApiError(indexUrl, HttpMethod.Get, None, Some(status), Some(err))
							}
							.flatMap(msg => ZIO.fail(SefariaApiError(indexUrl, HttpMethod.Post, Some(msg), Some(status), None)))
					case HttpService.Response(status, body) =>
						body
							.mapError {
								case () => SefariaApiError(indexUrl, HttpMethod.Get, Some("missing body when attempting to retrieve index"), Some(status), None)
								case err: Throwable => SefariaApiError(indexUrl, HttpMethod.Get, None, Some(status), Some(err))
							}
							.flatMap(msg => {
								msg
								  .decodeJsonZIO[SefariaIndex]
							})
				}

		override def getIndexEntry(ref: SefariaRef): IO[Error, SefariaIndexEntry] =
			val refString = ref.refString
			val url = indexUrl + "/" + refString
			httpService.get(url)
				.mapError {
					case err: HttpIOError => err.copy(problem = s"Failed to retrieve index entry for $refString: ${err.problem}")
					case err: DecodingError => err.copy(problem = s"Failed to retrieve index entry for $refString: ${err.problem}")
				}
				.flatMap {
					case HttpService.Response(status, body) if status != 200 =>
						body
							.mapError {
								case () => SefariaApiError(url, HttpMethod.Get, None, Some(status), None)
								case err: Throwable => SefariaApiError(url, HttpMethod.Get, None, Some(status), Some(err))
							}
							.flatMap(msg => ZIO.fail(SefariaApiError(url, HttpMethod.Post, Some(msg), Some(status), None)))
					case HttpService.Response(status, body) =>
						body
							.mapError {
								case () => SefariaApiError(url, HttpMethod.Get, Some(s"missing body when attempting to retrieve index entry for $refString"), Some(status), None)
								case err: Throwable => SefariaApiError(url, HttpMethod.Get, None, Some(status), Some(err))
							}
							.flatMap(msg => {
								msg
								  .decodeJsonZIO[SefariaIndexEntry]
							})
				}

		override def updateCategories(update: SefariaCategoryUpdate): IO[Error, Unit] = indexCache.invalidating(()):
			val baseErrorMessage = s"Failed to update category: ${update.encodeJson}"

			httpService
				.postForm(
					categoryUrl,
					formData(update),
				)
				.flatMap {
					case HttpService.Response(status, body) if status < 200 || status > 201 =>
						body
							.mapError {
								case () => SefariaApiError(categoryUrl, HttpMethod.Post, Some(s"$baseErrorMessage: failure status"), Some(status), None)
								case err: Throwable => SefariaApiError(categoryUrl, HttpMethod.Post, Some(s"$baseErrorMessage: failure status"), Some(status), Some(err))
							}
							.flatMap(msg => ZIO.fail(SefariaApiError(categoryUrl, HttpMethod.Post, Some(s"$baseErrorMessage: $msg"), Some(status), None)))
					case res @ HttpService.Response(status, _) =>
						getBodyHandlingError(
							res, categoryUrl, HttpMethod.Post, Some(status), baseErrorMessage
						).unit
				}

		override def deleteDoc(title: String): IO[Error, Unit] =
			val titlePath = refToPath(title)
			val url = textUrl + "/" + titlePath

			val baseErrorMessage = s"Failed to delete $title"

			httpService
				.delete(url)
				.mapError {
					case err: HttpIOError => err.copy(problem = s"$baseErrorMessage: ${err.problem}")
					case err: DecodingError => err.copy(problem = s"$baseErrorMessage: ${err.problem}")
				}
				.flatMap {
					case HttpService.Response(status, body) if status < 200 || status > 201 =>
						val errorMessage = s"$baseErrorMessage: status was not 200 or 201"
						body
							.mapError {
								case () => SefariaApiError(url, HttpMethod.Post, Some(errorMessage), Some(status), None)
								case err: Throwable => SefariaApiError(url, HttpMethod.Post, Some(errorMessage), Some(status), Some(err))
							}
							.flatMap(msg => ZIO.fail(SefariaApiError(url, HttpMethod.Post, Some(s"$errorMessage: msg"), Some(status), None)))
					case HttpService.Response(status, body) =>
						body.tap(body => Console.printLine(s"$status: $body ($url)")).catchAll(_ => ZIO.unit)
						  *> ZIO.unit
				}