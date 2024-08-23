package contextus.cli

import contextus.service.ConfigurationService
import zio.*
import zio.nio.file.*
import zio.cli.*
import contextus.model.DomainError
import contextus.json.*
import contextus.service.UpdateService.Arch

object ConfigCommands:
	private val setTextOption = Options.text("set").alias("s").optional ?? "Set configuration value"

	private val apiKeyCommand = Command(
		"api-key",
		setTextOption,
	).withHelp("Display or update current API key")
		.map {
			case None => for {
				configService <- ZIO.service[ConfigurationService]
				apiKey <- configService.getApiKey
				_ <- apiKey match
					case None => Console.printLine("No value! (to set, run: contextus config api-key --set <value>)").orDie
					case Some(key) => Console.printLine(key).orDie
			} yield ()
			case Some(newValue) => for {
				configService <- ZIO.service[ConfigurationService]
				_ <- configService.setApiKey(newValue)
			} yield ()
		}
	 	.map(_.catchAll {
			(_: DomainError.IOError) =>
				Console.printLine("Encountered unexpected error opening the configuration file. Contact your maintainer.")
		})
	
	private val archCommand = Command(
		"arch",
		setTextOption.mapOrFail {
				case None => Right(None)
				case Some(str) => Arch.parse(str)
					.map(Some.apply)
					.left
					.map(msg => ValidationError(ValidationErrorType.InvalidValue, HelpDoc.p(msg)))
		},
	).withHelp("Display or update system architecture. Needed to run update command.")
		.map {
			case None => for {
				configService <- ZIO.service[ConfigurationService]
				archOpt <- configService.getArch
				_ <-  archOpt match
					case None => Console.printLine("No value! (to set, run: contextus config arch --set <value>)").orDie
					case Some(arch) => Console.printLine(arch.text).orDie
			} yield ()
			case Some(newValue) => for {
				configService <- ZIO.service[ConfigurationService]
				_ <- configService.setArch(newValue)
			} yield ()
		}
	 	.map(_.catchAll {
			(_: DomainError.IOError) =>
				Console.printLine("Encountered unexpected error opening the configuration file. Contact your maintainer.")
		})

	val rootCommand = Command("config").subcommands(apiKeyCommand, archCommand)
