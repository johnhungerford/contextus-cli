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
		"arch"
	).withHelp("Display or update system architecture. Needed to run update command.")
		.map { _ =>
			for {
				configService <- ZIO.service[ConfigurationService]
				arch <- configService.getArch
				_ <- Console.printLine(arch.text).orDie
			} yield ()
		}
	 	.map(_.catchAll {
			(e: DomainError.IOError) =>
				Console.printLine(s"${e}\n\nEncountered unexpected error reading architecture configuration. Contact your maintainer.")
		})

	val rootCommand = Command("config").withHelp("Commands for setting configuration").subcommands(apiKeyCommand, archCommand)
