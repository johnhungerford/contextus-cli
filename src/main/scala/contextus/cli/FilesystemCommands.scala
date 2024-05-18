package contextus.cli

import zio.*
import zio.nio.file.*
import zio.cli.*

object FilesystemCommands:
	private val dirArg = Args.directory(Exists.Yes).atMost(1).map(_.headOption)
	  ?? "Directory to list the contents of (current directory by default)"

	val lsCommand = Command(
		"list",
		dirArg,
	).map { dirOpt =>
		val dir = dirOpt.fold(Path(""))(p => Path.fromJava(p))
		val contents = Files.list(dir)
		contents.foreach(p => Console.printLine(p.filename).orDie).orDie
	}
