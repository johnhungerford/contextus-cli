package contextus.cli

import contextus.model.DomainError
import zio.*
import zio.cli.*
import zio.nio.file.*

import java.io.IOException
import java.nio.charset.StandardCharsets

object TextCommands:
	private val repairSplitLinesOpt =
		Options.boolean("unhyphen").??("Join words that have been split with hyphens across multiple lines")

	private val repairSplitLinesRegex =
		"""([A-Za-zÀ-ÖØ-öø-ÿ]+)-\s*\n([A-Za-zÀ-ÖØ-öø-ÿ]+)"""

	private lazy val regexChars = ".*+|{}[]()\\/-?^".toSet

	private val removePageNumbersOpt = Options
		.text("remove-page-numbers")
		.??("Provide an example page number string")
		.map(pattern => {
			val stringBuilder = StringBuilder()
			var parsingNumber = false
			pattern foreach { c =>
				if c.isDigit then parsingNumber = true
				else
					if parsingNumber then
						parsingNumber = false
						stringBuilder ++= "\\d+"

					if regexChars.contains(c) then
						stringBuilder ++= s"""\\$c"""
					else stringBuilder += c
			}

			if parsingNumber then
				stringBuilder ++= "\\d+"

			stringBuilder.toString()
		})
		.optional

	val fixTextCommand = Command(
		"fix-text",
		repairSplitLinesOpt ++ removePageNumbersOpt,
		Args.file("text-file") ++ Args.file("output-file").atMost(1).map(_.headOption),
	).map { case ((repairSplitLines, removePageNumbers), (textFile, outFileOpt)) =>
		val textPath = Path.fromJava(textFile)
		val outPath = outFileOpt.fold(textPath)(Path.fromJava)
		val effect = for
			text <- Files.readAllBytes(textPath)
				.map(v => String(v.toArray, StandardCharsets.UTF_8))
			withSplitLinesRepaired =
				if repairSplitLines then text.replaceAll(repairSplitLinesRegex, """$1$2""")
				else text
			withPageNumbersRemoved = removePageNumbers
				.fold(withSplitLinesRepaired)(pnRegex => withSplitLinesRepaired.replaceAll(pnRegex, ""))
			_ <- ZIO.foreach(removePageNumbers)(v => ZIO.debug(f"Removing page numbers using the following regex: $v"))
			_ <- Files
				.writeBytes(outPath, Chunk.fromArray(withPageNumbersRemoved.getBytes(StandardCharsets.UTF_8)))
		yield ()

		effect.mapError { (err: IOException) =>
			DomainError
				.IOError
				.FileIOError(
					textPath.toString,
					s"Unable to read text file for repair",
					Some(err),
				)
		}
	}