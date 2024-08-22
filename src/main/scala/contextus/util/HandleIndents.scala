package contextus.util

import contextus.model.xml.ParseIndent

/**
 * Removes indents when parsing text sections
 */
object HandleIndents {
	def apply(text: String, indent: ParseIndent = ParseIndent.Auto): String = indent match
		case ParseIndent.IgnoreIndent =>
			text
		case ParseIndent.Auto =>
			val splitText = text.split("\n").toList
			var currentSmallestIndent: Option[String] = None
			splitText
				.foreach { line =>
					if !line.isBlank then currentSmallestIndent match
						case None =>
							currentSmallestIndent = Some(line.takeWhile(_.isWhitespace))
						case Some(currentIndent) =>
							currentSmallestIndent = Some(
								currentIndent
									.zip(line)
									.takeWhile(tup => tup._1 == tup._2)
									.map(_._1)
									.mkString("")
							)
				}
			val smallestIndent = currentSmallestIndent.getOrElse("")
			splitText
			  .map(_.stripPrefix(smallestIndent))
			  .mkString("\n")

}
