package contextus.model.xml

import contextus.model.contextus.Title

trait Template:
    def doc(title: Title, versionTitle: Option[String]): String

object Template:
    case object Song extends Template:
        def doc(title: Title, versionTitle: Option[String]): String = 
            s"""|<document>
               |    <title>$title</title>
               |
               |    <author>Author goes here</author>
               |
               |    <category>Parent Category / Child Category</category>
               |
               |    <schema paragraphBreak="line-break">
               |        <level>Verse</level>
               |        <level>Line</level>
               |    </schema>
               |    
               |    <version>
               |        <title>${versionTitle.getOrElse("Title of text version goes here")}</title>
               |        <source>https://source.url.goes.here</source>
               |    </version>
               |    
               |    <body>
               |        <section level="Verse">
               |            Text goes here
               |            lines should be separated by single carriage return
               |        </section>
               |        <section level="Verse">
               |            Text goes here
               |            lines should be separated by single carriage return
               |        </section>
               |    </body>
               |
               |</document>""".stripMargin


    case object SimpleParagraphs extends Template:
        def doc(title: Title, versionTitle: Option[String]): String = 
            s"""|<document>
               |    <title>${title}</title>
               |
               |    <author>Author goes here</author>
               |
               |    <category>Parent Category / Child Category</category>
               |
               |    <schema paragraphBreak="multi-line-break">
               |        <level>Paragraph</level>
               |    </schema>
               |    
               |    <version>
               |        <title>${versionTitle.getOrElse("Title of text version goes here")}</title>
               |        <source>https://source.url.goes.here</source>
               |    </version>
               |    
               |    <body>
               |        Text goes here
               |
               |        paragraphs should be separated by blank lines
               |    </body>
               |</document>""".stripMargin

    case object SimpleChapters extends Template:
        def doc(title: Title, versionTitle: Option[String]): String = 
            s"""|<document>
               |    <title>${title}</title>
               |
               |    <author>Author goes here</author>
               |
               |    <category>Parent Category / Child Category</category>
               |
               |    <schema paragraphBreak="multi-line-break">
               |        <level>Chapter</level>
               |        <level>Paragraph</level>
               |    </schema>
               |    
               |    <version>
               |        <title>${versionTitle.getOrElse("Title of text version goes here")}</title>
               |        <source>https://source.url.goes.here</source>
               |    </version>
               |    
               |    <body>
               |        <section level="Chapter">
               |            Text goes here
               |
               |            paragraphs should be separated by blank lines
               |        </section>
               |        <section level="Chapter">
               |            Text goes here
               |
               |            paragraphs should be separated by blank lines
               |        </section>
               |    </body>
               |
               |</document>""".stripMargin

    case object Poem extends Template:
        def doc(title: Title, versionTitle: Option[String]): String = 
            s"""|<document>
               |    <title>${title}</title>
               |
               |    <author>Author goes here</author>
               |
               |    <category>Parent Category / Child Category</category>
               |
               |    <schema paragraphBreak="line-break">
               |        <level>Stanza</level>
               |        <level>Line</level>
               |    </schema>
               |    
               |    <version>
               |        <title>${versionTitle.getOrElse("Title of text version goes here")}</title>
               |        <source>https://source.url.goes.here</source>
               |    </version>
               |    
               |    <body>
               |        <section level="Stanza">
               |            Text goes here
               |            paragraphs should be separated by single carriage return
               |        </section>
               |        <section level="Chapter">
               |            Text goes here
               |            paragraphs should be separated by single carriage return
               |        </section>
               |    </body>
               |
               |</document>""".stripMargin

    case object NamedChapters extends Template:
        def doc(title: Title, versionTitle: Option[String]): String = 
            s"""|<document>
                |    <title>${title}</title>
                |
                |    <author>Author goes here</author>
                |
                |    <category>Parent Category / Child Category</category>
                |
                |    <schema paragraphBreak="multi-line-break">
                |        <level>Paragraph</level>
                |    </schema>
                |    
                |    <version>
                |        <title>${versionTitle.getOrElse("Title of text version goes here")}</title>
                |        <source>https://source.url.goes.here</source>
                |    </version>
                |    
                |    <body>
                |        <section>
                |            <title>Preface</title>
                |            Text goes here
                |
                |            paragraphs should be separated by blank lines
                |        </section>
                |            <title>Chapter 1</title>
                |            Text goes here
                |
                |            paragraphs should be separated by blank lines
                |        <section>
                |            <title>Chapter 2</title>
                |            Text goes here
                |
                |            paragraphs should be separated by blank lines
                |        </section>
                |        <section>
                |            <title>Conclusion</title>
                |            Text goes here
                |
                |            paragraphs should be separated by blank lines
                |        </section>
                |    </body>
                |</document>""".stripMargin

    case object MultipleBooks extends Template:
        def doc(title: Title, versionTitle: Option[String]): String = 
            s"""|<document>
                |    <title>${title}</title>
                |
                |    <author>Author goes here</author>
                |
                |    <category>Parent Category / Child Category</category>
                |
                |    <schema paragraphBreak="multi-line-break">
                |        <level>Paragraph</level>
                |    </schema>
                |    
                |    <version>
                |        <title>${versionTitle.getOrElse("Title of text version goes here")}</title>
                |        <source>https://source.url.goes.here</source>
                |    </version>
                |    
                |    <body>
                |        <section>
                |            <title>Preface</title>
                |            Text goes here
                |
                |            paragraphs should be separated by blank lines
                |        </section>
                |        <section>
                |            <title>Book 1</title>
                |            <section>
                |                <title>Chapter 1</title>
                |                Text goes here
                |
                |                paragraphs should be separated by blank lines
                |            </section>
                |            <section>
                |                <title>Chapter 2</title>
                |                Text goes here
                |
                |                paragraphs should be separated by blank lines
                |            </section>
                |        </section>
                |        <section>
                |            <title>Book 2</title>
                |            <section>
                |                <title>Chapter 1</title>
                |                Text goes here
                |
                |                paragraphs should be separated by blank lines
                |            <section>
                |                <title>Chapter 2</title>
                |                Text goes here
                |
                |                paragraphs should be separated by blank lines
                |            </section>
                |        </section>
                |        <section>
                |            <title>Conclusion</title>
                |            Text goes here
                |
                |            paragraphs should be separated by blank lines
                |        </section>
                |    </body>
                |</document>""".stripMargin