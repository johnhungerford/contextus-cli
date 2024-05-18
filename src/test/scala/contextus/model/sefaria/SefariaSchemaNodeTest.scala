package contextus.model.sefaria

import zio.*
import zio.test.*

import contextus.json.*

object SefariaSchemaNodeTest extends ZIOSpecDefault:
	override def spec = suite("SefariaSchemaNode")(
		suite("serialization/deserialization")(
			test("should serialize an empty section schema node") {
				val node = SefariaSchemaNode.Section(
					nodes = Nil,
					titles = List(
						SefariaTitle(
							text = "\u05e9",
							lang = "he",
							primary = Some(true),
						),
						SefariaTitle(
							text = "The Spirit of the Laws",
							lang = "en",
							primary = Some(true),
						),
					),
					title = "The Spirit of the Laws",
					heTitle = "\u05e9",
					key = "The Spirit of the Laws",
				)
				val json = node.encodeJson
				assertTrue(
					json == """{"nodes":[],"titles":[{"text":"ש","lang":"he","primary":true},{"text":"The Spirit of the Laws","lang":"en","primary":true}],"title":"The Spirit of the Laws","heTitle":"ש","key":"The Spirit of the Laws"}""",
				)
			},
			test("should deserialize a section schema node") {
				val json = """{
							 |    "nodes": [],
							 |    "titles": [
							 |        {
							 |            "text": "\u05e9",
							 |            "lang": "he",
							 |            "primary": true
							 |        },
							 |        {
							 |            "text": "The Spirit of the Laws",
							 |            "lang": "en",
							 |            "primary": true
							 |        }
							 |    ],
							 |    "title": "The Spirit of the Laws",
							 |    "heTitle": "\u05e9",
							 |    "key": "The Spirit of the Laws"
							 |}""".stripMargin
				for {
					node <- json.decodeJson[SefariaSchemaNode]
				} yield assertTrue(
					node == SefariaSchemaNode.Section(
						nodes = Nil,
						titles = List(
							SefariaTitle(
								text = "\u05e9",
								lang = "he",
								primary = Some(true),
							),
							SefariaTitle(
								text = "The Spirit of the Laws",
								lang = "en",
								primary = Some(true),
							),
						),
						title = "The Spirit of the Laws",
						heTitle = "\u05e9",
						key = "The Spirit of the Laws",
					)
				)
			},
			test("should serialize a text schema node") {
				val node = SefariaSchemaNode.Text(
					depth = 2,
					addressTypes = List(SefariaAddressType.Integer, SefariaAddressType.Integer),
					sectionNames = List("Chapter", "Paragraph"),
					titles = List(
						SefariaTitle(
							text = "\u05e1\u05e4\u05e8 0",
							lang = "he",
							primary = Some(true),
						),
						SefariaTitle(
							text = "Book X Of Laws in the Relation They Bear to Offensive Force",
							lang = "en",
							primary = Some(true),
						),
					),
					heSectionNames = List(
						"\u05d0Chapter",
						"\u05d0Paragraph",
					),
					title = "Book X Of Laws in the Relation They Bear to Offensive Force",
					heTitle = "\u05e1\u05e4\u05e8 0",
					key = "Book X; Of Laws in the Relation They Bear to Offensive Force",
				)
				val json = node.encodeJson
				assertTrue(
					json == """{"nodeType":"JaggedArrayNode","depth":2,"addressTypes":["Integer","Integer"],"sectionNames":["Chapter","Paragraph"],"titles":[{"text":"ספר 0","lang":"he","primary":true},{"text":"Book X Of Laws in the Relation They Bear to Offensive Force","lang":"en","primary":true}],"heSectionNames":["אChapter","אParagraph"],"title":"Book X Of Laws in the Relation They Bear to Offensive Force","heTitle":"ספר 0","key":"Book X; Of Laws in the Relation They Bear to Offensive Force"}""",
				)
			},
			test("should deserialize a text schema node") {
				val textNodeString =
					"""{
					  |    "nodeType": "JaggedArrayNode",
					  |    "depth": 2,
					  |    "addressTypes": [
					  |        "Integer",
					  |        "Integer"
					  |    ],
					  |    "sectionNames": [
					  |        "Chapter",
					  |        "Paragraph"
					  |    ],
					  |    "titles": [
					  |        {
					  |            "text": "\u05e1\u05e4\u05e8 0",
					  |            "lang": "he",
					  |            "primary": true
					  |        },
					  |        {
					  |            "text": "Book X Of Laws in the Relation They Bear to Offensive Force",
					  |            "lang": "en",
					  |            "primary": true
					  |        }
					  |    ],
					  |    "title": "Book X Of Laws in the Relation They Bear to Offensive Force",
					  |    "heTitle": "\u05e1\u05e4\u05e8 0",
					  |    "heSectionNames": [
					  |        "\u05d0Chapter",
					  |        "\u05d0Paragraph"
					  |    ],
					  |    "key": "Book X; Of Laws in the Relation They Bear to Offensive Force"
					  |}""".stripMargin
				for {
					schemaNode <- textNodeString.decodeJsonZIO[SefariaSchemaNode]
				} yield assertTrue(
					schemaNode match
						case text: SefariaSchemaNode.Text=>
							text.depth == 2
								&& text.addressTypes == List(SefariaAddressType.Integer, SefariaAddressType.Integer)
								&& text.sectionNames == List("Chapter", "Paragraph")
								&& text.titles.size == 2
								&& text.title == "Book X Of Laws in the Relation They Bear to Offensive Force"
								&& text.heTitle == "\u05e1\u05e4\u05e8 0"
								&& text.heSectionNames == List("\u05d0Chapter", "\u05d0Paragraph")
						case _ => false
				)
			},
			test("should deserialize a real section schema node with nested text nodes") {
				val schemaNodeString =
					"""{
					  |    "nodes": [
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book I Of Laws in General",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book I Of Laws in General",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book I; Of Laws in General"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book II Of Laws Directly Derived from the Nature of Government",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book II Of Laws Directly Derived from the Nature of Government",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book II; Of Laws Directly Derived from the Nature of Government"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book III Of the Principles of the Three Kinds of Government",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book III Of the Principles of the Three Kinds of Government",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book III; Of the Principles of the Three Kinds of Government"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book IV That the Laws of Education Ought to Be in Relation to the Principles of Government",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book IV That the Laws of Education Ought to Be in Relation to the Principles of Government",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book IV; That the Laws of Education Ought to Be in Relation to the Principles of Government"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book V That the Laws Given by the Legislator Ought to Be in Relation to the Principle of Government",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book V That the Laws Given by the Legislator Ought to Be in Relation to the Principle of Government",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book V; That the Laws Given by the Legislator Ought to Be in Relation to the Principle of Government"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book VI Consequences of the Principles of Different Governments with Respect to the Simplicity of Civil and Criminal Laws, the Form of Judgments, and the Inflicting of Punishments",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book VI Consequences of the Principles of Different Governments with Respect to the Simplicity of Civil and Criminal Laws, the Form of Judgments, and the Inflicting of Punishments",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book VI; Consequences of the Principles of Different Governments with Respect to the Simplicity of Civil and Criminal Laws, the Form of Judgments, and the Inflicting of Punishments"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book VII Consequences of the Different Principles of the Three Governments with Respect to Sumptuary Laws, Luxury, and the Condition of Women",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book VII Consequences of the Different Principles of the Three Governments with Respect to Sumptuary Laws, Luxury, and the Condition of Women",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book VII; Consequences of the Different Principles of the Three Governments with Respect to Sumptuary Laws, Luxury, and the Condition of Women"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book VIII Of the Corruption of the Principles of the Three Governments",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book VIII Of the Corruption of the Principles of the Three Governments",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book VIII; Of the Corruption of the Principles of the Three Governments"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book IX Of Laws in the Relation They Bear to a Defensive Force",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book IX Of Laws in the Relation They Bear to a Defensive Force",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book IX; Of Laws in the Relation They Bear to a Defensive Force"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book X Of Laws in the Relation They Bear to Offensive Force",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book X Of Laws in the Relation They Bear to Offensive Force",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book X; Of Laws in the Relation They Bear to Offensive Force"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XI Of the Laws Which Establish Political Liberty, with Regard to the Constitution",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XI Of the Laws Which Establish Political Liberty, with Regard to the Constitution",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XI; Of the Laws Which Establish Political Liberty, with Regard to the Constitution"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XII Of the Laws That Form Political Liberty, in Relation to the Subject",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XII Of the Laws That Form Political Liberty, in Relation to the Subject",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XII; Of the Laws That Form Political Liberty, in Relation to the Subject"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XIII Of the Relation Which the Levying of Taxes and the Greatness of the Public Revenues Bear to Liberty",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XIII Of the Relation Which the Levying of Taxes and the Greatness of the Public Revenues Bear to Liberty",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XIII; Of the Relation Which the Levying of Taxes and the Greatness of the Public Revenues Bear to Liberty"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XIV Of Laws in Relation to the Nature of the Climate",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XIV Of Laws in Relation to the Nature of the Climate",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XIV; Of Laws in Relation to the Nature of the Climate"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XV In What Manner the Laws of Civil Slavery Relate to the Nature of the Climate",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XV In What Manner the Laws of Civil Slavery Relate to the Nature of the Climate",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XV; In What Manner the Laws of Civil Slavery Relate to the Nature of the Climate"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XVI How the Laws of Domestic Slavery Bear a Relation to the Nature of the Climate",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XVI How the Laws of Domestic Slavery Bear a Relation to the Nature of the Climate",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XVI; How the Laws of Domestic Slavery Bear a Relation to the Nature of the Climate"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XVII How the Laws of Political Servitude Bear a Relation to the Nature of the Climate",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XVII How the Laws of Political Servitude Bear a Relation to the Nature of the Climate",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XVII; How the Laws of Political Servitude Bear a Relation to the Nature of the Climate"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XVIII Of Laws in the Relation They Bear to the Nature of the Soil",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XVIII Of Laws in the Relation They Bear to the Nature of the Soil",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XVIII; Of Laws in the Relation They Bear to the Nature of the Soil"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XIX Of Laws in Relation to the Principles Which Form the General Spirit, Morals, and Customs of a Nation",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XIX Of Laws in Relation to the Principles Which Form the General Spirit, Morals, and Customs of a Nation",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XIX; Of Laws in Relation to the Principles Which Form the General Spirit, Morals, and Customs of a Nation"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XX Of Laws in Relation to Commerce, Considered in its Nature and Distinctions",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XX Of Laws in Relation to Commerce, Considered in its Nature and Distinctions",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XX; Of Laws in Relation to Commerce, Considered in its Nature and Distinctions"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XXI Of Laws in relation to Commerce, considered in the Revolutions it has met with in the World",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XXI Of Laws in relation to Commerce, considered in the Revolutions it has met with in the World",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XXI; Of Laws in relation to Commerce, considered in the Revolutions it has met with in the World"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XXII Of Laws in Relation to the Use of Money",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XXII Of Laws in Relation to the Use of Money",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XXII; Of Laws in Relation to the Use of Money"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XXIII Of Laws in the Relation They Bear to the Number of Inhabitants",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XXIII Of Laws in the Relation They Bear to the Number of Inhabitants",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XXIII; Of Laws in the Relation They Bear to the Number of Inhabitants"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XXIV Of Laws in relation to Religion Considered in Itself, and in its Doctrines",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XXIV Of Laws in relation to Religion Considered in Itself, and in its Doctrines",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XXIV; Of Laws in relation to Religion Considered in Itself, and in its Doctrines"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XXV Of Laws in Relation to the Establishment of Religion and its External Polity",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XXV Of Laws in Relation to the Establishment of Religion and its External Polity",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XXV; Of Laws in Relation to the Establishment of Religion and its External Polity"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XXVI Of Laws in Relation to the Order of Things Which They Determine",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XXVI Of Laws in Relation to the Order of Things Which They Determine",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XXVI; Of Laws in Relation to the Order of Things Which They Determine"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XXVII",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XXVII",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XXVII;"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XXVIII Of the Origin and Revolutions of the Civil Laws among the French",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XXVIII Of the Origin and Revolutions of the Civil Laws among the French",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XXVIII; Of the Origin and Revolutions of the Civil Laws among the French"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XXIX Of the Manner of Composing Laws",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XXIX Of the Manner of Composing Laws",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XXIX; Of the Manner of Composing Laws"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XXX Theory of the Feudal Laws among the Franks in the Relation They Bear to the Establishment of the Monarchy",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XXX Theory of the Feudal Laws among the Franks in the Relation They Bear to the Establishment of the Monarchy",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XXX; Theory of the Feudal Laws among the Franks in the Relation They Bear to the Establishment of the Monarchy"
					  |        },
					  |        {
					  |            "nodeType": "JaggedArrayNode",
					  |            "depth": 2,
					  |            "addressTypes": [
					  |                "Integer",
					  |                "Integer"
					  |            ],
					  |            "sectionNames": [
					  |                "Chapter",
					  |                "Paragraph"
					  |            ],
					  |            "titles": [
					  |                {
					  |                    "text": "\u05e1\u05e4\u05e8 0",
					  |                    "lang": "he",
					  |                    "primary": true
					  |                },
					  |                {
					  |                    "text": "Book XXXI Theory of the Feudal Laws among the Franks, in the Relation They Bear to the Revolutions of their Monarchy",
					  |                    "lang": "en",
					  |                    "primary": true
					  |                }
					  |            ],
					  |            "title": "Book XXXI Theory of the Feudal Laws among the Franks, in the Relation They Bear to the Revolutions of their Monarchy",
					  |            "heTitle": "\u05e1\u05e4\u05e8 0",
					  |            "heSectionNames": [
					  |                "\u05d0Chapter",
					  |                "\u05d0Paragraph"
					  |            ],
					  |            "key": "Book XXXI; Theory of the Feudal Laws among the Franks, in the Relation They Bear to the Revolutions of their Monarchy"
					  |        }
					  |    ],
					  |    "titles": [
					  |        {
					  |            "text": "\u05e9",
					  |            "lang": "he",
					  |            "primary": true
					  |        },
					  |        {
					  |            "text": "The Spirit of the Laws",
					  |            "lang": "en",
					  |            "primary": true
					  |        }
					  |    ],
					  |    "title": "The Spirit of the Laws",
					  |    "heTitle": "\u05e9",
					  |    "key": "The Spirit of the Laws"
					  |}""".stripMargin

				for {
					schemaNode <- schemaNodeString.decodeJsonZIO[SefariaSchemaNode]
				} yield assertTrue(
					schemaNode match
						case sect: SefariaSchemaNode.Section=>
							sect.nodes.size == 31
						case _ => false
				)
			},
		),
	)