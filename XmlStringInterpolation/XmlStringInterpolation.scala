import scala.language.implicitConversions

class XmlStringInterpolation(str: StringContext) {
    object xml {
        def apply(exprs: Any*) = {
            val isString = """\s*([^"]*)\s*("[^"]*")""".r
            val firstPS = """\s*([^(]*)\s*(\([^\)]*\))""".r
            val firstBS = """\s*([^\{]*)\s*(\{[^\}]*\})""".r
            str.parts.map(part => {
                val sss = firstBS.findFirstIn(part)
                if (!sss.isEmpty) {
                    println("------------------------------------------------------")
                    println(sss)
                    println("-----------------------------")
                }
                val aPos = part.indexOf('(')
                val cPos = part.indexOf('{')
                if (aPos > cPos) {
                    // Er dette en Tuple2
                    // Er dette en List[Tuple2]
                    // Er dette en String
                        // Find stykket inden næste string, er der en ) i dette stykke ?
                            // Ja, => Do your thing
                        // Ellers
                            // Nej, Do it again
                    // Syntax - Fejl
                }
                // Er dette en XML object (epxr)
                // Er dette en String
                    // Find stykket inden næste string, er det tilhørende } i dette stykke ?
                        // Ja, => Do your thing
                    // Ellers
                        // Nej, Do it again
                // Syntax - Fejl
            })
        }
        
        
        //def unapplySeq(xml: Node): Option[Seq[Node]] = None
    }
}

object test extends App {
    implicit final def XmlStringInterpolation(str: StringContext): XmlStringInterpolation = new XmlStringInterpolation(str)
    
    val searchLinks = List(
        "Google" -> "http://www.google.com",
        "Bing" -> "http://www.bing.com",
        "Ask.com" -> "http://www.ask.com",
        "Yahoo!" -> "http://www.yahoo.com",
        "Alta Vista" -> "http://www.altavista.com",
        "Excite" -> "http://www.excite.com",
        "HotBot" -> "http://www.hotbot.com")

    val pageStyle = """
#searchList {
    padding: 15px 25px;
    border: solid 2px black;   
    width: 300px;
}

#searchList li {
    margin: 3px;
    list-style: none;
}

p.title {
    margin: 0px;
    padding: 30px;
    font-size: 3em;
}
"""

    println(
xml"""html {
    head {
        title {"Search Links - Build with Xml Builder in Scala"}
        style {$pageStyle}
    }
    body {
        p (class="title" style="font-weight: 900;") {"Search Links:"}
        br
        ul (id="ulSearchList") {${
            for ((name, link) <- searchLinks) yield {
                xml"""li {
                    a (href="$link") {$name}
                }"""
            }
        }}
    }
}""")
    
//    println(xml"""
//        html {
//            head {
//                title
//                "Search Links - Build with Xml Builder in Scala"
//                style
//                $pageStyle
//            }
//            body {
//                p
//                List("class" -> "title", "style" -> "font-weight: 900;")
//                "Search Links:"
//                br
//                ""
//                ul
//                "id" -> "ulSearchList"
//                {
//                    for ((name, link) <- searchLinks) yield {
//                        li {
//                            a
//                            "href" -> link;
//                            name
//                        }
//                    }
//                }.foldLeft("")(_ + _)
//            }
//        }
//    })
    
}