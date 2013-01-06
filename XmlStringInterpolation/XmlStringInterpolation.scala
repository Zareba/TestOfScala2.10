import scala.language.implicitConversions

class XmlStringInterpolation(str: StringContext) {
    
    object xml {
        import scala.xml._
        
        def apply(args: Any*): Seq[Node] = {
            str.checkLengths(args)
            
            if (args.foldLeft(true)((acc, el) => acc && (el.isInstanceOf[String] || isInstanceOfSeqNodes(el)))) {
                import scala.util.parsing.combinator._
                
                val sip = new StringInterpolationParsers
                val result = sip.parseAll(
                    sip.tag,
                    str.parts.toArray,
                    args.foldLeft(Array[Seq[Node]]())((acc, el) => acc :+ {el match {
                        case str: String => Text(str)
                        case sn => sn.asInstanceOf[Seq[Node]]
                    }})
                )
                
                if (result.successful)
                    result.get
                else {
                    println(result.toString)
                    Seq(Text(""))
                }
            } else
                Seq(Text("Wrong types in expr's"))
        }
        
        //def unapplySeq(xml: Node): Option[Seq[Node]] = None
        
        def isInstanceOfSeqNodes(sn: Any): Boolean = {
            if (sn.isInstanceOf[Seq[_]])
                sn.asInstanceOf[Seq[_]].foldLeft(true)(
                    (acc, el) =>
                        acc && (
                            if (el.isInstanceOf[Node])
                                true
                            else
                                isInstanceOfSeqNodes(el)
                        )
                )
            else
                false
        }
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


    val xml = xml"""
        html {
            head {
                title {"Search Links - Build with Xml Builder in Scala"}
                style {${pageStyle}}
            }
            body {
                p (class="title" style="font-weight: 900;") {"Search Links:"}
                br
                ul (id="ulSearchList")
                {${
                    for ((name, link) <- searchLinks) yield {
                        xml"""li {
                            a (href=${link}) {${name}}
                        }"""
                    }
                }}
            }
        }"""
    
    
    
    
    if (xml.length == 1) {
        val pp = new scala.xml.PrettyPrinter(80, 2)
        println(pp.format(xml(0)))
    }

    
}