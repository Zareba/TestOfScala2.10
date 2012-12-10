import scala.language.implicitConversions

class XmlStringInterpolation(str: StringContext) {
    import StringContext._       // treatEscapes
    import scala.xml._
    
    object xml {
        
        def apply(args: Any*): Seq[Node] = {
            str.checkLengths(args)
            
            val findTagName = """\s*\w+\s*""".r
            
            
            
            
            def getXml(_parts: Array[String], _args: Seq[Any]): Seq[Node] = {
                var parts = _parts
                var args = _args
                
                var content  = Seq[Node]()
                if (parts(0).trim == "" && args.length > 0) {
                    if (args(0).isInstanceOf[String])
                        parts(0) = args(0).asInstanceOf[String]
                    else if (args(0).isInstanceOf[List[String]])
                        parts(0) = args(0).asInstanceOf[List[String]].foldLeft("")(_ + _)
                    else if (args(0).isInstanceOf[Seq[Node]]) {
                        content = args(0).asInstanceOf[Seq[Node]].foldLeft(Seq[Node]())(_ :+ _)
                    } else
                        println("ARGS TYPE ERROR")
                    
                    args = args drop 1
                }
                
                
                if (content.length == 0) {
                    
                    var part = parts(0)
                    
                    //println("Start Start Start Start Start")
                    //println(parts.toList)
                    
                    //println("\nTagTagTagTagTagTagTagTagTagTagTagTagTagTagTag")
                    val tag = findTagName findFirstIn part getOrElse ""
                    val tagName = tag.trim
                    //if (tag == "") println("ERROR No tag") //           ERROR No tag
                    //println("Tag: <" + tag.trim + ">")
                    
                    part = (part drop tag.length).trim
                    
                    var attrsStr = ""
                    if (part.indexOf("(") == 0) {
                        //println("AttrAttrAttrAttrAttrAttrAttrAttrAttrAttrAttrAttrAttrAttrAttr")
                        var idx = 0
                        var cur = 0
                        val len = part.length
                        var backslash = false
                        var inString = false
                        var inTripString = false
                        var finished = false
                        while (!finished) {
                            part(cur) match {
                                case '(' if (!inString && !inTripString) => idx += 1
                                case ')' if (!inString && !inTripString) => idx -= 1
                                case '"' => 
                                    if (part.drop(cur).take(3) == "\"\"\"" && !inString) {
                                        if (inTripString && !backslash) 
                                            inTripString = false
                                        else if (!inTripString) 
                                            inTripString = true
                                    } else if (!inTripString) {
                                        if (inString && !backslash) 
                                            inString = false
                                        else if (!inString) 
                                            inString = true
                                    }
                                case '\\' if ((inString || inTripString) && !backslash) => backslash = true
                                case _ => ()
                            }
                            if ((inString || inTripString) && part(cur) != '\\') backslash = false
                            
                            if (idx == 0) {
                                attrsStr = (part drop 1 take cur - 1).trim
                                finished = true
                            } else
                                cur += 1
                        }
                        
                        part = part.drop(cur + 1).trim
                        parts(0) = part
                        //println("Attrs: " + attrsStr)
                    }
                    
                    var cur = 0
                    var curP = 0
                    var len = part.length
                    var lenP = parts.length
                    if (part.indexOf("{") == 0) {
                        //println("ContentContentContentContentContentContentContent")
                        part = part drop 1
                        parts(curP) = part
                        
                        len -= 1
                        var idx = 1
                        var backslash = false
                        var inString = false
                        var inTripString = false
                        var finished = false
                        var inAttr = false
                        var attrIdx = 0
                        while (!finished) {
                            if ((inString || inTripString) && part(cur - 1) != '\\') backslash = false
                            part(cur) match {
                                case '{' if (!inString && !inTripString) => {
                                    idx += 1
                                    cur += 1
                                }
                                case '}' if (!inString && !inTripString) => {
                                    idx -= 1
                                    cur += 1
                                }
                                case '(' if (!inString && !inTripString) => {
                                    attrIdx += 1
                                    cur += 1
                                }
                                case ')' if (!inString && !inTripString) => {
                                    attrIdx -= 1
                                    cur += 1
                                }
                                case '"' => {
                                    if (part.drop(cur).take(3) == "\"\"\"") 
                                        if (!inString) {
                                            if (inTripString && !backslash) {
                                                inTripString = false
                                                if (idx == 1 && attrIdx == 0) {
                                                    //println("after inTripString")
                                                    content = content :+ Text(part.take(cur).drop(3))
                                                    part = part.drop(cur + 3)
                                                    len = part.length
                                                    cur = 0
                                                } else
                                                    cur += 1
                                            } else if (!inTripString) {
                                                inTripString = true
                                                if (idx == 1 && cur > 0 && attrIdx == 0) {
                                                    //println("before inTripString")
                                                    content = content ++ getXml({
                                                        parts(curP) = part.take(cur)
                                                        parts take(curP + 1)
                                                    } , args)
                                                    
                                                    part = part drop cur
                                                    parts = parts drop curP
                                                    parts(0) = part
                                                    
                                                    cur = 3
                                                    curP = 0
                                                    len = part.length
                                                    lenP = parts.length
                                                } else
                                                    cur += 1
                                            } else
                                                cur += 1
                                        } else {
                                            //println("Syntax error")
                                            cur += 1
                                        }
                                    else if (!inTripString) 
                                        if (inString && !backslash) {
                                            inString = false
                                            if (idx == 1 && attrIdx == 0) {
                                                //println("after inString")
                                                content = content :+ Text(part.take(cur).drop(1))
                                                part = part.drop(cur + 1)
                                                len = part.length
                                                cur = 0
                                            } else
                                                cur += 1
                                        } else if (!inString) {
                                            inString = true
                                            if (idx == 1 && cur > 0 && attrIdx == 0) {
                                                //println("before inString")
                                                content = content ++ getXml({
                                                    parts(curP) = part.take(cur)
                                                    parts take(curP + 1)
                                                } , args)
                                                
                                                part = part drop cur
                                                parts = parts drop curP
                                                parts(0) = part
                                                
                                                cur = 1
                                                curP = 0
                                                len = part.length
                                                lenP = parts.length
                                            } else
                                                cur += 1
                                        } else
                                            cur += 1
                                    else
                                        cur += 1
                                }
                                case '\\' if ((inString || inTripString) && !backslash) => {
                                    backslash = true
                                    cur += 1
                                }
                                case _ => cur += 1
                            }
                            //if (cur < len) println(part(cur) + " - " + cur + " - " + attrIdx + " - " + idx + " - " + (if (inString) "Yes" else "No") + " - " + (if (inTripString) "Yes" else "No") + " - " + (if (backslash) "Yes" else "No"))
                            
                            if (idx == 0) {
                                val cPart = (part take(cur - 1)).trim
                                if (cPart.length > 0) {
                                    //println("NONE NONE NONE NONE NONE")
                                    
                                    content = content ++ getXml({
                                        parts(curP) = cPart
                                        parts take(curP + 1)
                                    } , args)
                                    
                                    //curP = 0
                                }
                                parts = parts drop curP
                                
                                part = part.drop(cur + 1).trim
                                parts(0) = part
                                
                                finished = true
                            } else {
                                if (cur == len) {
                                    curP += 1
                                    if (curP == lenP) {
                                        println("SYNTAX ERROR IN CONTENT")
                                        finished = true
                                    } else {
                                        part = parts(curP)
                                        cur = 0
                                        len = part.length
                                    }
                                }
                            }
                        }
                        //println("Content: " + content)
                    } else {
                        parts(curP) = part
                    }
                    //println("</" + tagName + ">")
                    
                    
                    
                    
                    val el: Node = if (attrsStr.trim.length > 0) {
                        var elTmp = new Elem(null, tagName, Null, TopScope, content: _*)
                        val getAttrName = """\s*\w+\s*""".r
                        val removeEq = """=\s*""".r
                        
                        while (attrsStr.trim.length > 0) {
                            
                            val attrName = getAttrName findFirstIn attrsStr getOrElse ""
                            attrsStr = (attrsStr drop attrName.length).trim
                            
                            if (attrName.trim.length > 0) {
                                
                                val attrValue: String = 
                                    if (attrsStr(0) != '=') {
                                        ""
                                    } else {
                                        attrsStr = attrsStr drop((removeEq findFirstIn attrsStr getOrElse "=").length)
                                        if (attrsStr(0) == '"' && attrsStr.length > 1) {
                                            var cur = 1
                                            var backslash = false
                                            val len = attrsStr.length
                                            //println(attrsStr)
                                            while (cur < len && (attrsStr(cur) != '"' || backslash)) {
                                                backslash = if (attrsStr(cur) == '\\') true else false
                                                cur += 1
                                            }
                                            //println(attrsStr(cur) + " - " + (if (backslash) "Yes" else "No"))
                                            
                                            val tmp = attrsStr.drop(1).take(cur - 1)
                                            attrsStr = attrsStr.drop(cur + 1)
                                            //println(tmp)
                                            tmp
                                        } else {
                                            println("Syntax error")
                                            attrsStr = ""
                                            ""
                                        }
                                    }
                                
                                //println("---------------------------------------------------------------.")
                                //println(attrName + " - " + attrValue)
                                //println("d----------------------------------------------------------------b")
                                
                                elTmp = elTmp % new UnprefixedAttribute(attrName.trim, Text(attrValue), Null)
                            } else {
                                println("Syntax error")
                                attrsStr = ""
                            }
                        }
                        elTmp
                    } else
                        new Elem(null, tagName, Null, TopScope, content: _*)
                        
                    if (part.trim == "" && parts.length > 1) {
                        parts = parts drop 1
                        part = parts(0)
                    }
                    
                    if (part.trim != "") {
                        //println("Next part of content")
                        Seq[Node](el) ++ getXml(parts, args)
                    } else
                        el
                    
                    
                    
                    
                    
                } else
                    content
            }
            
            
            getXml(str.parts.toArray, args)
            
            
            
            
            
            
            
            //val xml = atTag(str.parts, args)
            
            //xml
            
                    
                    // Er dette en Tuple2
                    // Er dette en List[Tuple2]
                    // Er dette en String
                        // Find stykket inden næste string, er der en ) i dette stykke ?
                            // Ja, => Do your thing
                        // Ellers
                            // Nej, Do it again
                    // Syntax - Fejl
                    
                    // Er dette en XML object (epxr)
                    // Er dette en String
                        // Find stykket inden næste string, er det tilhørende } i dette stykke ?
                            // Ja, => Do your thing
                        // Ellers
                            // Nej, Do it again
                    // Syntax - Fejl
               
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

val xml = xml""" 
 html {
    head {
        title {"Search Links - Build with Xml Builder in Scala"}
        style {"pageStyle"}
    }
    body {
        p (class="title" style="font-weight: 900;") {"Search Links:"}
        br
        "Hej Hej Hej"
        br
        p {"Farvel"}
    }
}"""

if (xml.length == 1) {
    val pp = new scala.xml.PrettyPrinter(80, 2)
    println(pp.format(xml(0)))
}



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
//                ${
//                    for ((name, link) <- searchLinks) yield {
//                        li {
//                            a
//                            "href" -> link;
//                            name
//                        }
//                    }
//                }
//            }
//        }
//    })
    
}