package dynamictypes

import scala.language.dynamics

class rubyXmlBuilder extends Dynamic {
    private var xml = ""
    private var xmlTag = ""
    val pretty = true
    val tabSpace = 4
    var level = -1
	def applyDynamic(name: String)(block: => Any, args: (String, Any)*): Any = {
//        println(name)
        level += 1
        if (pretty) {
            xml += tabSpaces(level) + "<" + name + makeArgs(args) + ">\n"
            val bStr = block
            if (bStr.isInstanceOf[String]) xml += tabSpaces(level + 1) + bStr + "\n"
            xml += tabSpaces(level) + "</" + name + ">\n"
        } else {
            xml += "<" + name + makeArgs(args) + ">"
            val bStr = block
            if (bStr.isInstanceOf[String]) xml += bStr
            xml += "</" + name + ">"
        }
        level -= 1
    }
    def tabSpaces(_level: Int): String = " " * (_level * tabSpace)
    def makeArgs(args: Seq[(String, Any)]): String = args.foldLeft("")((acc, i) => acc + " " + i._1 + "=\"" + i._2 + "\"")
    def getXml: String = xmlTag + xml
    def setXMLTag(args: (String, Any)*): Unit = {
        xmlTag = "<xml" + makeArgs(args) + ">"
        if (pretty) xmlTag += "\n"
        ()
    }
    def setDoctype(args: String*): Unit = {
        xmlTag = "<!DOCTYPE html" + args.foldLeft("")((acc, i) => acc + " " + i) + ">"
        if (pretty) xmlTag += "\n"
        ()
    }
    def resetXML = {
        xml = ""
        xmlTag = ""
        level = -1
    }
}