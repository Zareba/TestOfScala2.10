package scala.util.parsing.combinator
import scala.xml.{Elem => nElem, _}

class StringInterpolationParsers extends CharAndExprEitherParsers[Seq[Node]]  {
    
    val string = """[a-zA-Z-_#]+""".r
    
    val noDoubleQuote = """[^\"]+""".r
    
    
    def tag: Parser[Seq[Node]] = tagName ~ opt(attrs) ~ opt(content) ^^ {
        case nameStr ~ None ~ None => new nElem(null, nameStr, Null, TopScope)
        case nameStr ~ Some(seqAttr) ~ None => 
            seqAttr.foldLeft(new nElem(null, nameStr, Null, TopScope))(_ % _)
        case nameStr ~ None ~ Some(cont) => new nElem(null, nameStr, Null, TopScope, cont: _*)
        case nameStr ~ Some(seqAttr) ~ Some(cont) => 
            seqAttr.foldLeft(new nElem(null, nameStr, Null, TopScope, cont: _*))(_ % _)
    }
    
    def tagName: Parser[String] = string ^^ {
        case str => str.toString
    }
    
    def attrs: Parser[Seq[UnprefixedAttribute]] = "(" ~> rep1(attr) <~ ")" ^^ {
        case lst => lst.toSeq
    }
    
    def attr: Parser[UnprefixedAttribute] = {attrName ~ "=" ~ attrValue | attrName} ^^ {
        case nameStr ~ "=" ~ valStr => new UnprefixedAttribute(nameStr.toString, Text(valStr.toString), Null)
        case nameStr => new UnprefixedAttribute(nameStr.toString, Text(""), Null)
    }
    
    def attrName: Parser[String] = string ^^ {
        case str => str.toString
    }
    
    def attrValue: Parser[String] = "\"" ~> noDoubleQuote <~ "\"" | string | EXPR ^^ {
        case str => str.toString
    }
    
    def content: Parser[Seq[Node]] = "{" ~> rep("\"" ~> noDoubleQuote <~ "\"" | tag | EXPR) <~ "}" ^^ {
        case lst => lst.map { el => 
            el match {
                case str: String => Seq(Text(str))
                case sn: NodeSeq => sn
            }
        }.flatten.toSeq
    }
}