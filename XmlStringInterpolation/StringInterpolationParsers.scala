package scala.util.parsing.combinator
import scala.xml.{Elem => nElem, _}

class StringInterpolationParsers extends CharAndExprEitherParsers[Seq[Node]]  {
    
    val string = """[a-zA-Z-_#]+""".r
    
    val noDoubleQuote = """[^\"]+""".r
    
    
//    def tag: Parser[Seq[Node]] = tagName ~ opt(attrs) ~ opt(content) ^^ {
//        case nameStr ~ None ~ None => new nElem(null, nameStr, Null, TopScope, true)
//        case nameStr ~ Some(seqAttr) ~ None => seqAttr.foldLeft(new nElem(null, nameStr, Null, TopScope, true))(_ % _)
//        case nameStr ~ None ~ Some(cont) => new nElem(null, nameStr, Null, TopScope, true, cont: _*)
//        case nameStr ~ Some(seqAttr) ~ Some(cont) => seqAttr.foldLeft(new nElem(null, nameStr, Null, TopScope, true, cont: _*))(_ % _)
//    }
    
    
    def tag: Parser[Seq[Node]] = tagWithAttrsWithContent | tagWithAttrsWithoutContent | tagWithoutAttrsWithContent | tagWithAttrsWithContent
   
    def tagWithoutAttrsWithoutContent: Parser[Seq[Node]] = tagName ^^ {
        case nameStr => new nElem(null, nameStr, Null, TopScope, true)
    }
    
    def tagWithoutAttrsWithContent: Parser[Seq[Node]] = tagName ~ content ^^ {
        case nameStr ~ cont => new nElem(null, nameStr, Null, TopScope, true, cont: _*)
    }
    
    def tagWithAttrsWithoutContent: Parser[Seq[Node]] = tagName ~ attrs ^^ {
        case nameStr ~ seqAttr => seqAttr.foldLeft(new nElem(null, nameStr, Null, TopScope, true))(_ % _)
    }
    
    def tagWithAttrsWithContent: Parser[Seq[Node]] = tagName ~ attrs ~ content ^^ {
        case nameStr ~ seqAttr ~ cont => seqAttr.foldLeft(new nElem(null, nameStr, Null, TopScope, true, cont: _*))(_ % _)
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
    
    def attrName: Parser[String] = {EXPR | string} ^^ {
        case Text(str) => str
        case str => str.toString
    }
    
    def attrValue: Parser[String] = {EXPR | "\"" ~> noDoubleQuote <~ "\"" | string} ^^ {
        case Text(str) => str
        case str => str.toString
    }
    
    def content: Parser[Seq[Node]] = "{" ~> rep(EXPR | "\"" ~> noDoubleQuote <~ "\"" | tag) <~ "}" ^^ {
        case lst => lst.map { el => 
            el match {
                case str: String => Seq(Text(str))
                case sn: Seq[_] => sn map {case nd: Node => nd}
            }
        }.flatten.toSeq
    }
}