package scala.util.parsing.combinator

import scala.util.matching.Regex

trait CharAndExprEitherParsers[E] extends Parsers  {
    import scala.util.parsing.input._
    
    type Elem = Either[Char, E]
    
    //override type Input = CharAndExprReader[E]
    
    protected val whiteSpace = """\s+""".r
    
    def skipWhitespace = whiteSpace.toString.length > 0
    
    protected def handleWhiteSpace(source: java.lang.CharSequence, offset: Int): Int =
        if (skipWhitespace)
            (whiteSpace findPrefixMatchOf (source.subSequence(offset, source.length))) match {
                case Some(matched) => offset + matched.end
                case None => offset
            }
        else
            offset
    
    def EXPR = new Parser[E] {
        def apply(in: Input) = {
            val (sSource, sOffset) = in.asInstanceOf[CharAndExprReader[E]].subSourceAndSubOffset
            sSource match {
                case Left(cSeq) => 
                    Failure("expr1` expected expr but " +  cSeq + " found", in.drop(cSeq.length))
                case Right(ex) => 
                    Success(ex, in.drop(1))
            }
        }
    }
    
    implicit def literal(s: String): Parser[String] = new Parser[String] {
        def apply(in: Input) = {
            val (sSource, sOffset) = in.asInstanceOf[CharAndExprReader[E]].subSourceAndSubOffset
            sSource match {
                case Left(cSeq) => 
                    if (sOffset < cSeq.length) {
                        val start = handleWhiteSpace(cSeq, sOffset)
                        var i = 0
                        var j = start
                        while (i < s.length && j < cSeq.length && s.charAt(i) == cSeq.charAt(j)) {
                            i += 1
                            j += 1
                        }
                        if (i == s.length)
                            Success(cSeq.subSequence(start, j).toString, in.drop(j - sOffset))
                        else  {
                            val found = if (start == cSeq.length()) "end of source" else "`" + cSeq.charAt(start) + "'"
                            Failure("1`" + s + "' expected but " + found + " found", in.drop(start - sOffset))
                        }
                    } else
                        Failure("2`" + s + "' expected but end of all sources reached", in)
                case Right(_) => 
                    Failure("3`" + s + "' expected but expr found", in.drop(1))
            }
        }
    }
    
    implicit def regex(r: Regex): Parser[String] = new Parser[String] {
        def apply(in: Input) = {
            val (sSource, sOffset) = in.asInstanceOf[CharAndExprReader[E]].subSourceAndSubOffset
            sSource match {
                case Left(cSeq) => 
                    if (sOffset < cSeq.length) {
                        val start = handleWhiteSpace(cSeq, sOffset)
                        (r findPrefixMatchOf (cSeq.subSequence(start, cSeq.length))) match {
                            case Some(matched) =>
                                Success(cSeq.subSequence(start, start + matched.end).toString, in.drop(start + matched.end - sOffset))
                            case None =>
                                val found = if (start == cSeq.length()) "end of source" else "`" + cSeq.charAt(start) + "'"
                                Failure("1string matching regex `" + r + "' expected but " + found + " found", in.drop(start - sOffset))
                        }
                    } else
                        Failure("2string matching regex `" + r + "' expected but end of all sources reached", in)
                case Right(_) => 
                    Failure("3string matching regex `" + r + "' expected but expr found", in.drop(1))
            }
        }
    }
    
    
    
    
    
    def parse[T](p: Parser[T], in: CharAndExprReader[E]): ParseResult[T] = p(in)
    
    def parseAll[T](p: Parser[T], cSource: Array[java.lang.CharSequence], eSource: Array[E]): ParseResult[T] = 
        parse(phrase(p), new CharAndExprReader[E](cSource, eSource, 0))
    
}