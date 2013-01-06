package scala.util.parsing.combinator

import scala.util.matching.Regex

trait CharAndExprEitherParsers[E] extends Parsers  {
    import scala.language.implicitConversions
    import scala.util.parsing.input._
    
    type Elem = Either[Char, E]
    
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
            val (sSource, sOffset) = in.asInstanceOf[CharAndExprEitherReader[E]].subSourceAndSubOffset
//            println("--------------EXPR--------------")
//            println(sSource)
//            println(sOffset)
            sSource match {
                case Left(cSeq) => 
                    Failure("expected expr but found:\n" +  cSeq.subSequence(0, sOffset).toString + "\nEND", in)
                case Right(ex) => 
                    Success(ex, in.drop(1))
            }
        }
    }
    
    implicit def literal(s: String): Parser[String] = new Parser[String] {
        def apply(in: Input) = {
            val (sSource, sOffset) = in.asInstanceOf[CharAndExprEitherReader[E]].subSourceAndSubOffset
//            println("--------------literal--------------")
//            println(s)
//            println(sSource)
//            println(sOffset)
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
                            val found = if (start == cSeq.length()) "end of source" else cSeq.charAt(start)
                            Failure(s + " expected but " + found + " found", in.drop(start - sOffset))
                        }
                    } else
                        Failure(s + " expected but end of all sources reached", in.drop(cSeq.length - sOffset))
                case Right(_) => 
                    Failure(s + " expected but expr found", in)
            }
        }
    }
    
    implicit def regex(r: Regex): Parser[String] = new Parser[String] {
        def apply(in: Input) = {
            val (sSource, sOffset) = in.asInstanceOf[CharAndExprEitherReader[E]].subSourceAndSubOffset
//            println("--------------regex--------------")
//            println(""+r)
//            println(sSource)
//            println(sOffset)
            sSource match {
                case Left(cSeq) => 
                    if (sOffset < cSeq.length) {
                        val start = handleWhiteSpace(cSeq, sOffset)
                        (r findPrefixMatchOf (cSeq.subSequence(start, cSeq.length))) match {
                            case Some(matched) =>
                                Success(cSeq.subSequence(start, start + matched.end).toString, in.drop(start + matched.end - sOffset))
                            case None =>
                                val found = if (start == cSeq.length()) "end of source" else cSeq.charAt(start)
                                Failure("string matching regex " + r + "' expected but \"" + found + "\" found", in.drop(start - sOffset))
                        }
                    } else
                        Failure("string matching regex " + r + "' expected but end of all sources reached", in.drop(cSeq.length - sOffset))
                case Right(_) => 
                    Failure("string matching regex " + r + "' expected but expr found", in)
            }
        }
    }
        
//    override def positioned[E <: Positional](p: => Parser[E]): Parser[E] = {
//        val pp = super.positioned(p)
//        new Parser[E] {
//            def apply(in: Input) = {
//                val (sSource, sOffset) = in.asInstanceOf[CharAndExprEitherReader[E]].subSourceAndSubOffset
//                sSource match {
//                    case Left(cSeq) => 
//                        val start = handleWhiteSpace(cSeq, sOffset)
//                        pp(in.drop(start - sOffset))
//                    case Right(ex) => 
//                        pp(in.drop(1))
//                }
//            }
//        }
//    }
    
//    override def phrase[T](p: Parser[T]): Parser[T] = super.phrase(p <~ opt("""\z""".r))
    
    def parse[T](p: Parser[T], in: CharAndExprEitherReader[E]): ParseResult[T] = p(in)
    
    def parseAll[T](p: Parser[T], cSource: Array[java.lang.CharSequence], eSource: Array[E]): ParseResult[T] = 
        parse(phrase(p), new CharAndExprEitherReader[E](cSource, eSource, 0))
    
}