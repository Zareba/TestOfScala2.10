package scala.util.parsing.input

object CharAndExprEitherReader {
    final val EofCh = '\032'
}

class CharAndExprEitherReader[E](charSource: Array[java.lang.CharSequence], eSource: Array[E], override val offset: Int) extends EitherReader[Char, E] {
    import CharAndExprEitherReader._
    
    lazy val length: Int = charSource.foldLeft(0)(_+_.length) + eSource.length
    
    def subSourceAndSubOffset: Tuple2[Either[java.lang.CharSequence, E], Int] = {
        var cur = 0
        var curL = 0
        var n = offset
        var either: Tuple2[Either[java.lang.CharSequence, E], Int] = (Left(charSource.last), charSource.last.length)
        if (!atEnd) {
            while (n >=0) {
                curL = charSource(cur).length
                if (n < curL) {either = (Left(charSource(cur)), n); n = -1}
                else if (n == curL) {either = (Right(eSource(cur)), n); n = -1}
                else {
                    n -= (curL + 1)
                    cur += 1
                }
            }
        }
        either
    }
    
    def first: Either[Char, E] = {
        var either: Either[Char, E] = Left(EofCh)
        if (!atEnd) {
            val (sSource, sOffset) = subSourceAndSubOffset
            either = sSource match {
                case Left(cSeq) => Left(cSeq.charAt(sOffset))
                case Right(ex) => Right(ex) // case ex @ Right(_) => ex  // This gives the wrong type
            }
        }
        either
    }
    
    def rest: CharAndExprEitherReader[E] = drop(offset + 1)
    
    def pos: Position = new CharAndExprEitherOffsetPosition(charSource, eSource, offset)
    
    def atEnd: Boolean = offset >= length
    
    override def drop(n: Int): CharAndExprEitherReader[E] = new CharAndExprEitherReader[E](charSource, eSource, offset + n)
}