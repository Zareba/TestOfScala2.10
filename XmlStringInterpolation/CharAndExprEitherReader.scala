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
        while (n >=0) {
            curL = charSource(cur).length
            if (n < curL) either = (Left(charSource(cur)), n)
            else if (n == curL) either = (Right(eSource(cur)), n)
            n -= curL + 1
            cur += 1
        }
        either
    }
    
    def first: Either[Char, E] = {
        var either: Either[Char, E] = Left(EofCh)
        if (offset < length) {
            val (sSource, sOffset) = subSourceAndSubOffset
            either = sSource match {
                case Left(cSeq) => Left(cSeq.charAt(sOffset))
                case Right(ex) => Right(ex) // case ex @ Right(_) => ex  // This gives the wrong type
            }
        }
        either
    }
    
    def rest: CharAndExprEitherReader[E] = if (offset < length) new CharAndExprEitherReader[E](charSource, eSource, offset + 1) else this
    
    // I do not know if I have to use this later
    def pos: Position = NoPosition
    
    def atEnd: Boolean = offset >= length
    
    override def drop(n: Int): CharAndExprEitherReader[E] = new CharAndExprEitherReader[E](charSource, eSource, offset + n)
}