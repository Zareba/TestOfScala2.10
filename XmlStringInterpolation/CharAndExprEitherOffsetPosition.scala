package scala.util.parsing.input

case class CharAndExprEitherOffsetPosition[E](charSource: Array[java.lang.CharSequence], eSource: Array[E], offset: Int) extends Position {
    
    lazy val length: Int = charSource.foldLeft(0)(_+_.length) + eSource.length
    
    val (sectionVal: Int, sectionOffsetVal: Int, lineVal: Int, columnVal: Int) = {
            var cur = 0
            var curL = 0
            var lineNr = 1
            var columnNr = 0
            var section = 0
            var sectionOffset = 0
            var n = offset
            var cSource: java.lang.CharSequence = ""
            while (n >=0) {
                cSource = charSource(cur)
                curL = cSource.length
                if (n < curL) {
                    section = cur * 2
                    sectionOffset = n
                    for (i <- 0 until n) {
                        if (cSource.charAt(i) == '\n') {
                            lineNr += 1
                            columnNr = 0
                        }
                        columnNr += 1
                    }
                    n = -1
                } else if (n == curL) {
                    for (i <- 0 until curL) if (cSource.charAt(i) == '\n') lineNr += 1
                    section = cur * 2 + 1
                    sectionOffset = 0
                    n = -1
                } else {
                    for (i <- 0 until curL) if (cSource.charAt(i) == '\n') lineNr += 1
                    n -= (curL + 1)
                    cur += 1
                }
            }
            (section, sectionOffset, lineNr, columnNr)
        
    }
    
    def line: Int = lineVal
    
    def column: Int = columnVal
    
    def lineContents: String = {
        if (sectionVal % 2 == 0) {
            val cs = charSource(sectionVal / 2)
            var n0 = 0
            var n1 = cs.length
            var strFound = false
            for (i <- 0 until cs.length) {
                if (strFound == false && cs.charAt(i) == '\n') {
                    if (i > sectionOffsetVal) {
                        n1 = i
                        strFound = true
                    } else
                        n0 = i + 1
                }
            }
            cs.subSequence(n0, n1).toString()
        } else {
            "Error at Expr nr. " + ((sectionVal + 1) / 2)
        }
    }
    
    override def toString = lineVal+"."+columnVal+"-"+(sectionVal+1)+"."+sectionOffsetVal
    
    override def <(that: Position) = that match {
        case CharAndExprEitherOffsetPosition(_, _, that_offset) =>
            offset < that_offset
        case _ =>
            this.line < that.line || this.line == that.line && this.column < that.column
    }
}