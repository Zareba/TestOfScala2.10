object newXSI extends App {
    import scala.util.parsing.combinator._
    import scala.xml._
    
    val str = Array[java.lang.CharSequence]("""body (backgroud-color="#C333C3") {""", """br "Det er bare en test;)"}""")
    val ex = Array[Seq[Node]](new Elem(null, "p", Null, TopScope, Text("Hello World")))
    
    //val str = Array[java.lang.CharSequence]("""body (backgroud-color="#C333C3") {p {"Hello World"} br "Det er bare en test;)"}""")
    //val ex = Array[String]()
    
    val sip = new StringInterpolationParsers
    val result = sip.parseAll(sip.tag, str, ex)
    if (result.successful)
        println(result.get)
    else
        println(result)
}