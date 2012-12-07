object test extends App {
    import getAttributes.GetAttributes._
    
    val l = 3
    val in1 = Array(List(2),List(324),List(249),List("34"))
    //val in2 = Array(List("sfdsf"),List("lfs---pdf"))
    val in2 = Array(List(7),List(List(())))
    
    println(in1.toList)
    println(in2.toList)
    
    getAttributes(in1(l), in2(1+0))
    
    println(in1.toList)
    println(in2.toList)
    
}