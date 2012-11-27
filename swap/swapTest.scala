object swapTest extends App {
    import swap.Swap._
    
    
    
    var a: List[Int] = List(13, 11, 7, 5, 3, 2)
    
    println(a)
    
    swapL(a(1), a(3))
    
    println(a)
    
    
    
    val b = Array(7,6,5,4,3,2,1)
    
    println(b.toList)
    
    swapA(b(2), b(5))
    
    println(b.toList)

}