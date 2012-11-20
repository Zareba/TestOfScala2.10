object swapTest extends App {
    import swap.Swap._
    
    var a: List[Int] = List(13, 11, 7, 5, 3, 2)
    
    println(a)
    
    swap(a(1), a(3))
    
    println(a)
}