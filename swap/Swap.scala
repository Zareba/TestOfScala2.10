package swap

import scala.language.experimental.macros
import scala.reflect.macros.Context

object Swap {
    def swapL(a: Any, b: Any): Unit = macro swapL_impl
    
    def swapL_impl(c: Context)(a: c.Expr[Any], b: c.Expr[Any]): c.Expr[Unit] = {
        import c.universe._
        
        val (obj, tmpA, tmpB, indexA, indexB) = a.tree match {
            case Apply(Select(tmpObj1, _), List(Literal(Constant(tmpIndex1: Int)))) => b.tree match {
                    case Apply(Select(tmpObj2, _), List(Literal(Constant(tmpIndex2: Int)))) => 
                        if (tmpIndex1 < tmpIndex2)
                            (tmpObj1, a, b, tmpIndex1, tmpIndex2)
                        else
                            (tmpObj1, b, a, tmpIndex2, tmpIndex1)
                }
            }
        
        if (indexA == indexB)
            c.Expr[Unit](Literal(Constant(())))
        else
            c.Expr[Unit](Assign(obj,Apply(Select(Apply(Select(Apply(Select(Apply(Select(Apply(Select(obj,newTermName("drop")),List(Literal(Constant(indexB + 1)))),newTermName("$colon$colon$colon")),List(reify(List(tmpA.splice)).tree)),newTermName("$colon$colon$colon")),List(Apply(Select(Apply(Select(obj,newTermName("drop")),List(Literal(Constant(indexA + 1)))),newTermName("take")),List(Literal(Constant(indexB - indexA - 1)))))),newTermName("$colon$colon$colon")),List(reify(List(tmpB.splice)).tree)),newTermName("$colon$colon$colon")),List(Apply(Select(obj,newTermName("take")),List(Literal(Constant(indexA))))))))
    }
    
    def swapA(a: => Any, b: Any): Unit = macro swapA_impl
    
    def swapA_impl(c: Context)(a: c.Expr[Any], b: c.Expr[Any]): c.Expr[Unit] = {
        import c.universe._
        
        val (obj, tmpA, tmpB, indexA, indexB) = a.tree match {
            case Apply(Select(tmpObj1, _), List(Literal(Constant(tmpIndex1: Int)))) => b.tree match {
                    case Apply(Select(tmpObj2, _), List(Literal(Constant(tmpIndex2: Int)))) => 
                        if (tmpIndex1 < tmpIndex2)
                            (tmpObj1, a, b, tmpIndex1, tmpIndex2)
                        else
                            (tmpObj1, b, a, tmpIndex2, tmpIndex1)
                }
            }
        
        if (indexA == indexB)
            c.Expr[Unit](Literal(Constant(())))
        else
            c.Expr[Unit](Block(List(ValDef(Modifiers(), newTermName("tmp"), TypeTree(), tmpA.tree), Apply(Select(obj, newTermName("update")), List(Literal(Constant(indexA)), tmpB.tree)),Apply(Select(obj, newTermName("update")), List(Literal(Constant(indexB)), Ident(newTermName("tmp"))))), Literal(Constant(()))))
    }
}