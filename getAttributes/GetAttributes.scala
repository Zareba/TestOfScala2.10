package getAttributes

import scala.language.experimental.macros
import scala.reflect.macros.Context

object GetAttributes {
    def getAttributes(a: Any, b: Any): Unit = macro getAttributes_impl
    
    def getAttributes_impl(c: Context)(a: c.Expr[Any], b: c.Expr[Any]): c.Expr[Unit] = {
        import c.universe._
        
        val Apply(Select(objA, _), List(indexA)) = a.tree
        val Apply(Select(objB, _), List(indexB)) = b.tree
        
        // I would like to check if (!(a.tree.tpe subTypeOf b.tree.tpe && b.tree.tpe subTypeOf a.tree.tpe))
        //if (!(a.tree.tpe =:= b.tree.tpe)) c.error(c.enclosingPosition, "The values that are being swapped should have same type")
        
        if (!(indexA.tpe.erasure =:= indexB.tpe.erasure)) c.error(c.enclosingPosition, "The values used to retrieve the swapping values should have same type")
        
        if (objA.tpe.typeSymbol.typeSignature.member(stringToTermName("update")).isMethod == false) c.error(c.enclosingPosition, "Update method is missing")
        if (objB.tpe.typeSymbol.typeSignature.member(stringToTermName("update")).isMethod == false) c.error(c.enclosingPosition, "Update method is missing")
        
        c.Expr[Unit](
            Block(
                List(
                    ValDef(
                        Modifiers(), 
                        newTermName("tMp3424VaLUe"), 
                        TypeTree(), 
                        a.tree
                    ), 
                    Apply(
                        Select(
                            objA, 
                            newTermName("update")
                        ), 
                        List(
                            indexA, 
                            b.tree
                        )
                    ),
                    Apply(
                        Select(
                            objB, 
                            newTermName("update")
                        ), 
                        List(
                            indexB, 
                            Ident(newTermName("tMp3424VaLUe"))
                        )
                    )
                ), 
                Literal(Constant(()))
            )
        )
    }
}