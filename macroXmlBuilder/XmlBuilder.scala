package xmlbuilder

import scala.language.experimental.macros
import scala.reflect.macros.Context

object XmlBuilder {
    def xml(block: Any): String = macro xml_impl
    
    def xml_impl(c: Context)(block: c.Expr[Any]): c.Expr[String] = {
        import c.universe._
        xml_r_impl(c)(List(block.tree))
    }
    
    def xml_r_impl(c: Context)(lst: List[c.Tree]): c.Expr[String] = {
        import c.universe._
        
        val ast_symbol = newTypeName("Symbol")
        val ast_apply = newTypeName("apply")
        val ast_tuple2 = newTypeName("Tuple2")
        val ast_plus = newTypeName("$plus")
        
        val typeListTuple2StringString = typeOf[List[(String, String)]]
        // val typeListTuple2StringString = weakTypeOf[List[(String, String)]]
        
        val typeTuple2StringString = typeOf[(String, String)]
        // val typeTuple2StringString = weakTypeOf[(String, String)]
        
        def addStrings(str1: c.Tree, str2: c.Tree): c.Expr[String] = c.Expr[String](
            Apply(
                Select(
                    str1,
                    newTermName("$plus")
                ),
                List(str2)
            )
        )
        
        def tagOutAttrs(tagName: String, content: c.Tree): c.Expr[String] = 
            tag(
                tagName, 
                Literal(Constant("")), 
                content
            )
        
        def tagOneAttr(tagName: String, attr: c.Tree, content: c.Tree): c.Expr[String] = 
            tag(
                tagName, 
                reify(" " + c.Expr[(String,String)](attr).splice._1 + "=\"" + c.Expr[(String,String)](attr).splice._2 + "\"").tree, 
                content
            )
        
        
        def tagAttrs(tagName: String, attrs: c.Tree, content: c.Tree): c.Expr[String] = 
            tag(
                tagName, 
                reify(c.Expr[List[(String,String)]](attrs).splice.foldLeft("")((acc,el) => acc + " " + el._1 + "=\"" + el._2 + "\"")).tree, 
                content
            )

        
        def tag(tagName: String, attrs: c.Tree, content: c.Tree): c.Expr[String] = {
            addStrings(
                addStrings(
                    addStrings(
                        addStrings(
                            Literal(Constant("<" + tagName)),
                            attrs
                        ).tree,
                        Literal(Constant(">"))
                    ).tree,
                    xml_r_impl(c)(List(content)).tree
                ).tree,
                Literal(Constant("</" + tagName + ">"))
            )}
        
        
        
        lst match {
            case List(
                        Apply(
                            Select(Select(Ident(scala), ast_symbol), ast_apply), 
                            List(Literal(Constant(tagName: String)))
                        ),
                        tuple2,
                        content,
                        next @ _*
                    ) if (tuple2.tpe =:= typeTuple2StringString) => 
                        addStrings(
                            tagOneAttr(tagName, tuple2, content).tree,
                            xml_r_impl(c)(next.toList).tree
                        )
                    
            case List(
                        Apply(
                            Select(Select(Ident(scala), ast_symbol),  ast_apply), 
                            List(Literal(Constant(tagName: String)))
                        ),
                        lstTuple2,
                        content,
                        next @ _*
                    ) if (lstTuple2.tpe =:= typeListTuple2StringString) => 
                        addStrings(
                            tagAttrs(tagName, lstTuple2, content).tree,
                            xml_r_impl(c)(next.toList).tree
                        )
                    
            case List(
                        Apply(
                            Select(Select(Ident(scala), ast_symbol),  ast_apply), 
                            List(Literal(Constant(tagName: String)))
                        ),
                        content,
                        next @ _*
                    ) => 
                        addStrings(
                            tagOutAttrs(tagName, content).tree,
                            xml_r_impl(c)(next.toList).tree
                        )
                    
            case List(
                        Block(lstExp, expTree),
                        next @ _*
                    ) => 
                        addStrings(
                            xml_r_impl(c)(lstExp ::: List(expTree)).tree,
                            xml_r_impl(c)(next.toList).tree
                        )
                    
            case lstExp @ List(_*) => 
                        if (lstExp.isEmpty)
                            c.Expr(Literal(Constant("")))
                        else {
                            val expTree = lstExp.head.duplicate match {
                                case Apply(sym, trees) =>
                                    Apply(
                                        xml_r_impl(c)(List(sym)).tree, 
                                        List(xml_r_impl(c)(trees).tree)
                                    )
                                
                                case TypeApply(sym, trees) => 
                                    TypeApply(
                                        xml_r_impl(c)(List(sym)).tree, 
                                        trees
                                    )
                                
                                case Select(tree, term) => 
                                    Select(
                                        xml_r_impl(c)(List(tree)).tree, 
                                        term
                                    )
                                
                                case Function(lstTmp, tree) => 
                                    c.resetAllAttrs(
                                        Function(
                                            lstTmp,
                                            xml_r_impl(c)(List(tree)).tree
                                        )
                                    )
                                
                                case Match(typed, lstOfCaseDef) => 
                                    Match(
                                        typed,
                                        lstOfCaseDef.foldRight(List[CaseDef]()) {
                                            (el, acc) => 
                                                (el match {
                                                    case CaseDef(p1, p2, p3) => CaseDef(p1, p2, xml_r_impl(c)(List(p3)).tree)
                                                    case _ => el
                                                }) :: acc
                                        }
                                    )
                                
                                case _ => lstExp.head
                            }
                            
                            if (lstExp.tail.isEmpty)
                                c.Expr(expTree)
                            else
                                addStrings(
                                    expTree,
                                    xml_r_impl(c)(lstExp.tail).tree
                                )
                        }
        }
    }
}