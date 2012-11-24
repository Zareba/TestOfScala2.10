package xmlbuilder

import scala.language.experimental.macros
import scala.reflect.macros.Context

object XmlBuilder {
    def xml(block: Any): String = macro xml_impl
    
    def xml_impl(c: Context)(block: c.Expr[Any]): c.Expr[String] = {
        import c.universe._
        
        def evalNext(next: c.Tree): c.Expr[String] = xml_impl(c)(c.Expr(next))
        def tag(tagName: String, next: c.Tree): c.Expr[String] = 
            c.Expr[String](
                Apply(
                    Select(
                        Apply(
                            Select(
                                Literal(Constant("<" + tagName + ">")),
                                newTermName("$plus")
                            ),
                            List(next)
                        ),
                        newTermName("$plus")
                    ),
                    List(Literal(Constant("</" + tagName + ">")))
                )
            )
        
        def tagE(tagName: String, next: c.Tree): c.Expr[String] = 
            c.Expr[String](
                Apply(
                    Select(
                        Apply(
                            Select(
                                Literal(Constant("<" + tagName + ">")),
                                newTermName("$plus")
                            ),
                            List(evalNext(next).tree)
                        ),
                        newTermName("$plus")
                    ),
                    List(Literal(Constant("</" + tagName + ">")))
                )
            )
        
        def tagC(tagName: String, content: String): c.Expr[String] = 
            c.Expr[String](
                Apply(
                    Select(
                        Apply(
                            Select(
                                Literal(Constant("<" + tagName + ">")),
                                newTermName("$plus")
                            ),
                            List(Literal(Constant(content)))
                        ),
                        newTermName("$plus")
                    ),
                    List(Literal(Constant("</" + tagName + ">")))
                )
            )
        
        val ast_symbolToFunction = Select(Select(Ident("xmlbuilder"), "XmlBuilder"), newTypeName("symbolToFunction"))
        val ast_tupleToFunction = Select(Select(Ident("xmlbuilder"), "XmlBuilder"), newTypeName("tupleToFunction"))
        val ast_apply = newTermName("apply")
        val ast_symbolApply = Select(Select(Ident("scala"), newTypeName("Symbol")),  ast_apply)
        val ast_listApply = TypeApply(Select(Select(This(newTypeName("immutable")), "List"), ast_apply), List(TypeTree()))
        
        block.tree match {
            case Literal(Constant(content: String)) => c.Expr[String](Literal(Constant(content)))
            case Block(Literal(Constant(content: String))) => c.Expr[String](Literal(Constant(content)))              // Look at me later
            case Apply(
                        Select(
                            Apply(
                                ast_symbolToFunction,
                                List(
                                    Apply(
                                        ast_symbolApply, 
                                        List(Literal(Constant(tagName: String)))
                                    )
                                )
                            ),
                            ast_apply
                        ),
                        List(next)
                    ) => reify(tagE(tagName, next).splice)
            case Block(
                        List(
                            Apply(
                                Select(
                                    Apply(
                                        ast_symbolToFunction,
                                        List(
                                            Apply(
                                                ast_symbolApply, 
                                                List(Literal(Constant(tagName: String)))
                                            )
                                        )
                                    ),
                                    ast_apply
                                ),
                                List(next)
                            ),
                            n2 @ _*
                        ),
                    n3 @ _) => {
                                    val tag = tagE(tagName, next)
                                    val nextNext = n2.toList ::: List(n3)
                                    val evalN = if (nextNext.isEmpty) reify("") else evalNext(Block(nextNext : _*))
                                    reify(tag.splice + evalN.splice)
                                }
            case Block(
                        List(
                            Apply(
                                ast_symbolApply,
                                List(Literal(Constant(tagName: String)))
                            ),
                            tmp @ _*
                        ),
                        next2) => tmp.toList ::: List(next2) match {
                                            case next @ List(
                                                Apply(
                                                    ast_symbolApply,
                                                    List(Literal(Constant(_: String)))
                                                ),
                                                _*
                                            ) => {
                                                val tag = tagC(tagName, "")
                                                val evalN = if (next.isEmpty) reify("") else evalNext(Block(next.toList : _*))
                                                reify(tag.splice + evalN.splice)
                                            }
                                            case List(
                                                list @ Apply(
                                                    ast_listApply,
                                                    List(_*)
                                                ),
                                                Literal(Constant(content: String)),
                                                next @ _*
                                            ) => {
                                                val tag = tagC(tagName, content)
                                                val evalN = if (next.isEmpty) reify("") else evalNext(Block(next.toList : _*))
                                                reify(tag.splice + evalN.splice)
                                            }
                                            case List(
                                                Literal(Constant(content: String)),
                                                next @ _*
                                            ) => {
                                                val tag = tagC(tagName, content)
                                                val evalN = if (next.isEmpty) reify("") else evalNext(Block(next.toList : _*))
                                                reify(tag.splice + evalN.splice)
                                            }
                                            case List(
                                                Apply(
                                                    Select(
                                                        Apply(
                                                            ast_tupleToFunction,
                                                            List(tuple2)
                                                        ),
                                                        ast_apply
                                                    ),
                                                    List(content)
                                                ),
                                                next @ _*
                                            ) => {
                                                val tagT = tag(tagName, content)
                                                val evalN = if (next.isEmpty) reify("") else evalNext(Block(next.toList : _*))
                                                reify(tagT.splice + evalN.splice)
                                            }
                                            case List(
                                                content,
                                                next @ _*
                                            ) => {
                                                val tagT = tag(tagName, content)
                                                val evalN = if (next.isEmpty) reify("") else evalNext(Block(next.toList : _*))
                                                reify(tagT.splice + evalN.splice)
                                            }
                                            case content @ _ => c.Expr[String](Literal(Constant("---***---" + showRaw(content))))
                                        }
            case content @ _ => c.Expr[String](Literal(Constant("---*---" + showRaw(content))))
        }
    }
    
    implicit def symbolToFunction(s: Symbol): Function1[Any, Any] = (t: Any) => t
    implicit def tupleToFunction(t: Tuple2[Any, Any]): Function1[Any, Any] = (b: Any) => b
}