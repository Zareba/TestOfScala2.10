package xmlbuilder

import scala.language.experimental.macros
import scala.reflect.macros.Context

object XmlBuilder {
    def xml(block: Any): String = macro xml_impl
    
    def xml_impl(c: Context)(block: c.Expr[Any]): c.Expr[String] = {
        import c.universe._
        
        println("-----------------------------------------------------------------------------------------------------------")
        //println(showRaw(block.tree))
        
        def evalNext(next: c.Tree): c.Expr[String] = xml_impl(c)(c.Expr(next))
        def tag(tagName: String, next: c.Tree): c.Expr[String] = {
            println("------------------"+showRaw(next))
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
            )}
        
        def tagE(tagName: String, next: c.Tree): c.Expr[String] = 
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
        
        val ast_symbolToFunction = Select(Select(Ident("xmlbuilder"), "XmlBuilder"), "symbolToTag5")
        val ast_tupleToFunction = Select(Select(Ident("xmlbuilder"), "XmlBuilder"), "symbolToTag5")
        val ast_apply = newTermName("apply")
        val ast_symbolApply = Select(Select(Ident("scala"), "Symbol"),  ast_apply)
        
        block.tree match {
            case Literal(Constant(content: String)) => c.Expr[String](Literal(Constant(content)))
            case Block(Literal(Constant(content: String))) => c.Expr[String](Literal(Constant(content)))              // Tjek mig sener
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
                    ) => tag(tagName, next)
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
                            _*
                        ),
                    _) => tag(tagName, next)
            case Block(
                        List(
                            Apply(
                                ast_symbolApply,
                                List(Literal(Constant(tagName: String)))
                            ),
                            tmp @ _*
                        ),
                        next2) => {println("3333"+showRaw(tmp.toList ::: List(next2))); tmp.toList ::: List(next2) match {
                                            case List(
                                                // Tuple2(_, _),
                                                Literal(Constant(content: String)),
                                                next @ _*
                                            ) => {
                                                val tag = tagC(tagName, content)
                                                val evalN = if (next.isEmpty) reify("") else evalNext(Block(next.toList : _*))
                                                reify(tag.splice + evalN.splice)
                                            }
                                            case List(
                                                List(_),
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
                                                content,
                                                next @ _*
                                            ) => {
                                                println("44444"+showRaw(content))
                                                val tag = tagE(tagName, content)
                                                val evalN = if (next.isEmpty) reify("") else evalNext(Block(next.toList : _*))
                                                reify(tag.splice + evalN.splice)
                                            }
                                            case content @ _ => c.Expr[String](Literal(Constant("Nope2" + showRaw(content))))
                                        }}
            case content @ _ => c.Expr[String](Literal(Constant("Nope" + showRaw(content))))
        }
    }
    
    def makeArgs(args: Seq[(String, Any)]): String = args.foldLeft("")((acc, i) => acc + " " + i._1 + "=\"" + i._2 + "\"")
    
    implicit def symbolToFunction(s: Symbol): Function1[Any, Any] = (t: Any) => t
    implicit def tupleToFunction(t: Tuple2[Any, Any]): Function1[Any, Any] = (b: Any) => b
}