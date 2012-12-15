package scala.util.parsing.input

abstract class EitherReader[T, U] extends Reader[Either[T, U]]