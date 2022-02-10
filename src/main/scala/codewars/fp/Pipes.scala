package codewars.fp

//complexity = 6 kyu
object Pipes {
  case class Pipe[A](value: A) {
    def pipe[B](mapper: A => B): B = mapper(value)

    def tap(ef: A => Unit): A = {
      ef(value) //performs a side-effect
      value
    }

    def |>[B](mapper: A => B): B = pipe(mapper)
  }

  //Implicit conversions:
  implicit def toPipe[A](value: A): Pipe[A] = Pipe(value)
}

