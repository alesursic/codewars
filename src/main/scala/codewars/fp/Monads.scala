package codewars.fp

//complexity = 4 kyu
object Monads {
  trait Monad[M[_]] {
    def unit[A](a: A): M[A]
    def bind[A, B](ma: M[A])(amb: A => M[B]): M[B]
  }

  trait Monoid[A] {
    def mempty: A
    def mappend(x: A)(y: A): A
  }

  case class Identity[A](a: A)
  object Identity {
    implicit val identityMonad = new Monad[Identity] {
      def unit[A](a: A): Identity[A] = Identity(a)
      def bind[A, B](ma: Identity[A])(amb: A => Identity[B]): Identity[B] = amb(ma.a)
    }
  }

  sealed trait Maybe[+A]
  case class Just[A](a: A) extends Maybe[A]
  case object None extends Maybe[Nothing]
  object Maybe {
    implicit val maybeMonad = new Monad[Maybe] {
      def unit[A](a: A): Maybe[A] = Just(a)
      def bind[A, B](ma: Maybe[A])(amb: A => Maybe[B]): Maybe[B] =  ma match {
        case Just(a) => amb(a)
        case None => None
      }
    }
  }

  case class State[S, A](run: S => (S, A))
  object State {
    // inspired by https://stackoverflow.com/a/6248296
    implicit def stateMonad[S] = new Monad[({type x[a]=State[S, a]})#x] {
      def unit[A](a: A): State[S, A] = State(s => (s, a))
      def bind[A, B](ma: State[S, A])(amb: A => State[S, B]): State[S, B] = State(s0 => {
        val (s1, a) = ma.run(s0)
        val newState: State[S, B] = amb(a)

        newState.run(s1)
      })
    }
  }

  case class Reader[R, A](run: R => A)
  object Reader {
    implicit def readerMonad[R] = new Monad[({type x[a]=Reader[R, a]})#x] {
      def unit[A](a: A): Reader[R, A] = Reader(_ => a)
      def bind[A, B](ma: Reader[R, A])(amb: A => Reader[R, B]): Reader[R, B] = Reader(r0 => {
        val a = ma.run(r0)
        val newReader: Reader[R, B] = amb(a)

        newReader.run(r0)
      })
    }
  }

  case class Writer[W, A](run: (W, A))
  object Writer {
    implicit def readerMonad[W](implicit m: Monoid[W]) = new Monad[({type x[a]=Writer[W, a]})#x] {
      def unit[A](a: A): Writer[W, A] = Writer((m.mempty, a))
      def bind[A, B](ma: Writer[W, A])(amb: A => Writer[W, B]): Writer[W, B] = Writer({
        val (w0, a) = ma.run
        val newWriter: Writer[W, B] = amb(a)
        val (w1, b) = newWriter.run

        (m.mappend(w0)(w1), b)
      })
    }
  }
}