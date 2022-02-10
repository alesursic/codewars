package codewars.fp

//complexity = 1 kyu
object Scott {

  trait STuple[+A, +B] {
    def apply[C]: ((A, B) => C) => C
  }

  trait SOption[+A] {
    def apply[B]: (=> B, A => B) => B
  }

  trait SEither[+A, +B] {
    def apply[C]: (A => C, B => C) => C
  }

  trait SList[+A] {
    def apply[B]: (=> B, (A, SList[A]) => B) => B
  }

  def toTuple[A, B](tuple: STuple[A, B]): (A, B) =
    tuple[(A, B)]((_, _))

  def fromTuple[A, B](tuple: (A, B)): STuple[A, B] = new STuple[A, B] {
    override def apply[C] = tuple match {
      case (a, b) => f => f(a, b)
    }
  }

  def fst[A, B](tuple: STuple[A, B]): A = tuple[A]((a, _) => a)

  def snd[B](tuple: STuple[_, B]): B = tuple[B]((_, b) => b)

  def swap[A, B](tuple: STuple[A, B]): STuple[B, A] = tuple[STuple[B, A]](
    (a, b) => new STuple[B, A] {
      override def apply[C] = f => f(b, a)
    }
  )

  def curry[A, B, C](f: STuple[A, B] => C): A => B => C =
    a => b => f(new STuple[A, B] {
      override def apply[C] = g => g(a, b)
    })

  def uncurry[A, B, C](f: A => B => C): STuple[A, B] => C =
    tuple => f(fst(tuple))(snd(tuple))

  def toOption[A](option: SOption[A]): Option[A] =
    option[Option[A]](None, Some(_))

  def fromOption[A](option: Option[A]): SOption[A] = new SOption[A] {
    override def apply[B] = option match {
      case None => (z, _) => z
      case Some(a) => (_, f) => f(a)
    }
  }

  def isSome(option: SOption[_]): Boolean = option[Boolean](false, _ => true)

  def isNone(option: SOption[_]): Boolean = option[Boolean](true, _ => false)

  def catOptions[A](list: SList[SOption[A]]): SList[A] = {
    list[SList[A]](
      nil,
      (option, tail) => {
        val newTail = catOptions(tail)

        option[SList[A]](newTail, cons(_, newTail))
      }
    )
  }

  def toEither[A, B](either: SEither[A, B]): Either[A, B] =
    either[Either[A, B]](Left(_), Right(_))

  def fromEither[A, B](either: Either[A, B]): SEither[A, B] = new SEither[A, B] {
    override def apply[C] = either match {
      case Left(a) => (left, _) => left(a)
      case Right(b) => (_, right) => right(b)
    }
  }

  def isLeft[A](either: SEither[A, _]): Boolean = either[Boolean](_ => true, _ => false)

  def isRight[A](either: SEither[A, _]): Boolean = either[Boolean](_ => false, _ => true)

  def nil[A]: SList[A] = new SList[A] {
    override def apply[B] = (z, _) => z
  }

  def toList[A](list: SList[A]): List[A] = {
    list[List[A]](List(), _ :: toList(_))
  }

  def fromList[A](list: List[A]): SList[A] = new SList[A] {
    override def apply[B] = list match {
      case List() => (z, _) => z
      case head :: tail => (_, f) => f(head, fromList(tail))
    }
  }

  def cons[A](head: A, list: SList[A]): SList[A] = new SList[A] {
    override def apply[B] = (_, f) => f(head, list)
  }

  def concat[A](left: SList[A], right: SList[A]): SList[A] = fromList(toList(left) ++ toList(right))

  def empty(list: SList[_]): Boolean = list[Boolean](true, (_, _) => false)

  def length(list: SList[_]): Int = list[Int](0, (_, t) => 1 + length(t))

  def map[A, B](f: (A => B), list: SList[A]): SList[B] =
    list[SList[B]](
      nil[B],
      (h, t) => cons(f(h), map(f, t))
    )

  def zip[A, B](listA: SList[A], listB: SList[B]): SList[STuple[A, B]] = {
    listA[SList[STuple[A, B]]](
      nil,
      (ha, ta) => listB[SList[STuple[A, B]]](
        nil[STuple[A, B]],
        (hb, tb) => cons(fromTuple(ha, hb), zip(ta, tb))
      )
    )
  }

  def foldLeft[A, B](f: ((B, A) => B), z: B, list: SList[A]): B =
    list[B](
      z,
      (h, t) => foldLeft(f, f(z, h), t)
    )

  def foldRight[A, B](f: ((A, B) => B), z: B, list: SList[A]): B =
    list[B](
      z,
      (h, t) => f(h, foldRight(f, z, t))
    )

  def take[A](n: Int, list: SList[A]): SList[A] = {
    println(n)
    list[SList[A]](nil, (h, t) => cons(h, take(n - 1, t)))
  }

  //type(acc) = STuple[SList[A], SList[B]]
  def partition[A, B](list: SList[SEither[A, B]]): STuple[SList[A], SList[B]] = foldRight[SEither[A, B], STuple[SList[A], SList[B]]](
    (either, acc) => either[STuple[SList[A], SList[B]]](
      left => acc[STuple[SList[A], SList[B]]]((listA, listB) => fromTuple(cons(left, listA), listB)),
      right => acc[STuple[SList[A], SList[B]]]((listA, listB) => fromTuple(listA, cons(right, listB)))
    ),
    fromTuple(fromList(List()), fromList(List())),
    list
  )

  //********************************************Temp***************************************

  object Scott1 {
    trait SC0 {
      def apply(f: () => String): String
    }

    trait SC1 {
      def apply(f: (String) => String): String
    }

    trait SC2 {
      def apply(f: (String, String) => String): String
    }

    case class SCS(sc0: SC0, sc1: SC1, sc2: SC2) {
      def apply(f: () => String) = sc0(f)
      def apply(f: (String) => String) = sc1(f)
      def apply(f: (String, String) => String) = sc2(f)
    }
  }
}
