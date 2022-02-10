package codewars.fp

//complexity = 3 kyu
object Isomorphism {
  /**
   * The type [[Nothing]] has no value.
   * So it is impossible to construct an instance of it.
   * In this solution, wherever a situation arises where
   * for types to check, you need a function that takes a [[Nothing]],
   * you can use [[absurd]].
   */
  def absurd[R](n: Nothing): R = asInstanceOf[R] //defacto type R with value null

  // so, when are two type, `A` and `B`, considered equal?
  // a definition might be, it is possible to go from `A` to `B`,
  // and from `B` to `A`.
  // Going a roundway trip should leave you the same value.
  // Unfortunately it is virtually impossible to test this in Scala.
  // This is called Isomorphism.

  type ISO[A, B] = (A => B, B => A)

  // given ISO a b, we can go from a to b
  def substL[A, B]: ISO[A, B] => (A => B) = iso => iso._1 //_._1

  // and vice versa
  def substR[A, B]: ISO[A, B] => (B => A) = _._2

  // There can be more than one ISO a b
  def isoBool: ISO[Boolean, Boolean] = (identity, identity)
  def isoBoolNot: ISO[Boolean, Boolean] = (! _, ! _) //!!x == x

  // isomorphism is reflexive
  def refl[A]: ISO[A, A] = (identity, identity)

  // isomorphism is symmetric
  def symm[A, B]: ISO[A, B] => ISO[B, A] = {
    case (ab, ba) => (ba, ab)
  }

  // isomorphism is transitive
  def trans[A, B, C]: (ISO[A, B], ISO[B, C]) => ISO[A, C] = {
    case ((ab, ba), (bc, cb)) => (
      bc compose ab,
      ba compose cb
    )
  }

  // We can combine isomorphism:
  def isoTuple[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[(A, C), (B, D)] = {
    case ((ab, ba), (cd, dc)) => (
      { case (a, c) => (ab(a), cd(c)) },
      { case (b, d) => (ba(b), dc(d)) }
    )
  }

  def isoList[A, B]: ISO[A, B] => ISO[List[A], List[B]] = {
    case (ab, ba) => (_ map ab, _ map ba)
  }

  def isoEither[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[Either[A, C], Either[B, D]] = {
    case ((ab, ba), (cd, dc)) => (
      _.fold(a => Left(ab(a)), c => Right(cd(c))),
      _.fold(b => Left(ba(b)), d => Right(dc(d)))
    )
  }

  def isoFunc[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[(A => C), (B => D)] = {
    case ((ab, ba), (cd, dc)) => (
      (ac: A => C) => cd compose ac compose ba, //(A => C) => (B => D)
      (bd: B => D) => dc compose bd compose ab //(B => D) => (A => C)
    )
  }

  def isoOption[A, B]: ISO[A, B] => ISO[Option[A], Option[B]] = {
    case (ab, ba) => (oa => oa.map(ab), _ map ba)
  }

  //--------------------------------------------------------------------------------------

  //statefull solution isn't correct because it's not ref. transparent
  // Going another way is hard (and is generally impossible)
  def isoUnOption[A, B]: ISO[Option[A], Option[B]] => ISO[A, B] = {
    case (fOpt, gOpt) => {
      if (Boolean.isInstanceOf[A]) {
        val fBoolOpt = fOpt.asInstanceOf[Option[Boolean] => Option[Boolean]]
        val gBoolOpt = gOpt.asInstanceOf[Option[Boolean] => Option[Boolean]]

        isoUnOptionBool((fBoolOpt, gBoolOpt)).asInstanceOf[ISO[A, B]]
      } else
        throw new NotImplementedError
    }
  }
  // Remember, for all valid ISO, converting and converting back
  // Is the same as the original value.
  // You need this to prove some case are impossible.
  def isoUnOptionBool: ISO[Option[Boolean], Option[Boolean]] => ISO[Boolean, Boolean] = {
    case (fOpt, gOpt) => {
      if (fOpt(None) == None) {
        //special case (None-to-None mapping)
        val f = (x: Boolean) => fOpt(Some(x)).get //Bool => Bool

        (f, f)
      } else {
        //there exists a Some that is mapped to None (only 2 cases possible)
        //=> None can't be ignored!
        val hfs = new HFS((fOpt, gOpt))

        hfs.getIso
      }
    }
  }

  //Assumes None-to-None mapping does not exist
  class HFS(iso: ISO[Option[Boolean], Option[Boolean]]) {
    var a: Boolean = null.asInstanceOf[Boolean]
    var b: Boolean = null.asInstanceOf[Boolean]

    def getIso: ISO[Boolean, Boolean] = (
      a => {
        if (isVirgin()) {
          this.a = a
          iso._1(None).get
        } else if (this.a == a) {
          iso._1(None).get
        } else {
          iso._1(Some(a)).getOrElse(b)
        }
      },
      b => {
        if (isVirgin()) {
          this.b = b
          iso._2(None).get
        } else if (this.b == b) {
          iso._2(None).get
        } else {
          iso._2(Some(b)).getOrElse(a)
        }
      }
    )

    private[this] def isVirgin(): Boolean = a == null && b == null
  }

  //--------------------------------------------------------------------------------------

  // We cannot have
  // isoUnEither[A, B, C, D]: (ISO[Either[A, B], Either[C, D]], ISO[A, C]) => ISO[B, D]
  // Note that we have
  val eu: Eu = new Eu

  def isoEU: ISO[Either[List[Unit], Unit], Either[List[Unit], Nothing]] = (
    _.fold(
      list => { Left(()::list) },
      u => { Left(List()) }
    ),
    _.fold(
      list => if (list.isEmpty) Right(()) else Left(list.tail),
      absurd
    )
  )
  // where Unit, has 1 value, (the value is also called Unit), and Void has 0 values.
  // If we have isoUnEither,
  // We have ISO[Unit, Nothing] by calling isoUnEither isoEU
  // That is impossible, since we can get a Nothing by substL on ISO[Unit, Nothing]
  // So it is impossible to have isoUnEither

  class Eu {
    var isRegular = true

    def isoEU: ISO[Either[List[Unit], Unit], Either[List[Unit], Nothing]] = (
      _.fold(
        x => { isRegular = true; Left(x) },
        u => { isRegular = false; Left(List()) }
      ),
      _.fold(
        x => if (isRegular && x.nonEmpty) Left(x) else Right(()),
        absurd
      )
    )
  }

  // And we have isomorphism on isomorphism!
  def isoSymm[A, B]: ISO[ISO[A, B], ISO[B, A]] = (symm, symm)
}