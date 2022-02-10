package rc

import rc.RcAlgebra.Event

//2. aug. 2021
object RcAlgebra {
  //*************** main data-types ***************

  trait Event {
    //*************** operations ***************
    def toList(): List[Event]

    //joins this with other if they contain members of cross groups in linked
    def joinWith(other: Event)(linked: Linked): Event = {
      val thisSet: Set[Event] = this.toList().toSet
      val otherSet: Set[Event] = other.toList().toSet

      val leftIntersection = thisSet.intersect(linked.l)
      val rightIntersection = otherSet.intersect(linked.r)

      if (leftIntersection.nonEmpty && rightIntersection.nonEmpty) {
        
      }
    }
  }
  case class Sport(id: String) extends Event {
    override def toList(): List[Event] = List(this)
  }
  case class Category(id: String, p: Sport) extends Event {
    def getSport: Sport = p

    override def toList(): List[Event] = this :: p.toList()
  }
  case class Tournament(id: String, p: Either[Tournament, Category]) extends Event {
    def getCategory: Category = p.fold(_.getCategory, identity)

    override def toList(): List[Event] = this :: p.fold(_.toList(), _ => List())
  }
  case class Match(id: String, p: Tournament) extends Event {
    def getTournament: Tournament = p

    override def toList(): List[Event] = this :: p.toList()
  }

  //*************** isomorphisms ***************

  case class Hierarchy(s: Sport, c: Category, t: Tournament, m: Match) {

  }

  def toHierarchy(m: Match): Hierarchy = {
    val t = m.getTournament
    val c = t.getCategory
    val s = c.getSport

    Hierarchy(s, c, t, m)
  }

  //*************** configuration data-types ***************

  case class Linked(l: Set[Event], r: Set[Event]) //aka related
  case class Combinable(l: Set[Event], r: Set[Event]) //aka unrelated

  //NOTE: Same event tuples shouldn't be both linked and combinable (user error)

  //*************** operations ***************

  //splits e0 from e1 or the other way around if they contain members of cross groups in combinable
  def split(e0: Event, e1: Event)(combinable: Combinable): (Event, Event) = ???
}
