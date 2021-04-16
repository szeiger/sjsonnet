package sjsonnet

import java.util.BitSet

/**
  * [[Expr]]s are the parsed syntax trees of a Jsonnet program. They model the
  * program mostly as-written, except for resolving local variable names and
  * assigning them indices in the scope bindings array.
  *
  * Each [[Expr]] represents an expression in the Jsonnet program, and contains an
  * integer offset into the file that is later used to provide error messages.
  */
trait Expr{
  def pos: Position
  def withPos(pos: Position): Expr
}
object Expr{
  private final def arrStr(a: Array[_]): String = {
    if(a == null) "null" else a.mkString("[", ", ", "]")
  }

  case class Self(pos: Position) extends Expr { def withPos(p: Position) = new Self(p) }
  case class Super(pos: Position) extends Expr { def withPos(p: Position) = new Super(p) }
  case class $(pos: Position) extends Expr { def withPos(p: Position) = new $(p) }

  case class Id(pos: Position, value: Int) extends Expr { def withPos(p: Position) = copy(p) }
  case class Arr(pos: Position, value: Array[Expr]) extends Expr {
    def withPos(p: Position) = copy(pos = p)
    override def toString = s"Arr($pos, ${arrStr(value)})"
  }

  sealed trait FieldName

  object FieldName{
    case class Fixed(value: String) extends FieldName
    case class Dyn(expr: Expr) extends FieldName
  }
  sealed trait Member {
    def withPos(pos: Position): Member
  }

  object Member{
    sealed trait Visibility
    object Visibility{

      case object Normal extends Visibility
      case object Hidden extends Visibility
      case object Unhide extends Visibility
    }
    case class Field(pos: Position,
                     fieldName: FieldName,
                     plus: Boolean,
                     args: Params,
                     sep: Visibility,
                     rhs: Expr) extends Member {
      def isStatic = fieldName.isInstanceOf[FieldName.Fixed] && !plus && args == null && sep == Visibility.Normal && rhs.isInstanceOf[Val.Literal]
      def withPos(p: Position) = copy(pos = p)
    }
    case class AssertStmt(value: Expr, msg: Expr) extends Member {
      def withPos(p: Position) = this
    }
  }

  case class Params(names: Array[String], defaultExprs: Array[Expr], indices: Array[Int]){
    val argIndices: Map[String, Int] = (names, indices).zipped.toMap
    val noDefaultIndices: BitSet = {
      val b = new BitSet(defaultExprs.size)
      (defaultExprs, indices).zipped.foreach((e, i) => if(e == null) b.set(i))
      b
    }
    val defaultsOnlyIndices: Array[Int] = (defaultExprs, indices).zipped.collect { case (x, i) if x != null => i }.toArray
    val defaultsOnly: Array[Expr] = defaultExprs.filter(_ != null).toArray
    val allIndices: BitSet = {
      val b = new BitSet(indices.size)
      indices.foreach(b.set)
      b
    }
    override def toString = s"Params(${arrStr(names)}, ${arrStr(defaultExprs)}, ${arrStr(indices)})"
  }
  case class Args(names: Array[String], exprs: Array[Expr])

  case class UnaryOp(pos: Position, op: Int, value: Expr) extends Expr {
    def withPos(p: Position) = copy(pos = p)
  }
  object UnaryOp{
    final val OP_! = 0
    final val OP_- = 1
    final val OP_~ = 2
    final val OP_+ = 3
    private val names = Map(OP_! -> "!", OP_- -> "-", OP_~ -> "~", OP_+ -> "+")
    def name(op: Int): String = names.getOrElse(op, "<unknown>")
  }
  case class BinaryOp(pos: Position, lhs: Expr, op: Int, rhs: Expr) extends Expr {
    def withPos(p: Position) = copy(pos = p)
  }
  object BinaryOp{
    final val OP_* = 0
    final val OP_/ = 1
    final val OP_% = 2
    final val OP_+ = 3
    final val OP_- = 4
    final val OP_<< = 5
    final val OP_>> = 6
    final val OP_< = 7
    final val OP_> = 8
    final val OP_<= = 9
    final val OP_>= = 10
    final val OP_in = 11
    final val OP_== = 12
    final val OP_!= = 13
    final val OP_& = 14
    final val OP_^ = 15
    final val OP_| = 16
    final val OP_&& = 17
    final val OP_|| = 18
    private val names = Map(OP_* -> "*", OP_/ -> "/", OP_% -> "%", OP_+ -> "+", OP_- -> "-", OP_<< -> "<<",
      OP_>> -> ">>", OP_< -> "<", OP_> -> ">", OP_<= -> "<=", OP_>= -> ">=", OP_in -> "in", OP_== -> "==",
      OP_!= -> "!=", OP_& -> "&", OP_^ -> "^", OP_| -> "|", OP_&& -> "&&", OP_|| -> "||" )
    def name(op: Int): String = names.getOrElse(op, "<unknown>")
  }
  case class AssertExpr(pos: Position, asserted: Member.AssertStmt, returned: Expr) extends Expr {
    def withPos(p: Position) = copy(pos = p)
  }
  case class LocalExpr(pos: Position, bindings: Array[Bind], returned: Expr) extends Expr {
    def withPos(p: Position) = copy(pos = p)
    override def toString = s"LocalExpr($pos, ${arrStr(bindings)}, $returned)"
  }

  case class Bind(pos: Position, name: Int, args: Params, rhs: Expr) extends Member {
    def withPos(p: Position) = copy(pos = p)
  }
  case class Import(pos: Position, value: String) extends Expr {
    def withPos(p: Position) = copy(pos = p)
  }
  case class ImportStr(pos: Position, value: String) extends Expr {
    def withPos(p: Position) = copy(pos = p)
  }
  case class Error(pos: Position, value: Expr) extends Expr {
    def withPos(p: Position) = copy(pos = p)
  }
  case class Apply(pos: Position, value: Expr, argNames: Array[String], argExprs: Array[Expr]) extends Expr {
    def withPos(p: Position) = copy(pos = p)
  }
  case class ApplyBuiltin(pos: Position, func: Val.Builtin, argExprs: Array[Expr]) extends Expr {
    def withPos(p: Position) = copy(pos = p)
  }
  case class ApplyBuiltin1(pos: Position, func: Val.Builtin1, a1: Expr) extends Expr {
    def withPos(p: Position) = copy(pos = p)
  }
  case class ApplyBuiltin2(pos: Position, func: Val.Builtin2, a1: Expr, a2: Expr) extends Expr {
    def withPos(p: Position) = copy(pos = p)
  }
  case class Select(pos: Position, value: Expr, name: String) extends Expr {
    def withPos(p: Position) = copy(pos = p)
  }
  case class Lookup(pos: Position, value: Expr, index: Expr) extends Expr {
    def withPos(p: Position) = copy(pos = p)
  }
  case class Slice(pos: Position, value: Expr, start: Option[Expr],
                   end: Option[Expr], stride: Option[Expr]) extends Expr {
    def withPos(p: Position) = copy(pos = p)
  }
  case class Function(pos: Position, params: Params, body: Expr) extends Expr {
    def withPos(p: Position) = copy(pos = p)
  }
  case class IfElse(pos: Position, cond: Expr, then: Expr, `else`: Expr) extends Expr {
    def withPos(p: Position) = copy(pos = p)
  }

  sealed trait CompSpec extends Expr
  case class IfSpec(pos: Position, cond: Expr) extends CompSpec {
    def withPos(p: Position) = copy(pos = p)
  }
  case class ForSpec(pos: Position, name: Int, cond: Expr) extends CompSpec {
    def withPos(p: Position) = copy(pos = p)
  }

  case class Comp(pos: Position, value: Expr, first: ForSpec, rest: Array[CompSpec]) extends Expr {
    def withPos(p: Position) = copy(pos = p)
  }
  case class ObjExtend(pos: Position, base: Expr, ext: ObjBody) extends Expr {
    def withPos(p: Position) = copy(pos = p)
  }

  trait ObjBody extends Expr
  object ObjBody{
    case class MemberList(pos: Position, binds: Array[Bind], fields: Array[Member.Field], asserts: Array[Member.AssertStmt]) extends ObjBody {
      def withPos(p: Position) = copy(pos = p)
    }
    case class ObjComp(pos: Position,
                       preLocals: Array[Bind],
                       key: Expr,
                       value: Expr,
                       postLocals: Array[Bind],
                       first: ForSpec,
                       rest: List[CompSpec]) extends ObjBody {
      def withPos(p: Position) = copy(pos = p)
      override def toString = s"ObjComp($pos, ${arrStr(preLocals)}, $key, $value, ${arrStr(postLocals)}, $first, $rest)"
    }
  }

}
