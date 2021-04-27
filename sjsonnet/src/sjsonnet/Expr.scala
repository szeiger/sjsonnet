package sjsonnet

import java.util.BitSet

import scala.collection.mutable

/**
  * [[Expr]]s are the parsed syntax trees of a Jsonnet program. They model the
  * program mostly as-written, except for resolving local variable names and
  * assigning them indices in the scope bindings array.
  *
  * Each [[Expr]] represents an expression in the Jsonnet program, and contains an
  * integer offset into the file that is later used to provide error messages.
  */
abstract class Expr{
  var eid: Int = Expr.EID_Val
  def pos: Position
}
object Expr{
  final val EID_Val = 0
  final val EID_ValidId = 1
  final val EID_BinaryOp = 2
  final val EID_Select = 3
  final val EID_Error = 4
  final val EID_ApplyBuiltin1 = 5
  final val EID_ApplyBuiltin2 = 6
  final val EID_And = 7
  final val EID_Or = 8
  final val EID_UnaryOp = 9
  final val EID_Apply1 = 10
  final val EID_Lookup = 11
  final val EID_Function = 12
  final val EID_LocalExpr = 13
  final val EID_Apply = 14
  final val EID_IfElse = 15
  final val EID_Apply3 = 16
  final val EID_MemberList = 17
  final val EID_Apply2 = 18
  final val EID_AssertExpr = 19
  final val EID_ApplyBuiltin = 20
  final val EID_Comp = 21
  final val EID_Arr = 22
  final val EID_SelectSuper = 23
  final val EID_LookupSuper = 24
  final val EID_InSuper = 25
  final val EID_ObjExtend = 26
  final val EID_ObjComp = 27
  final val EID_Slice = 28
  final val EID_Import = 29
  final val EID_Apply0 = 30
  final val EID_ImportStr = 31

  final val EID_Id = 32
  final val EID_Self = 33
  final val EID_$ = 34
  final val EID_Super = 35

  private final def arrStr(a: Array[_]): String = {
    if(a == null) "null" else a.mkString("[", ", ", "]")
  }

  case class Self(pos: Position) extends Expr {
    this.eid = EID_Self
  }
  case class Super(pos: Position) extends Expr {
    this.eid = EID_Super
  }
  case class $(pos: Position) extends Expr {
    this.eid = EID_$
  }

  case class Id(pos: Position, name: String) extends Expr {
    this.eid = EID_Id
  }
  case class ValidId(pos: Position, name: String, nameIdx: Int) extends Expr {
    this.eid = EID_ValidId
  }
  case class Arr(pos: Position, value: Array[Expr]) extends Expr {
    this.eid = EID_Arr
    override def toString = s"Arr($pos, ${arrStr(value)})"
  }

  sealed trait FieldName

  object FieldName{
    case class Fixed(value: String) extends FieldName
    case class Dyn(expr: Expr) extends FieldName
  }
  sealed trait Member

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
    }
    case class AssertStmt(value: Expr, msg: Expr) extends Member
  }

  case class Params(names: Array[String], defaultExprs: Array[Expr]){
    val paramMap = names.zipWithIndex.toMap
    override def toString = s"Params(${arrStr(names)}, ${arrStr(defaultExprs)})"
  }

  case class UnaryOp(pos: Position, op: Int, value: Expr) extends Expr {
    this.eid = EID_UnaryOp
  }
  object UnaryOp{
    final val OP_! = 0
    final val OP_- = 1
    final val OP_~ = 2
    final val OP_+ = 3
    private val names = Map(OP_! -> "!", OP_- -> "-", OP_~ -> "~", OP_+ -> "+")
    def name(op: Int): String = names.getOrElse(op, "<unknown>")
  }
  case class And(pos: Position, lhs: Expr, rhs: Expr) extends Expr {
    this.eid = EID_And
  }
  case class Or(pos: Position, lhs: Expr, rhs: Expr) extends Expr {
    this.eid = EID_Or
  }
  case class BinaryOp(pos: Position, lhs: Expr, op: Int, rhs: Expr) extends Expr {
    this.eid = EID_BinaryOp
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
    this.eid = EID_AssertExpr
  }
  case class LocalExpr(pos: Position, bindings: Array[Bind], returned: Expr) extends Expr {
    this.eid = EID_LocalExpr
    override def toString = s"LocalExpr($pos, ${arrStr(bindings)}, $returned)"
  }

  case class Bind(pos: Position, name: String, args: Params, rhs: Expr) extends Member
  case class Import(pos: Position, value: String) extends Expr {
    this.eid = EID_Import
  }
  case class ImportStr(pos: Position, value: String) extends Expr {
    this.eid = EID_ImportStr
  }
  case class Error(pos: Position, value: Expr) extends Expr {
    this.eid = EID_Error
  }
  case class Apply(pos: Position, value: Expr, args: Array[Expr], namedNames: Array[String]) extends Expr {
    this.eid = EID_Apply
  }
  case class Apply0(pos: Position, value: Expr) extends Expr {
    this.eid = EID_Apply0
  }
  case class Apply1(pos: Position, value: Expr, a1: Expr) extends Expr {
    this.eid = EID_Apply1
  }
  case class Apply2(pos: Position, value: Expr, a1: Expr, a2: Expr) extends Expr {
    this.eid = EID_Apply2
  }
  case class Apply3(pos: Position, value: Expr, a1: Expr, a2: Expr, a3: Expr) extends Expr {
    this.eid = EID_Apply3
  }
  case class ApplyBuiltin(pos: Position, func: Val.Builtin, argExprs: Array[Expr]) extends Expr {
    this.eid = EID_ApplyBuiltin
  }
  case class ApplyBuiltin1(pos: Position, func: Val.Builtin1, a1: Expr) extends Expr {
    this.eid = EID_ApplyBuiltin1
  }
  case class ApplyBuiltin2(pos: Position, func: Val.Builtin2, a1: Expr, a2: Expr) extends Expr {
    this.eid = EID_ApplyBuiltin2
  }
  case class Select(pos: Position, value: Expr, name: String) extends Expr {
    this.eid = EID_Select
  }
  case class SelectSuper(pos: Position, selfIdx: Int, name: String) extends Expr {
    this.eid = EID_SelectSuper
  }
  case class InSuper(pos: Position, value: Expr, selfIdx: Int) extends Expr {
    this.eid = EID_InSuper
  }
  case class Lookup(pos: Position, value: Expr, index: Expr) extends Expr {
    this.eid = EID_Lookup
  }
  case class LookupSuper(pos: Position, selfIdx: Int, index: Expr) extends Expr {
    this.eid = EID_LookupSuper
  }
  case class Slice(pos: Position,
                   value: Expr,
                   start: Option[Expr],
                   end: Option[Expr],
                   stride: Option[Expr]) extends Expr {
    this.eid = EID_Slice
  }
  case class Function(pos: Position, params: Params, body: Expr) extends Expr {
    this.eid = EID_Function
  }
  case class IfElse(pos: Position, cond: Expr, then: Expr, `else`: Expr) extends Expr {
    this.eid = EID_IfElse
  }

  sealed trait CompSpec extends Expr
  case class IfSpec(pos: Position, cond: Expr) extends CompSpec
  case class ForSpec(pos: Position, name: String, cond: Expr) extends CompSpec

  case class Comp(pos: Position, value: Expr, first: ForSpec, rest: Array[CompSpec]) extends Expr {
    this.eid = EID_Comp
  }
  case class ObjExtend(pos: Position, base: Expr, ext: ObjBody) extends Expr {
    this.eid = EID_ObjExtend
  }

  trait ObjBody extends Expr
  object ObjBody{
    case class MemberList(pos: Position, binds: Array[Bind], fields: Array[Member.Field], asserts: Array[Member.AssertStmt]) extends ObjBody {
      this.eid = EID_MemberList
    }
    case class ObjComp(pos: Position,
                       preLocals: Array[Bind],
                       key: Expr,
                       value: Expr,
                       postLocals: Array[Bind],
                       first: ForSpec,
                       rest: List[CompSpec]) extends ObjBody {
      this.eid = EID_ObjComp
      override def toString = s"ObjComp($pos, ${arrStr(preLocals)}, $key, $value, ${arrStr(postLocals)}, $first, $rest)"
    }
  }

}
