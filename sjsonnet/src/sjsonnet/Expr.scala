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
sealed trait Expr{
  def offset: Int
  def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val
}
object Expr{
  case class Null(offset: Int) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitNull(this)
  }
  case class True(offset: Int) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitTrue(this)
  }
  case class False(offset: Int) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitFalse(this)
  }
  case class Self(offset: Int) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitSelf(this)
  }
  case class Super(offset: Int) extends Expr {
    def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ???
  }
  case class $(offset: Int) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitDollar(this)
  }

  case class Str(offset: Int, value: String) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitStr(this)
  }
  case class Num(offset: Int, value: Double) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitNum(this)
  }
  case class Id(offset: Int, value: Int) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitId(offset, value)
  }
  case class Arr(offset: Int, value: Seq[Expr]) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitArr(this)
  }
  case class Obj(offset: Int, value: ObjBody) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitObjBody(offset, value)
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
    case class Field(offset: Int,
                     fieldName: FieldName,
                     plus: Boolean,
                     args: Option[Params],
                     sep: Visibility,
                     rhs: Expr) extends Member
    case class BindStmt(value: Bind) extends Member
    case class AssertStmt(value: Expr, msg: Option[Expr]) extends Member
  }


  case class Parened(offset: Int, value: Expr) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitParened(this)
  }
  case class Params(args: IndexedSeq[(String, Option[Expr], Int)]){
    val argIndices: Map[String, Int] = args.map{case (k, d, i) => (k, i)}.toMap
    val noDefaultIndices: BitSet = {
      val b = new BitSet(args.size)
      args.collect {
        case (_, None, i) => b.set(i)
      }
      b
    }
    val defaults: IndexedSeq[(Int, Expr)] = args.collect{case (_, Some(x), i) => (i, x)}
    val allIndices: BitSet = {
      val b = new BitSet(args.size)
      args.foreach { case (_, _, i) => b.set(i) }
      b
    }
  }
  case class Args(args: Seq[(Option[String], Expr)])

  case class UnaryOp(offset: Int, op: UnaryOp.Op, value: Expr) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitUnaryOp(offset, op, value)
  }
  object UnaryOp{
    sealed trait Op
    case object `+` extends Op
    case object `-` extends Op
    case object `~` extends Op
    case object `!` extends Op
  }
  case class BinaryOp(offset: Int, lhs: Expr, op: BinaryOp.Op, rhs: Expr) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitBinaryOp(offset, lhs, op, rhs)
  }
  object BinaryOp{
    sealed trait Op
    case object `*` extends Op
    case object `/` extends Op
    case object `%` extends Op
    case object `+` extends Op
    case object `-` extends Op
    case object `<<` extends Op
    case object `>>` extends Op
    case object `<` extends Op
    case object `>` extends Op
    case object `<=` extends Op
    case object `>=` extends Op
    case object `in` extends Op
    case object `==` extends Op
    case object `!=` extends Op
    case object `&` extends Op
    case object `^` extends Op
    case object `|` extends Op
    case object `&&` extends Op
    case object `||` extends Op
  }
  case class AssertExpr(offset: Int, asserted: Member.AssertStmt, returned: Expr) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitAssert(offset, asserted.value, asserted.msg, returned)
  }
  case class LocalExpr(offset: Int, bindings: Seq[Bind], returned: Expr) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitLocalExpr(this)
  }

  case class Bind(offset: Int, name: Int, args: Option[Params], rhs: Expr)
  case class Import(offset: Int, value: String) extends Expr {
    def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitImport(offset, value)
  }
  case class ImportStr(offset: Int, value: String) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitImportStr(offset, value)
  }
  case class Error(offset: Int, value: Expr) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitError(offset, value)
  }
  case class Apply(offset: Int, value: Expr, args: Args) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitApply(offset, value, args.args)
  }
  case class Select(offset: Int, value: Expr, name: String) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitSelect(offset, value, name)
  }
  case class Lookup(offset: Int, value: Expr, index: Expr) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitLookup(offset, value, index)
  }
  case class Slice(offset: Int,
                   value: Expr,
                   start: Option[Expr],
                   end: Option[Expr],
                   stride: Option[Expr]) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitSlice(offset, value, start, end, stride)
  }
  case class Function(offset: Int, params: Params, body: Expr) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitMethod(body, params, offset)
  }
  case class IfElse(offset: Int, cond: Expr, `then`: Expr, `else`: Option[Expr]) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitIfElse(offset, cond, `then`, `else`)
  }

  sealed trait CompSpec
  case class IfSpec(offset: Int, cond: Expr) extends CompSpec
  case class ForSpec(offset: Int, name: Int, cond: Expr) extends CompSpec

  case class Comp(offset: Int, value: Expr, first: ForSpec, rest: Seq[CompSpec]) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitComp(this)
  }
  case class ObjExtend(offset: Int, base: Expr, ext: ObjBody) extends Expr {
    override def visit(ev: Evaluator)(implicit scope: ValScope, fileScope: FileScope): Val = ev.visitObjExtend(this)
  }

  sealed trait ObjBody
  object ObjBody{
    case class MemberList(value: Seq[Member]) extends ObjBody
    case class ObjComp(preLocals: Seq[Member.BindStmt],
                       key: Expr,
                       value: Expr,
                       postLocals: Seq[Member.BindStmt],
                       first: ForSpec,
                       rest: Seq[CompSpec]) extends ObjBody
  }

}
