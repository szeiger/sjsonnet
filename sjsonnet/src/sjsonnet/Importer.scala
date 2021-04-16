package sjsonnet

import scala.collection.mutable

import fastparse.Parsed

/** Resolve and read imported files */
abstract class Importer {
  def resolve(docBase: Path, importName: String): Option[Path]
  def read(path: Path): Option[String]

  def resolveAndRead(docBase: Path, importName: String): Option[(Path, String)] = for {
    path <- resolve(docBase, importName)
    txt <- read(path)
  } yield (path, txt)

  def resolveAndReadOrFail(value: String, pos: Position)(implicit ev: EvalErrorScope): (Path, String) =
    resolveAndRead(pos.fileScope.currentFile.parent(), value)
      .getOrElse(Error.fail("Couldn't import file: " + pprint.Util.literalize(value), pos))
}

object Importer {
  val empty: Importer = new Importer {
    def resolve(docBase: Path, importName: String): Option[Path] = None
    def read(path: Path): Option[String] = None
  }
}

class CachedImporter(parent: Importer) extends Importer {
  val cache = mutable.HashMap.empty[Path, String]

  def resolve(docBase: Path, importName: String): Option[Path] = parent.resolve(docBase, importName)

  def read(path: Path): Option[String] = cache.get(path) match {
    case s @ Some(x) =>
      if(x == null) None else s
    case None =>
      val x = parent.read(path)
      cache.put(path, x.getOrElse(null))
      x
  }
}

class CachedResolver(
  parentImporter: Importer,
  val parseCache: mutable.HashMap[(Path, String), Either[String, (Expr, FileScope)]] = new mutable.HashMap
) extends CachedImporter(parentImporter) {
  private val textCache = new mutable.HashMap[String, Either[String, (Expr, FileScope)]]

  def parse(path: Path, txt: String): Either[String, (Expr, FileScope)] = {
    parseCache.getOrElseUpdate((path, txt), {
      textCache.getOrElse(txt, null) match {
        case null =>
          val parsed = fastparse.parse(txt, new Parser(path).document(_)) match {
            case f @ Parsed.Failure(l, i, e) => Left(f.trace().msg)
            case Parsed.Success(r, index) => Right(r)
          }
          val r = parsed.flatMap { case (e, fs) => process(e, fs) }
          textCache.put(txt, r)
          r
        case cached =>
          cached.map { case (expr, fs) => reposition(expr, fs, path) }
      }
    })
  }

  def parseOrFail(pos: Position, pathStr: String, path: Path, txt: String)(implicit ev: EvalErrorScope): (Expr, FileScope) =
    parse(path, txt) match {
      case Right(x) => x
      case Left(msg) =>
        Error.fail("Imported file " + pprint.Util.literalize(pathStr) + " had Parse error. " + msg, pos)
    }

  private def reposition(expr: Expr, fs: FileScope, path: Path): (Expr, FileScope) = {
    //println(s"reposition($expr, $fs, $path)")
    val newFs = new FileScope(path, fs.nameIndices)
    val newExpr = (new ExprTransform {
      def transform(expr: Expr): Expr =
        expr.withPos(new Position(newFs, expr.pos.offset))
    }).transform(expr)
    (newExpr, newFs)
  }

  def process(expr: Expr, fs: FileScope): Either[String, (Expr, FileScope)] = Right((expr, fs))
}
