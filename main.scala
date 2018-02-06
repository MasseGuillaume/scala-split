import scala.meta._
import java.nio.file._ 
import System.{lineSeparator => nl}

object Main {
  def main(args: Array[String]): Unit = {
    val code = new String(Files.readAllBytes(Paths.get("in/input.scala")))
    val parsed = code.parse[Source].get

    val Pkg(top, stats) = parsed.children.head

    val (imports, rest) = 
      stats.partition {
        case imp: Import => true
        case _ => false
      }

    val (pkgs, rest2) =
      rest.partition {
        case _: Pkg => true
        case _ => false
      }

    rest2.groupBy{
      case c: Defn.Class => c.name
      case t: Defn.Trait => t.name
      case o: Defn.Object => o.name
      case _ => ???
    }

    val base = Paths.get("output.scala")

    def doIt(pkgsName: List[Term.Ref], xs: List[Tree]): Unit = {
      val byName = 
        xs.groupBy {
          case c: Defn.Class => c.name.syntax
          case t: Defn.Trait => t.name.syntax
          case o: Defn.Object => o.name.syntax
          case _ => ???
        }

      val dir = pkgsName.foldLeft(base)((acc, x) => acc.resolve(x.syntax))
      Files.createDirectories(dir)

      val pkgHead = "package " + pkgsName.map(_.syntax).mkString(".")

      byName.foreach{
        case (name, vs) => {
          val outFile = dir.resolve(name + ".scala")
          val output =
            pkgHead + nl +
            imports.map(_.syntax).mkString(nl) + nl + nl +
            vs.map(_.syntax).mkString(nl)

          println(outFile)

          Files.write(outFile, output.getBytes("utf-8"), StandardOpenOption.CREATE)
        }
      }
    }

    doIt(List(top), rest2)
    pkgs.foreach{
      case Pkg(name, stats) => doIt(List(top, name), stats)
    }
  }
}


