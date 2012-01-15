package scala.tools.nsc
package typechecker

import symtab.Flags._
import scala.tools.nsc.util._
import scala.reflect.ReflectionUtils

trait Macros { self: Analyzer =>
  import global._
  import definitions._

  def macroMeth(mac: Symbol): Symbol = {
    var owner = mac.owner
    if (!owner.isModuleClass) owner = owner.companionModule.moduleClass
    owner.info.decl(nme.macroMethodName(mac.name))
  }

  def macroArgs(tree: Tree): (List[List[Tree]]) = tree match {
    case Apply(fn, args) =>
      macroArgs(fn) :+ args
    case TypeApply(fn, args) =>
      macroArgs(fn) :+ args
    case Select(qual, name) if !isStaticMacro(tree.symbol) =>
      List(List(qual))
    case _ =>
      List(List())
  }

  private def isStaticMacro(mac: Symbol): Boolean =
    mac.owner.isModuleClass

  /**
   * The definition of the method implementing a macro. Example:
   *  Say we have in a class C
   *
   *    def macro foo[T](xs: List[T]): T = expr
   *
   *  Then the following macro method is generated for `foo`:
   *
   *    def defmacro$foo(glob: scala.reflect.api.Universe)
   *           (_this: glob.Tree)
   *           (T: glob.Type)
   *           (xs: glob.Tree): glob.Tree = {
   *      implicit val $glob = glob
   *      expr
   *    }
   *
   *    If `foo` is declared in an object, the second parameter list is () instead of (_this: glob.Tree).
   */
  def macroMethDef(mdef: DefDef): Tree = {
    def paramDef(name: Name, tpt: Tree) = ValDef(Modifiers(PARAM), name, tpt, EmptyTree)
    val universeType = TypeTree(ReflectApiUniverse.tpe)
    val globParamSec = List(paramDef(nme.glob, universeType))
    def globSelect(name: Name) = Select(Ident(nme.glob), name)
    def globTree = globSelect(newTypeName("Tree"))
    def globType = globSelect(newTypeName("Type"))
    val thisParamSec = if (isStaticMacro(mdef.symbol)) List() else List(paramDef(newTermName("_this"), globTree))
    def tparamInMacro(tdef: TypeDef) = paramDef(tdef.name.toTermName, globType)
    def vparamInMacro(vdef: ValDef): ValDef = paramDef(vdef.name, vdef.tpt match {
      case tpt @ AppliedTypeTree(hk, _) if treeInfo.isRepeatedParamType(tpt) => AppliedTypeTree(hk, List(globTree))
      case _ => globTree
    })
    def wrapImplicit(tree: Tree) = atPos(tree.pos) {
      val implicitDecl = ValDef(Modifiers(IMPLICIT), nme.globImplicit, SingletonTypeTree(Ident(nme.glob)), Ident(nme.glob))
      val importGlob = Import(Ident(nme.globImplicit), List(ImportSelector(nme.WILDCARD, -1, null, -1)))
      Block(List(implicitDecl, importGlob), tree)
    }
    var formals = (mdef.vparamss map (_ map vparamInMacro))
    if (mdef.tparams.nonEmpty) formals = (mdef.tparams map tparamInMacro) :: formals

    atPos(mdef.pos) {
      new DefDef( // can't call DefDef here; need to find out why
        mods = mdef.mods &~ MACRO,
        name = nme.macroMethodName(mdef.name),
        tparams = List(),
        vparamss = globParamSec :: thisParamSec :: formals,
        tpt = globTree,
        wrapImplicit(mdef.rhs))
    }
  }

  def addMacroMethods(templ: Template, namer: Namer): Unit = {
    for (ddef @ DefDef(mods, _, _, _, _, _) <- templ.body if mods hasFlag MACRO) {
      val trace = scala.tools.nsc.util.trace when settings.debug.value
      val sym = namer.enterSyntheticSym(trace("macro def: ")(macroMethDef(ddef)))
      trace("added to "+namer.context.owner.enclClass+": ")(sym)
    }
  }

  lazy val mirror = new scala.reflect.runtime.Mirror {
    lazy val libraryClassLoader = {
      val classpath = global.classPath.asURLs
      ScalaClassLoader.fromURLs(classpath, self.getClass.getClassLoader)
    }

    override def defaultReflectiveClassLoader() = libraryClassLoader
  }

  case class MacroExpandError(msg: String) extends Exception(msg)

  /** Return address of companion object and implementation method symbol
   *  of given macro.
   */
  def macroImpl(mac: Symbol): (AnyRef, mirror.Symbol) = {
    def notFound() = throw MacroExpandError("macro implementation not found: " + mac.name)
    try {
      val mmeth = macroMeth(mac)
      if (mmeth == NoSymbol) notFound()

        val receiverClass: mirror.Symbol = mirror.classWithName(mmeth.owner.fullName)
        val receiverObj = receiverClass.companionModule
        if (receiverObj == NoSymbol) notFound()

        val receiver = mirror.getCompanionObject(receiverClass)
        val rmeth = receiverObj.info.member(mirror.newTermName(mmeth.name.toString))
        (receiver, rmeth)
    } catch {
      case ex: ClassNotFoundException =>
        notFound()
    }
  }

  def macroExpand(tree: Tree): Any = {
    val (receiver, rmeth) = macroImpl(tree.symbol)

    val argss = List(global) :: macroArgs(tree)
    val paramss = macroMeth(tree.symbol).paramss
    val rawArgss = for ((as, ps) <- argss zip paramss) yield {
      if (isVarArgsList(ps)) as.take(ps.length - 1) :+ as.drop(ps.length - 1)
      else as
    }
    val rawArgs: Seq[Any] = rawArgss.flatten

    try {
      mirror.invoke(receiver, rmeth, rawArgs: _*)
    } catch {
      case ex =>
        val realex = ReflectionUtils.unwrapThrowable(ex)
        val stacktrace = new java.io.StringWriter()
        realex.printStackTrace(new java.io.PrintWriter(stacktrace))
        val msg = System.getProperty("line.separator") + stacktrace
        throw MacroExpandError("exception during macro expansion: " + msg)
    }
  }
}
