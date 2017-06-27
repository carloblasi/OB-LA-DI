import scala.collection.mutable.ArrayBuffer

/**
  * Created by carloblasi on 11/05/17.
  */
object Compiler {

	def main(args: Array[String]): Unit = {

		val start = System.currentTimeMillis()

		val path: String = args(0)

		if (!path.endsWith(".obl"))
			Compiler.error("ERROR - FILE MUST BE .obl")

		val filename = path.substring(path.lastIndexOf("/") + 1, path.lastIndexOf(".obl"))

		val tokens: ArrayBuffer[Token] = new Lexer(path).lex()

		val AST = new Parser(tokens).parse().abstractNodes()

		val symbolTable = AST.contextualAnalysis()

		new CodeGenerator(filename).gen(AST, symbolTable)

		val totalTime = System.currentTimeMillis() - start
		println("[Compile time: %1d ms]".format(totalTime))
		println
	}

	def error(message: String): Unit = {

		println("\u001B[31m" + message + "\u001B[0m")
		System.exit(1)
	}
}
