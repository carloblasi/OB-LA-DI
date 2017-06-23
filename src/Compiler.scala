import scala.collection.mutable.ArrayBuffer

/**
  * Created by carloblasi on 11/05/17.
  */
object Compiler {

	def main(args: Array[String]): Unit = {

		val start = System.currentTimeMillis()

		val path: String = args(0)

		val tokens: ArrayBuffer[Token] = new Lexer(path).lex()

		val AST = new Parser(tokens).parse().abstractNodes()

		val symbolTable = AST.contextualAnalysis()

		new CodeGenerator().gen(AST, symbolTable)

		val totalTime = System.currentTimeMillis() - start
		println("[Compile time: %1d ms]".format(totalTime))
		println
	}

	def error(message: String): Unit = {

		println(message)
		System.exit(1)
	}
}
