import java.io.File

/**
  * Created by carloblasi on 11/05/17.
  */
object Compiler {

	/**
	  * Compiles the OB-LA-DI program specified by the path.
	  * @param args the path to the file to compile.
	  */
	def main(args: Array[String]): Unit = {

		println("[Compile time: %1d ms]\n" format getCompileTime {

			val path = getPath(args(0))

			val filename = getFilename(path)

			val tokens = new Lexer(path).lex()

			val AST = new Parser(tokens).parse().abstractNodes()

			val symbolTable = AST.semanticAnalysis()

			new CodeGenerator(filename).gen(AST, symbolTable)
		})
	}

	/**
	  * Prints an error message and halts the execution.
	  * @param message the error message to print.
	  */
	def error(message: String): Unit = {

		println("\u001B[31m" + message + "\u001B[0m")
		System.exit(1)
	}

	/**
	  * Measures the time it takes to compile.
	  * @param compile the compiler instructions.
	  * @return the time of execution in milliseconds.
	  */
	def getCompileTime(compile: => Unit): Long = {

		val start = System.currentTimeMillis()
		compile
		System.currentTimeMillis() - start
	}

	/**
	  * Checks that the path is correct and returns it.
	  * @param path the path to check for correctness.
	  * @return the path.
	  */
	def getPath(path: String): String = {

		if (!path.endsWith(".obl"))
			Compiler.error("ERROR - FILE MUST BE .obl")

		if (!new File(path).exists)
			Compiler.error("ERROR - FILE NOT FOUND")

		path
	}

	/**
	  * Returns the name of the file specified in the path, without extension.
	  * @param path the path from which to get the filename.
	  * @return the filename.
	  */
	def getFilename(path: String): String = {
		path.substring(path.lastIndexOf("/") + 1, path.lastIndexOf(".obl"))
	}
}
