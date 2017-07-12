import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by carloblasi on 02/05/17.
  */
class Lexer(path: String) {

	val source = Source.fromFile(path, "UTF-8").toArray

	/**
	  * Splits the source code in tokens and fills the tokens Array with them.
	  * It checks the lexical correctness of the code.
	  * Remade in pure functional style.
	  *
	  * @param source the char array from which to read the source code.
	  * @param tokens the token array to fill.
	  * @param i the index to read from the source.
	  * @param token the temporary variable which stores a block of characters.
	  * @return the array of tokens.
	  */
	@tailrec
	final def lex(source: Array[Char] = source, tokens: ArrayBuffer[Token] = ArrayBuffer[Token](), i: Int = 0, token: String = ""): ArrayBuffer[Token] = {

		if (i < source.length - 1) {

			val ch = source(i)

			if (ch.isLetter) {

				val t = getName(source, i + 1, token + ch, tokens)
				lex(source, t._2, t._1)
			}
			else if (ch.isDigit) {

				val t = getNumber(source, i + 1, token + ch, tokens)
				lex(source, t._2, t._1)
			}
			else if (ch == '"') {

				val t = getString(source, i + 1, token, tokens)
				lex(source, t._2, t._1)
			}
			else if (ch == '#') {

				val t = getComment(source, i + 1, token, tokens)
				lex(source, t._2, t._1)
			}
			else if (ch == ',') {
				lex(source, tokens += new Token(tokenValue = ch.toString, tokenType = "comma"), i + 1)
			}
			else if (ch == ';') {
				lex(source, tokens += new Token(tokenValue = ch.toString, tokenType = "linebreak"), i + 1)
			}
			else if (ch == '{' || ch == '}') {
				lex(source, tokens += new Token(tokenValue = ch.toString, tokenType = "paren"), i + 1)
			}
			else if (ch == '(' || ch == ')') {
				lex(source, tokens += new Token(tokenValue = ch.toString, tokenType = "arithparen"), i + 1)
			}
			else if (ch == '+' || ch == '-' || ch == '/' || ch == '*') {
				lex(source, tokens += new Token(tokenValue = ch.toString, tokenType = "op"), i + 1)
			}
			else if (ch == '=') {
				lex(source, tokens += new Token(tokenValue = ch.toString, tokenType = "eq"), i + 1)
			}
			else if (ch == '<' || ch == '>') {

				val t = getRelOp(source, i + 1, token + ch, tokens)
				lex(source, t._2, t._1)
			}
			else {
				lex(source, tokens, i + 1)
			}
		}
		else {
			tokens += new Token(tokenValue = "", tokenType = "EOF")
		}
	}

	@tailrec
	private def getName(source: Array[Char], i: Int, token: String, tokens: ArrayBuffer[Token]): (Int, ArrayBuffer[Token]) = {

		val ch = source(i)
		if (ch.isLetterOrDigit || ch == '_') {
			getName(source, i + 1, token + ch, tokens)
		}
		else {

			if (token == "var" || token == "let" || token == "if" || token == "else" ||
				token == "while" || token == "print" || token == "println" || token == "input") {

				tokens += new Token(tokenValue = token, tokenType = "keyword")
			}
			else {
				tokens += new Token(tokenValue = token, tokenType = "ident")
			}
			(i, tokens)
		}
	}

	@tailrec
	private def getNumber(source: Array[Char], i: Int, token: String, tokens: ArrayBuffer[Token]): (Int, ArrayBuffer[Token]) = {

		val ch = source(i)
		if (ch.isDigit) {
			getNumber(source, i + 1, token + ch, tokens)
		}
		else {
			tokens += new Token(tokenValue = token, tokenType = "constant")
			(i, tokens)
		}
	}

	@tailrec
	private def getString(source: Array[Char], i: Int, token: String, tokens: ArrayBuffer[Token]): (Int, ArrayBuffer[Token]) = {

		val ch = source(i)
		if (ch != '"') {
			getString(source, i + 1, token + ch, tokens)
		}
		else {
			tokens += new Token(tokenValue = token, tokenType = "strliteral")
			(i + 1, tokens)
		}
	}

	@tailrec
	private def getComment(source: Array[Char], i: Int, token: String, tokens: ArrayBuffer[Token]): (Int, ArrayBuffer[Token]) = {

		val ch = source(i)
		if (ch != '#') {
			getComment(source, i + 1, token, tokens)
		}
		else {
			(i + 1, tokens)
		}
	}

	@tailrec
	private def getRelOp(source: Array[Char], i: Int, token: String, tokens: ArrayBuffer[Token]): (Int, ArrayBuffer[Token]) = {

		val ch = source(i)
		if (token == "<" && ch == '>' || ch == '=') {
			getRelOp(source, i + 1, token + ch, tokens)
		}
		else {
			tokens += new Token(tokenValue = token, tokenType = "relop")
			(i, tokens)
		}
	}

	/**
	  * The old, imperative style, lexer. I'll keep it because it is the code I explained
	  * in my thesis.
	  */
	/*def lex(): ArrayBuffer[Token] = {

		var tokens = ArrayBuffer[Token]()
		var token = ""
		var ch = ' '
		var i = 0

		while (i < source.length - 1) {

			ch = source(i)

			if (ch == '#') {

				i += 1
				ch = source(i)

				while (ch != '#' && i < source.length - 1) {

					i += 1
					ch = source(i)
				}

				if (ch != '#') {
					Compiler.error("ERROR - COMMENT NOT CLOSED")
				}
				i += 1
			}
			else if (ch == '"') {

				i += 1
				ch = source(i)
				while (ch != '"' && i < source.length - 1) {

					token += ch
					i += 1
					ch = source(i)
				}

				if (ch != '"') {
					Compiler.error("ERROR - STRING NOT CLOSED")
				}

				tokens += new Token(tokenValue = token, tokenType = "strliteral")
				token = ""
				i += 1
			}
			else if (ch.isLetter) {

				while (ch.isLetterOrDigit || ch == '_') {

					token += ch
					i += 1
					ch = source(i)
				}

				if (token == "var" || token == "let" || token == "if" || token == "else" || token == "while" ||
					token == "print" || token == "println" || token == "input") {

					tokens += new Token(tokenValue = token, tokenType = "keyword")
				}
				else {
					tokens += new Token(tokenValue = token, tokenType = "ident")
				}
				token = ""
			}
			else if (ch.isDigit) {

				while (ch.isDigit) {

					token += ch
					i += 1
					ch = source(i)
				}

				tokens += new Token(tokenValue = token, tokenType = "constant")
				token = ""
			}
			else if (ch == '{' || ch == '}') {

				token += ch
				tokens += new Token(tokenValue = token, tokenType = "paren")
				token = ""
				i += 1
			}
			else if (ch == '(' || ch == ')') {

				token += ch
				tokens += new Token(tokenValue = token, tokenType = "arithparen")
				token = ""
				i += 1
			}
			else if (ch == '=') {

				token += ch
				tokens += new Token(tokenValue = token, tokenType = "eq")
				token = ""
				i += 1
			}
			else if (ch == '+' || ch == '-' || ch == '/' || ch == '*') {

				token += ch
				tokens += new Token(tokenValue = token, tokenType = "op")
				token = ""
				i += 1
			}
			else if (ch == '<' || ch == '>') {

				token += ch

				if (i + 1 < source.length - 1) {

					if (ch == '<' && source(i + 1) == '>') {

						token += source(i + 1)
						i += 1
					}
					else if (source(i + 1) == '=') {

						token += source(i + 1)
						i += 1
					}
				}
				tokens += new Token(tokenValue = token, tokenType = "relop")
				token = ""
				i += 1
			}
			else if (ch == ';') {

				token += ch
				tokens += new Token(tokenValue = token, tokenType = "linebreak")
				token = ""
				i += 1
			}
			else if (ch == ',') {

				token += ch
				tokens += new Token(tokenValue = token, tokenType = "comma")
				token = ""
				i += 1
			}
			else  {
				i += 1
			}
		}
		tokens += new Token(tokenValue = "", tokenType = "EOF")
		tokens
	}*/
}
