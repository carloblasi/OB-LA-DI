import java.io.FileNotFoundException

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by carloblasi on 02/05/17.
  */
class Lexer(path: String) {

	var source: Array[Char] = null

	try {
		source = Source.fromFile(path, "UTF-8").toArray :+ ' '
	} catch {
		case e: FileNotFoundException => Compiler.error("ERROR - FILE NOT FOUND")
	}

	/**
	  * Method that splits the source code in tokens and fills the tokens Array with them.
	  * It also checks the lexical correctness of the code.
	  *
	  * @return an Array of tokens
	  */
	def lex(): ArrayBuffer[Token] = {

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
	}
}
