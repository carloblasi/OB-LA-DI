import scala.collection.mutable.ArrayBuffer

/**
  * Created by carloblasi on 26/05/17.
  */
class Parser(tokens: ArrayBuffer[Token]) {

	//var tokens = ArrayBuffer[Token]()
	val verbose = false
	var next = 0

	/**
	  * Method that returns the next Token from the list previously filled by the lexer()
	  * method.
	  * @return the next Token
	  */
	def getNextToken(): Token = {

		val tok = tokens(next)
		next += 1
		tok
	}

	var nextToken: Token = _
	var AST = new Node(nodeValue = "STATEMENTS")
	var tempNode: Node = _
	var depth = 0

	/**
	  * Method that makes a tree out of the list of tokens previously filled by the lexer()
	  * method.
	  * It checks the syntactical correctness of the code.
	  */
	def parse(): Node = {

		tempNode = AST
		nextToken = getNextToken()
		while (nextToken.tokenType != "EOF") {

			if (verbose == true) println(next)
			statement()
		}
		AST
	}

	/**
	  * Method equivalent to the syntax rule for the <statement>
	  */
	def statement(): Unit = {

		if (nextToken.tokenValue == "var") {

			val n = new Node(nodeValue = "VARDEC")
			tempNode.addChild(n)
			tempNode = n

			if (verbose == true) println(" -- VARIABLE DECLARATION")
			nextToken = getNextToken()
			ident()
			nextToken = getNextToken()

			while (nextToken.tokenType == "comma") {

				if (verbose == true) println(" -- COMMA FOUND")
				nextToken = getNextToken()
				ident()
				nextToken = getNextToken()
			}
			if (nextToken.tokenType == "linebreak") {

				if (verbose == true) println(" -- LINEBREAK")
				nextToken = getNextToken()
			}
			else {
				Compiler.error("ERROR - MISSING LINEBREAK")
			}
			tempNode = AST
		}

		else if (nextToken.tokenValue == "let") {

			val n = new Node(nodeValue = "VARASSIGN")
			tempNode.addChild(n)
			tempNode = n

			/**
			  * Aggiungere il tipo di vardec, quindi nel contextanalyzer
			  * però bisogna controllare che ad una variabile dichiarata string non venga assegnata un espressione
			  * si può controllare nel parser, se il nextToken è di tipo string allora controlla che ci siano le quotes altrimenti fai l'espressione?
			  */
			if (verbose == true) println(" -- VARIABLE ASSIGNATION")
			nextToken = getNextToken()
			ident()
			nextToken = getNextToken()

			if (nextToken.tokenType == "eq") {

				if (verbose == true) println(" -- EQUAL FOUND")
				tempNode.addChild(new Node(nodeValue = nextToken.tokenValue, nodeType = "eq")) // nextToken.value = "="
				nextToken = getNextToken()
				expression()
			}
			else {
				Compiler.error("ERROR - EQUAL EXPECTED")
			}

			if (nextToken.tokenType == "linebreak") {

				if (verbose == true) println(" -- LINEBREAK")
				nextToken = getNextToken()
			} else {
				Compiler.error("ERROR - MISSING LINEBREAK")
			}

			tempNode = AST
		}

		else if (nextToken.tokenValue == "if") {

			val n = new Node(nodeValue = "CONDBR")
			tempNode.addChild(n)
			tempNode = n
			val n2 = new Node(nodeValue = "CONDITION")
			tempNode.addChild(n2)
			tempNode = n2

			if (verbose == true) println(" -- CONDITIONAL BRANCH")
			nextToken = getNextToken()
			expression()
			tempNode = n2

			if (nextToken.tokenType == "relop" || nextToken.tokenType == "eq") {

				if (verbose == true) println(" -- RELOP FOUND")
				tempNode.addChild(new Node(nodeValue = nextToken.tokenValue, nodeType = "relop"))
				nextToken = getNextToken()
				expression()
				tempNode = n

				if (nextToken.tokenType == "paren") {

					if (verbose == true) println(" -- LPAREN FOUND")
					val n3 = new Node(nodeValue = "IFBLOCK")
					tempNode.addChild(n3)
					tempNode = n3
					nextToken = getNextToken()
					while (nextToken.tokenType != "paren") {

						statement()
						tempNode = n3
					}
					if (verbose == true) println(" -- RPAREN FOUND")

					nextToken = getNextToken()
					if (nextToken.tokenValue == "else") {

						tempNode = n
						nextToken = getNextToken()
						if (nextToken.tokenType == "paren") {

							if (verbose == true) println(" -- LPAREN FOUND")
							val n4 = new Node(nodeValue = "ELSEBLOCK")
							tempNode.addChild(n4)
							tempNode = n4
							nextToken = getNextToken()
							while (nextToken.tokenType != "paren") {

								statement()
								tempNode = n4
							}
							if (verbose == true) println(" -- RPAREN FOUND")
							nextToken = getNextToken()
						}
						else {
							Compiler.error("ERROR - OPENING SQUARE BRACKET EXPECTED")
						}
					}
				}
				else {
					Compiler.error("ERROR - OPENING SQUARE BRACKET EXPECTED")
				}
			}
			else {
				Compiler.error("ERROR - RELOP EXPECTED")
			}
			tempNode = AST
		}

		else if (nextToken.tokenValue == "while") {

			val n = new Node(nodeValue = "LOOPST")
			tempNode.addChild(n)
			tempNode = n
			val n2 = new Node(nodeValue = "CONDITION")
			tempNode.addChild(n2)
			tempNode = n2

			if (verbose == true) println(" -- LOOP STATEMENT")
			nextToken = getNextToken()
			expression()
			tempNode = n2

			if (nextToken.tokenType == "relop" || nextToken.tokenType == "eq") {

				if (verbose == true) println(" -- RELOP FOUND")
				tempNode.addChild(new Node(nodeValue = nextToken.tokenValue, nodeType = "relop"))
				nextToken = getNextToken()
				expression()
				tempNode = n

				if (nextToken.tokenType == "paren") {

					if (verbose == true) println(" -- LPAREN FOUND")
					val n3 = new Node(nodeValue = "WHILEBLOCK")
					tempNode.addChild(n3)
					tempNode = n3

					nextToken = getNextToken()
					while (nextToken.tokenType != "paren") {

						statement()
						tempNode = n3
					}
					if (verbose == true) println(" -- RPAREN FOUND")
					nextToken = getNextToken()
				}
				else {
					Compiler.error("ERROR - OPENING SQUARE BRACKET EXPECTED")
				}
			}
			else {
				Compiler.error("ERROR - RELOP EXPECTED")
			}
			tempNode = AST
		}

		else if (nextToken.tokenValue == "print" || nextToken.tokenValue == "println") {

			val n = new Node(nodeValue = nextToken.tokenValue.toUpperCase)
			tempNode.addChild(n)
			tempNode = n

			if (verbose == true) println(s" -- ${nextToken.tokenValue} STATEMENT")

			nextToken = getNextToken()
			if (nextToken.tokenType == "strliteral") {

				val n = new Node(nodeValue = "STRLITERAL")
				n.addChild(new Node(nodeValue = nextToken.tokenValue, nodeType = nextToken.tokenType))
				tempNode.addChild(n)
				nextToken = getNextToken()
			}
			else
				expression()

			while (nextToken.tokenType == "comma") {

				tempNode = n
				if (verbose == true) println(" -- COMMA FOUND")

				nextToken = getNextToken()
				if (nextToken.tokenType == "strliteral") {

					val n = new Node(nodeValue = "STRLITERAL")
					n.addChild(new Node(nodeValue = nextToken.tokenValue, nodeType = nextToken.tokenType))
					tempNode.addChild(n)
					nextToken = getNextToken()
				}
				else
					expression()
			}

			if (nextToken.tokenType == "linebreak") {

				if (verbose == true) println(" -- LINEBREAK")
				nextToken = getNextToken()
			}
			else {
				Compiler.error("ERROR - MISSING LINEBREAK")
			}

			tempNode = AST
		}

		else if (nextToken.tokenValue == "input") {

			val n = new Node(nodeValue = "INPUT")
			tempNode.addChild(n)
			tempNode = n

			if (verbose == true) println(" -- INPUT STATEMENT")
			nextToken = getNextToken()
			ident()
			nextToken = getNextToken()

			if (nextToken.tokenType == "linebreak") {

				if (verbose == true) println(" -- LINEBREAK")
				nextToken = getNextToken()
			} else {
				Compiler.error("ERROR - MISSING LINEBREAK")
			}

			tempNode = AST
		}

		else {

			Compiler.error("ERROR - KEYWORD UNKNOWN")
			nextToken = getNextToken()
		}
	}

	/**
	  * Method that correctly identifies an identifier (a declared variable's name)
	  */
	def ident(): Unit = {

		if (nextToken.tokenType == "ident") {

			val n = new Node(nodeValue = "IDENT")
			tempNode.addChild(n)
			n.addChild(new Node(nodeValue = nextToken.tokenValue, nodeType = "ident"))
			if (verbose == true) println(" -- Variable identified: " + nextToken.tokenValue)
		}
		else {
			Compiler.error("ERROR - IDENT EXPECTED")
		}
	}

	/**
	  * Method that correctly identifies a constant (a number)
	  */
	def constant(): Unit = {

		if (nextToken.tokenType == "constant") {

			val n = new Node(nodeValue = "CONST")
			tempNode.addChild(n)
			n.addChild(new Node(nodeValue = nextToken.tokenValue, nodeType = nextToken.tokenType))
			if (verbose == true) println(" -- Constant identified: " + nextToken.tokenValue)
		}
		else {
			Compiler.error("ERROR - CONSTANT EXPECTED")
		}
	}

	/**
	  * Method that identifies an expression
	  */
	def expression(): Unit = {

		val n = new Node(nodeValue = "EXPR")
		tempNode.addChild(n)
		tempNode = n

		if (nextToken.tokenValue == "-" || nextToken.tokenValue == "+") {

			if (verbose == true) println(" -- Unary identified: " + nextToken.tokenValue)
			if (nextToken.tokenValue == "-")
				tempNode.addChild(new Node(nodeValue = "NEG", nodeType = "op"))
			nextToken = getNextToken()
		}
		term()
		tempNode = n

		while (nextToken.tokenValue == "+" || nextToken.tokenValue == "-") {

			if (verbose == true) println(" -- Operation identified: " + nextToken.tokenValue)
			tempNode.addChild(new Node(nodeValue = nextToken.tokenValue, nodeType = "op"))
			nextToken = getNextToken()
			term()
		}
	}

	/**
	  * Method that identifies a term
	  */
	def term(): Unit = {

		val n = new Node(nodeValue = "TERM")
		tempNode.addChild(n)
		tempNode = n
		factor()
		nextToken = getNextToken()
		tempNode = n

		while (nextToken.tokenValue == "*" || nextToken.tokenValue == "/") {

			if (verbose == true) println(" -- Operation identified: " + nextToken.tokenValue)
			tempNode.addChild(new Node(nodeValue = nextToken.tokenValue, nodeType = "op"))
			nextToken = getNextToken()
			factor()
			nextToken = getNextToken()
		}
	}

	/**
	  * Method that identifies a factor
	  */
	def factor(): Unit = {

		val n = new Node(nodeValue = "FACTOR")
		tempNode.addChild(n)
		tempNode = n

		if (nextToken.tokenType == "ident") {
			ident()
		}
		else if (nextToken.tokenType == "constant") {
			constant()
		}
		else {

			if (nextToken.tokenType == "arithparen") {

				if (verbose == true) println(" -- Parenthesis identified: " + nextToken.tokenValue)
				nextToken = getNextToken()
				expression()

				if (nextToken.tokenType != "arithparen") {
					Compiler.error("ERROR - MISSING CLOSING PARENTHESIS")
				}
				else {
					if (verbose == true) println(" -- Parenthesis identified: " + nextToken.tokenValue)
				}
			}
			else {
				Compiler.error("ERROR - MISSING OPENING PARENTHESIS")
			}
		}
	}
}
