import scala.collection.mutable.ArrayBuffer

/**
  * Created by carloblasi on 29/04/17.
  *
  * A Node is an object representing a node in a tree; a Node can be a tree itself.
  *
  * @param nodeValue The block of characters of which the token is made of
  * @param nodeType The category of the token
  */
class Node(var nodeValue: String, var nodeType: String = "") {

	var children = ArrayBuffer[Node]()
	var parent: Node = null
	var scope: Int = -1

	/**
	 * Method to add a child Node to the list of children of this Node
	 */
	def addChild(node: Node): Unit = {

		node.setParent(this)
		this.children += node
	}

	/**
	 * Method to get a child Node from the list of children of this Node given its index
	 */
	def getChild(index: Int): Node = {

		val idx = index - 1
		this.children(idx)
	}

	/**
	 * Method to set the parent of this Node
	 */
	private def setParent(node: Node): Unit = {
		this.parent = node
	}

	/**
	  * Method to set the parent of this Node
	  */
	private def setScope(scope: Int): Unit = {
		this.scope = scope
	}

	/**
	 * Method to print the tree representation starting at the node on which this method
	 * is called
	 */
	def printTree(depth: Int = 0): Unit = {

		println("      " * depth + this)
		this.children.foreach(child => child.printTree(depth + 1))
	}

	/**
	 * Method to abstract the tree starting at the node on which this method is called,
	 * which means removing all unnecessary nodes
	 */
	def abstractNodes(): Node = {

		while (this.children.size == 1 && (this.nodeValue != "PRINT" && this.nodeValue != "INPUT" &&
			this.nodeValue != "VARDEC" && this.nodeValue != "ELSEBLOCK" && this.nodeValue != "IFBLOCK" &&
			this.nodeValue != "STRING" && this.nodeValue != "PRINTLN")) {
			// The other conditions are the name of the nodes which might have only one child but
			// don't have to be removed, because name of 'functions' of the language

			this.nodeValue = this.children(0).nodeValue
			this.nodeType = this.children(0).nodeType
			this.children = this.children(0).children
		}

		var c = -1
		this.children.foreach(child => {

			if (child.nodeType == "eq" || child.nodeType == "op" || child.nodeType == "relop") {

				this.nodeValue = child.nodeValue
				this.nodeType = child.nodeType
				c = this.children.indexOf(child)
			}
		})
		if (c != -1)
			this.children.remove(c)

		this.children.foreach(child => child.abstractNodes)
		this
	}

	/**
	  * Method to fill the symbol table and typecheck the source code.
	  * BUG: variables declared in an IFBLOCK are valid also in the ELSEBLOCK
	  * @return a filled symbol table
	  */
	def contextualAnalysis(symTable: ArrayBuffer[Entry] = new ArrayBuffer[Entry](), scope: Int = 0, functionScope: String = "MAIN"): ArrayBuffer[Entry] = {

		if (this.nodeValue == "LOOPST" || this.nodeValue == "CONDBR") {

			this.children.foreach(child => {

				if (child.nodeValue == "IFBLOCK" || child.nodeValue == "ELSEBLOCK" || child.nodeValue == "WHILEBLOCK") {
					child.children.foreach(child => child.contextualAnalysis(symTable, scope + 1, functionScope))
				}
				else {
					child.contextualAnalysis(symTable, scope, functionScope)
				}
			})
		}
		else if (this.nodeValue == "VARDEC") {

			this.children.foreach(child => {

				val entry = new Entry(entryValue = child.nodeValue, entryType = "INT", entryKind = "variable", entryScope = scope, entryFunctionScope = functionScope)

				child.setScope(scope)
				if (symTable.contains(entry))
					Compiler.error("ERROR - VARIABLE " + child.nodeValue + " ALREADY DECLARED IN SCOPE: " + scope)

				symTable += entry
			})
		}
		else if (this.nodeValue == "INPUT") {

			try {
				val entries: ArrayBuffer[Entry] = symTable.filter(_.entryValue == this.children(0).nodeValue).filter(_.entryScope <= scope)
				if (entries.isEmpty) {
					throw new Throwable
				}
				val entry = entries.reduceLeft((x, y) => if (x.entryScope > y.entryScope) x else y)
				entry.setInitialized(true)
			} catch {
				case e: Throwable => Compiler.error("ERROR - UNDECLARED VARIABLE: " + this.children(0).nodeValue + " IN SCOPE: " + scope)
			}
			this.children(0).setScope(scope)
		}
		else if (this.nodeType == "ident") {

			try {
				val entries: ArrayBuffer[Entry] = symTable.filter(_.entryValue == this.nodeValue).filter(_.entryScope <= scope)

				if (entries.isEmpty) {
					throw new Throwable
				}
				else {
					val entry = entries.reduceLeft((x, y) => if (x.entryScope > y.entryScope) x else y)
//					println("variable: " + this.nodeValue + " in this scope: " + scope + " " + entry)

					if (this.parent.nodeValue == "=" && this.parent.children(0) == this) {
						entry.setInitialized(true)
					}
					else {
						if (entry.initialized == false)
							Compiler.error("ERROR - INITIALIZED VARIABLE: " + this.nodeValue + " IN SCOPE: " + scope)
					}
					this.setScope(scope)
				}
			} catch {
				case e: Throwable => Compiler.error("ERROR - UNDECLARED VARIABLE: " + this.nodeValue + " IN SCOPE: " + scope)
				case e: Throwable => Compiler.error("ERROR - UNDECLARED VARIABLE: " + this.nodeValue + " IN SCOPE: " + scope)
			}
		}
		else if (this.nodeType == "strliteral") {

			val entry = new Entry(entryValue = this.nodeValue, entryType = "STRING", entryKind = "constant", entryScope = -1, entryFunctionScope = functionScope)
			this.nodeValue = entry.strAddress
			symTable += entry

		}
		else {
			this.children.foreach(child => child.contextualAnalysis(symTable, scope, functionScope))
		}
		symTable
	}

	override def toString: String = {

		this.nodeType match {

			//case "ident" => "Value: " + nodeValue + ", scope: " + this.scope
			case "" => "Value: " + nodeValue
			case _ => "Value: " + nodeValue + ", type: " + this.nodeType

		}
	}

	/**
	  * Simple depth-first traversal of the tree.
	  */
	def traverse(): Unit = {

		children.foreach(c => c.traverse())
		println(this)
	}
}
