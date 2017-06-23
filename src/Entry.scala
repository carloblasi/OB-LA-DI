/**
  * Created by carloblasi on 29/04/17.
  * 
  * An Entry is an object representing an entry in the symbol table.
  *
  * @param entryType the type of the identifier (either "INT" or "STRING")
  * @param entryKind the kind of the identifier (either "variable" or "function")
  */
class Entry(var entryValue: String, var entryType: String, var entryKind: String, var entryScope: Int = 0, var entryFunctionScope: String, var initialized: Boolean = false) {

	val size: Int = this.entryType match {

		case "INT" => 8
		case _ => -1
	}

	val strAddress = {

		if (this.entryType == "STRING" && this.entryKind == "constant")
			"STR" + Entry.newStrAddress()
		else
			""
	}
	val address = Entry.newAddress(size)

	def setInitialized(init: Boolean): Unit = {
		this.initialized = init
	}

	override def toString(): String = {
		"[Value: " + entryValue + ", Type: " + entryType + ", Kind: " + entryKind + ", Scope: " + entryScope +  ", fScope: " + entryFunctionScope + ", Address: " + address + ", Initialized: " + initialized + ", Size: " + size + "]"
	}

	override def equals(obj: scala.Any): Boolean = {

		entryValue == obj.asInstanceOf[Entry].entryValue &&
		entryType == obj.asInstanceOf[Entry].entryType &&
		entryKind == obj.asInstanceOf[Entry].entryKind &&
		entryScope == obj.asInstanceOf[Entry].entryScope
	}
}

object Entry {

	private var address = 0
	private var strAddress = 0

	private def newAddress(size: Int) = {

		if (size > 0) {

			address += size
			address
		}
		else {
			size
		}
	}

	private def newStrAddress() = {

		strAddress += 1
		strAddress
	}
}