/**
  * Created by carloblasi on 29/04/17.
  * 
  * A Token is an object representing a block of characters in the source code which falls
  * in one of the categories of possible tokens (keyword, variable, constant, op, eq, paren)
  *
  * @param tokenValue The block of characters of which the token is made of
  * @param tokenType The category of the token
  */
class Token(var tokenValue: String, var tokenType: String) {

	var spaces = ""

	tokenType match {

		case "ident" => spaces = "     "
		case "op" => spaces = "        "
		case "eq" => spaces = "        "
		case "constant" => spaces = "  "
		case "keyword" => spaces = "   "
		case "paren" => spaces = "     "
		case "relop" => spaces = "     "
		case "comma" => spaces = "     "
		case "string" => spaces = "    "
		case _ => spaces = " "
	}

	/**
	  * Method to print a Token with fixed space independently from the tokenType
	  * @return A formatted string for the token 
	  */
	override def toString(): String = {
		tokenType + ":" + spaces + tokenValue
	}
}
