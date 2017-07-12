/**
  * Created by carloblasi on 29/04/17.
  * 
  * A Token is an object representing a block of characters in the source code which falls
  * in one of the categories of possible tokens (keyword, variable, constant, op, eq, paren).
  *
  * @param tokenValue The block of characters of which the token is made of.
  * @param tokenType The category of the token.
  */
class Token(var tokenValue: String, var tokenType: String) {

	/**
	  * Method to print a Token with fixed space independently from the tokenType.
	  * @return a well formatted string for this token.
	  */
	override def toString(): String = {
		tokenType + ":" + " " * (12 - tokenType.length) + tokenValue
	}
}
