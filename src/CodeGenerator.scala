import java.io.{ BufferedWriter, File, FileWriter }
import scala.io.Source

import sys.process._
import scala.collection.mutable.ArrayBuffer

/**
  * Created by carloblasi on 06/05/17.
  */
class CodeGenerator(filename: String) {

	private def generate(AST: Node, symTable: ArrayBuffer[Entry], writer: BufferedWriter, brAddr: Int = 0, lsAddr: Int = 0): Unit = {

		if (AST.nodeType == "eq") {

			generate(AST.children(1), symTable, writer, CodeGenerator.brc, CodeGenerator.lpc)

			val entry = symTable.filter(_.entryValue == AST.children(0).nodeValue).filter(_.entryScope <= AST.children(0).scope)
				.reduceLeft((x, y) => if (x.entryScope > y.entryScope) x else y)

			writer.write("\t" + "POP     qword[rbp - " + entry.address + "] ; " + entry.entryValue + "\n")
		}
		else if (AST.nodeType == "ident") {

			val entry = symTable.filter(_.entryValue == AST.nodeValue).filter(_.entryScope <= AST.scope)
				.reduceLeft((x, y) => if (x.entryScope > y.entryScope) x else y)

			writer.write("\t" + "PUSH    qword[rbp - " + entry.address + "] ; " + entry.entryValue + "\n")
		}
		else if (AST.nodeType == "constant") {
			writer.write("\t" + "PUSH    " + AST.nodeValue + "\n")
		}
		else if (AST.nodeType == "strliteral") {

			writer.write("\t" + "MOV     rax, " + AST.nodeValue + "\n")
			writer.write("\t" + "PUSH    rax" + "\n")
		}
		else if (AST.nodeValue == "+") {

			AST.children.foreach(child => generate(child, symTable, writer, CodeGenerator.brc, CodeGenerator.lpc))
			writer.write("\t" + "POP     rax" + "\n")
			writer.write("\t" + "POP     rbx" + "\n")
			writer.write("\t" + "ADD     rax, rbx" + "\n")
			writer.write("\t" + "PUSH    rax" + "\n")
		}
		else if (AST.nodeValue == "-") {

			AST.children.foreach(child => generate(child, symTable, writer, CodeGenerator.brc, CodeGenerator.lpc))
			writer.write("\t" + "POP     rbx" + "\n")
			writer.write("\t" + "POP     rax" + "\n")
			writer.write("\t" + "SUB     rax, rbx" + "\n")
			writer.write("\t" + "PUSH    rax" + "\n")
		}
		else if (AST.nodeValue == "/") {

			AST.children.foreach(child => generate(child, symTable, writer, CodeGenerator.brc, CodeGenerator.lpc))
			writer.write("\t" + "POP     rbx" + "\n")
			writer.write("\t" + "POP     rax" + "\n")
			writer.write("\t" + "CQO     ; sign extension" + "\n")
			writer.write("\t" + "IDIV    rbx" + "\n")
			writer.write("\t" + "PUSH    rax" + "\n")
		}
		else if (AST.nodeValue == "*") {

			AST.children.foreach(child => generate(child, symTable, writer, CodeGenerator.brc, CodeGenerator.lpc))
			writer.write("\t" + "POP     rbx" + "\n")
			writer.write("\t" + "POP     rax" + "\n")
			writer.write("\t" + "IMUL    rbx" + "\n")
			writer.write("\t" + "PUSH    rax" + "\n")
		}
		else if (AST.nodeValue == "NEG") {

			AST.children.foreach(child => generate(child, symTable, writer, CodeGenerator.brc, CodeGenerator.lpc))
			writer.write("\t" + "POP     rax" + "\n")
			writer.write("\t" + "NEG     rax" + "\n")
			writer.write("\t" + "PUSH    rax" + "\n")
		}
		else if (AST.nodeValue == "CONDBR") {

			writer.write("\t; START IF\n")
			generate(AST.children(0), symTable, writer, CodeGenerator.brc, CodeGenerator.lpc)

			AST.children(0).nodeValue match {

				// If the condition is not satisfied, go to the else block
				case "<" => {

					if (AST.children.size == 3) {
						writer.write("\t" + "JGE     " + ".B" + (brAddr) + "\n")
					} else {
						writer.write("\t" + "JGE     " + ".B" + (brAddr + 1) + "\n")
					}
				}
				case ">" => {

					if (AST.children.size == 3) {
						writer.write("\t" + "JLE     " + ".B" + (brAddr) + "\n")
					} else {
						writer.write("\t" + "JLE     " + ".B" + (brAddr + 1) + "\n")
					}
				}
				case "=" => {

					if (AST.children.size == 3) {
						writer.write("\t" + "JNE     " + ".B" + (brAddr) + "\n")
					} else {
						writer.write("\t" + "JNE     " + ".B" + (brAddr + 1) + "\n")
					}
				}
				case "<=" => {

					if (AST.children.size == 3) {
						writer.write("\t" + "JG      " + ".B" + (brAddr) + "\n")
					} else {
						writer.write("\t" + "JG      " + ".B" + (brAddr + 1) + "\n")
					}
				}
				case ">=" => {

					if (AST.children.size == 3) {
						writer.write("\t" + "JL      " + ".B" + (brAddr) + "\n")
					} else {
						writer.write("\t" + "JL      " + ".B" + (brAddr + 1) + "\n")
					}
				}
			}

			generate(AST.children(1), symTable, writer, CodeGenerator.brCount, CodeGenerator.lpc)
			if (AST.children.size == 3) {

				writer.write("\t" + "JMP     " + ".B" + (brAddr + 1) + "\n") // Go to after the else block
				writer.write(".B" + (brAddr) + ":\n")
				generate(AST.children(2), symTable, writer, CodeGenerator.brc, CodeGenerator.lpc)
				writer.write("\t; END IF/ELSE\n")
			} else {
				writer.write("\t; END IF\n")
			}
			writer.write(".B" + (brAddr + 1) + ":\n")
		}
		else if (AST.nodeType == "relop") {

			generate(AST.children(0), symTable, writer, CodeGenerator.brc, CodeGenerator.lpc)
			writer.write("\t" + "POP     rcx" + "\n")
			generate(AST.children(1), symTable, writer, CodeGenerator.brc, CodeGenerator.lpc)
			writer.write("\t" + "POP     rax" + "\n")
			writer.write("\t" + "CMP     rcx, rax" + "\n")
		}
		else if (AST.nodeValue == "IFBLOCK") {

			AST.children.foreach(child => generate(child, symTable, writer, CodeGenerator.brc, CodeGenerator.lpc))
		}
		else if (AST.nodeValue == "ELSEBLOCK") {

			AST.children.foreach(child => generate(child, symTable, writer, CodeGenerator.brc, CodeGenerator.lpc))
		}
		else if (AST.nodeValue == "PRINT") {

			// .reverse to perform calculations in reverse because of the way the stack works
			AST.children.reverse.foreach(child => generate(child, symTable, writer, CodeGenerator.brc, CodeGenerator.lpc))
			for (i <- AST.children) {

				if (i.nodeType == "strliteral") {

					writer.write("\t" + "POP     rax" + "\n")
					writer.write("\t" + "CALL    printstring" + "\n")

				} else {

					writer.write("\t" + "POP     rax" + "\n")
					writer.write("\t" + "CALL    printint" + "\n")
				}
			}
		}
		else if (AST.nodeValue == "PRINTLN") {

			// .reverse to perform calculations in reverse because of the way the stack works
			AST.children.reverse.foreach(child => generate(child, symTable, writer, CodeGenerator.brc, CodeGenerator.lpc))
			for (i <- AST.children) {

				if (i.nodeType == "strliteral") {

					writer.write("\t" + "POP     rax" + "\n")
					writer.write("\t" + "CALL    printstring" + "\n")

				} else {

					writer.write("\t" + "POP     rax" + "\n")
					writer.write("\t" + "CALL    printint" + "\n")
				}
			}
			writer.write("\t" + "MOV     rax, 10" + "\n")
			writer.write("\t" + "PUSH    rax" + "\n")
			writer.write("\t" + "MOV     rax, rsp" + "\n")
			writer.write("\t" + "CALL    printstring" + "\n")
			writer.write("\t" + "POP     rax" + "\n")
		}
		else if (AST.nodeValue == "LOOPST") {

			writer.write("\t; START WHILE\n")
			writer.write(".L" + CodeGenerator.lpc + ":\n")

			generate(AST.children(0), symTable, writer, CodeGenerator.brc, CodeGenerator.lpc)
			AST.children(0).nodeValue match {
				// If the condition is not satisfied, exit the loop
				case "<" => writer.write("\t" + "JGE     " + ".L" + (lsAddr + 1) + "\n")
				case ">" => writer.write("\t" + "JLE     " + ".L" + (lsAddr + 1) + "\n")
				case "<=" => writer.write("\t" + "JG      " + ".L" + (lsAddr + 1) + "\n")
				case ">=" => writer.write("\t" + "JL      " + ".L" + (lsAddr + 1) + "\n")
				case "=" => writer.write("\t" + "JNE     " + ".L" + (lsAddr + 1) + "\n")
			}

			generate(AST.children(1), symTable, writer, CodeGenerator.brc, CodeGenerator.lpCount)
			writer.write("\t" + "JMP     " + ".L" + (lsAddr) + "\n")
			writer.write(".L" + (lsAddr + 1) + ":\n")
			writer.write("\t; END WHILE\n")
		}
		else if (AST.nodeValue == "WHILEBLOCK") {
			AST.children.foreach(child => generate(child, symTable, writer, CodeGenerator.brc, CodeGenerator.lpc))
		}
		else if (AST.nodeValue == "VARDEC") {

		}
		else if (AST.nodeValue == "INPUT") {

			writer.write("\t" + "CALL    getint" + "\n")
			writer.write("\t" + "PUSH    rax" + "\n")
			writer.write("\t" + "XOR     rax, rax" + "\n")

			val entry = symTable.filter(_.entryValue == AST.children(0).nodeValue).filter(_.entryScope <= AST.children(0).scope)
				.reduceLeft((x, y) => if (x.entryScope > y.entryScope) x else y)

			writer.write("\t" + "POP     qword[rbp - " + entry.address + "] ; " + entry.entryValue + "\n")

		}
		else {
			AST.children.foreach(child => generate(child, symTable, writer, CodeGenerator.brc, CodeGenerator.lpc))
		}
	}

	def gen(AST: Node, symTable: ArrayBuffer[Entry]): Unit = {

		val file = new File(filename + ".asm")
		val functions = Source.fromInputStream(getClass.getResourceAsStream("/functions.asm")).mkString
		//val file = File.createTempFile("obl", "asm")

		val writer = new BufferedWriter(new FileWriter(file))
		writer.write(functions + "\n;" + "-" * 99 + ";\n\n")

		var constDecl = false
		symTable.foreach(e => if (e.entryType == "STRING" && e.entryKind == "constant") constDecl = true)
		if (constDecl) {

			writer.write("section .data\n")
			symTable.filter(_.entryType == "STRING").filter(_.entryKind == "constant").foreach(e => {
				writer.write(e.strAddress + ": db \"" + e.entryValue + "\", 0" + "\n")
			})
		}

		val memOffset = {

			if (symTable.nonEmpty)
				symTable.reduceLeft((x, y) => if (x.address > y.address) x else y).address
			else
				0
		}

		writer.write("\nsection .text\nglobal start\nstart:\n")

		writer.write("\t" + "PUSH    rbp" + "\n")
		writer.write("\t" + "MOV     rbp, rsp" + "\n")
		writer.write("\t" + "SUB     rsp, " + memOffset + "\n")

		generate(AST, symTable, writer)

		writer.write("\t" + "MOV     rax, 10" + "\n")
		writer.write("\t" + "PUSH    rax" + "\n")
		writer.write("\t" + "MOV     rax, rsp" + "\n")
		writer.write("\t" + "CALL    printstring" + "\n")
		writer.write("\t" + "POP     rax" + "\n")
		writer.write("\t" + "MOV     rax, sys_exit\n\tMOV     rbx, 0\n\tSYSCALL\n\tRET\n")
		writer.close()
	}
}

object CodeGenerator {

	private var lpc = 0
	private var brc = 0

	def lpCount = {

		lpc += 2
		lpc
	}

	def brCount = {

		brc += 2
		brc
	}
}
