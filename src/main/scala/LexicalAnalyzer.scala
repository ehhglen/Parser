import LexicalAnalyzer.{ ASSGM_STMT, BASIC_OP, WORD_TO_TOKEN}

import scala.io.Source

/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg01 - Lexical Analyzer
 * Student(s) Name(s): Echglene Woy
 */

/*
expression = expression ( ´+´  | ´-´ ) term | term
term       = term ( ´*´ | ´/´ ) factor | factor
factor     = identifier | literal | ´(´ expression ´)´
identifier = letter { ( letter | digit ) }
letter     = ´a´ | ´b´ | ´c´ | ´d´ | ´e´ | ´f´ | ´g´ | ´h´ | ´i´ | ´j´ | ´k´ | ´l´ | ´m´ | ´n´ | ´o´ | ´p´ | ´q´ | ´r´ | ´s´ | ´t´ | ´u´ | ´v´ | ´w´ | ´x´ | ´y´ | ´z´
literal    = digit { digit }
digit      = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
 */

class LexicalAnalyzer(private var source: String) extends Iterable[LexemeUnit] {

  private var input = ""
  for (line <- Source.fromFile(source).getLines)
    input += line + "\n"

  // determines the class of a given character
  private def getCharClass(c: Char): CharClass.Value = {
    if (LexicalAnalyzer.LETTERS.contains(c))
      CharClass.LETTER
    else if (LexicalAnalyzer.DIGITS.contains(c))
      CharClass.DIGIT
    else if (LexicalAnalyzer.BLANKS.contains(c))
      CharClass.BLANK
    else if (c == '+' || c == '-' || c == '*' || c == '/' || c == '=')
      CharClass.OPERATOR
    else if ( c == '<' || c == '>' || c == '=')
      CharClass.COMPARISON
    else if (c == '.' || c == ',' || c == ';' || c == ':')
      CharClass.PUNCTUATOR
    else if (c == '(' || c == ')')
      CharClass.DELIMITER
    else
      CharClass.OTHER
  }

  // reads the input until a non-blank character is found, returning the input updated
  private def readBlanks: Unit = {
    var foundNonBlank = false
    while (input.length > 0 && !foundNonBlank) {
      val c = input(0)
      if (getCharClass(c) == CharClass.BLANK)
        input = input.substring(1)
      else
        foundNonBlank = true
    }
  }

  def iterator: Iterator[LexemeUnit] = {
    new Iterator[LexemeUnit] {

      override def hasNext: Boolean = {
        readBlanks
        input.length > 0
      }

      override def next(): LexemeUnit = {
        if (!hasNext)
          new LexemeUnit("", Token.EOF)
        else {
          var lexeme = ""
          readBlanks
          if (input.length == 0)
            new LexemeUnit(lexeme, Token.EOF)
          else {
            var c = input(0)
            var charClass = getCharClass(c)

            // TODO: finish the code

            // TODO: recognize a letter followed by letters (or digits) as an identifier
            if (charClass == CharClass.LETTER) {
              input = input.substring(1)
              lexeme += c
              var noMoreLetterDigits = false
              while (!noMoreLetterDigits) {
                if (input.length == 0)
                  noMoreLetterDigits = true
                else {
                  c = input(0)
                  charClass = getCharClass(c)
                  if (charClass == CharClass.LETTER || charClass == CharClass.DIGIT) {
                    input = input.substring(1)
                    lexeme += c
                  }
                  else
                    noMoreLetterDigits = true
                }
              }
              // If reserved word, map to WORD_TO_TOKEN
              if (WORD_TO_TOKEN.contains(lexeme)) {
                return new LexemeUnit(lexeme, WORD_TO_TOKEN.get(lexeme).get)
              }
              return new LexemeUnit(lexeme, Token.IDENTIFIER)
            }

            // TODO: recognize a Int_literal
            if (charClass == CharClass.DIGIT) {
              input = input.substring(1)
              lexeme += c
              var noMoreDigits = false
              while (!noMoreDigits) {
                if (input.length == 0)
                  noMoreDigits = true
                else {
                  c = input(0)
                  charClass = getCharClass(c)
                  if (charClass == CharClass.DIGIT) {
                    input = input.substring(1)
                    lexeme += c
                  }
                  else
                    noMoreDigits = true
                }
              }
              return new LexemeUnit(lexeme, Token.INT_LITERAL)
            }

            // TODO: recognize operators
            if (charClass == CharClass.OPERATOR) {
              input = input.substring(1)
              lexeme += c

              c match {
                case '+' => return new LexemeUnit(lexeme, Token.ADD_OP)
                case '-' => return new LexemeUnit(lexeme, Token.SUB_OP)
                case '*' => return new LexemeUnit(lexeme, Token.MUL_OP)
                case '/' => return new LexemeUnit(lexeme, Token.DIV_OP)
//                case '>' => return new LexemeUnit(lexeme, Token.GREATER_OP)
//                case '<' => return new LexemeUnit(lexeme, Token.LESS_OP)
//                case '=' => return new LexemeUnit(lexeme, Token.EQUAL_OP)
              }
            }
            if (charClass == CharClass.COMPARISON) {
              input = input.substring(1)
              lexeme += c
              var noMoreLetterDigits = false
              while (!noMoreLetterDigits) {
                if (input.length == 0)
                  noMoreLetterDigits = true
                else {
                  c = input(0)
                  //                  charClass = getCharClass(c)
                  if (c == '=') {
                    input = input.substring(1)
                    lexeme += c
                  }
                  else
                    noMoreLetterDigits = true
                }
              }
              return new LexemeUnit(lexeme, Token.ARITHMETIC_EXPR)
            }

            // TODO: recognize operators
//            if (charClass == CharClass.OPERATOR) {
//              input = input.substring(1)
//              lexeme += c
//              var noMoreLetters = false
//              while (!noMoreLetters) {
//                if (input.length == 0)
//                  noMoreLetters = true
//                else {
//                  c = input(0)
//                  charClass = getCharClass(c)
//                  if (charClass == CharClass.OPERATOR) {
//                    input = input.substring(1)
//                    lexeme += c
//                  }
//                  else
//                    noMoreLetters = true
//                }
//              }
//              if (ARITHMETIC_EXPR.contains(lexeme))
//                return new LexemeUnit(lexeme, ARITHMETIC_EXPR.get(lexeme).get)
////              else if (BASIC_OP.contains(lexeme))
////                return new LexemeUnit(lexeme, BASIC_OP.get(lexeme).get)
//              else
//                return new LexemeUnit(lexeme, Token.OPERATOR)
//
//            }

            // TODO: recognize delimiters
            if (charClass == CharClass.DELIMITER) {
              input = input.substring(1)
              lexeme += c
              c match {
                case '(' => return new LexemeUnit(lexeme, Token.OPEN_PAR)
                case ')' => return new LexemeUnit(lexeme, Token.CLOSE_PAR)
              }
            } // end delimiters

            // TODO: recognize punctuators
            if (charClass == CharClass.PUNCTUATOR) {
              input = input.substring(1)
              lexeme += c
              var noMoreLetters = false
              while (!noMoreLetters) {
                if (input.length == 0)
                  noMoreLetters = true
                else {
                  c = input(0)
                  charClass = getCharClass(c)
                  if (charClass == CharClass.PUNCTUATOR || charClass == CharClass.OPERATOR) {
                    input = input.substring(1)
                    lexeme += c
                  }
                  else
                    noMoreLetters = true
                }
              }
              if (WORD_TO_TOKEN.contains(lexeme)) {
                return new LexemeUnit(lexeme, WORD_TO_TOKEN.get(lexeme).get)
              }
              else if (ASSGM_STMT.contains(lexeme)) {
                return new LexemeUnit(lexeme, ASSGM_STMT.get(lexeme).get)
              }
              else {
                return new LexemeUnit(lexeme, Token.PUNCTUATOR)
              }
            } // end punctuators


//            // TODO: recognize arithmetic expressions
//            if (charClass == CharClass.OPERATOR) {
//              input = input.substring(1)
//              lexeme += c
//              var noMoreLetters = false
//              while (!noMoreLetters) {
//                if (input.length == 0)
//                  noMoreLetters = true
//                else {
//                  c = input(0)
//                  charClass = getCharClass(c)
//                  if (charClass == CharClass.OPERATOR) {
//                    input = input.substring(1)
//                    lexeme += c
//                  }
//                  else
//                    noMoreLetters = true
//                }
//              }
//                return new LexemeUnit(lexeme, Token.ARITHMETIC_EXPR)
//            }





            // throw an exception if an unrecognizable symbol is found
            throw new Exception("Lexical Analyzer Error: unrecognizable symbol found!")
          }
        }
      } // end next
    } // end 'new' iterator
  } // end iterator method
} // end LexicalAnalyzer class

object LexicalAnalyzer {
  val LETTERS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val DIGITS  = "0123456789"
  val BLANKS  = " \n\t"
  val WORD_TO_TOKEN = Map(
    "declare"  -> Token.DECLARE,
    "real"     -> Token.REAL,
    "complex"  -> Token.COMPLEX,
    "fixed"    -> Token.FIXED,
    "floating" -> Token.FLOATING,
    "single"   -> Token.SINGLE,
    "double"   -> Token.DOUBLE,
    "binary"   -> Token.BINARY,
    "decimal"  -> Token.DECIMAL,
    "if"       -> Token.IF_STMT,
    "begin"    -> Token.BEGIN_STMT,
    "read"     -> Token.READ_STMT,
    "write"    -> Token.WRITE_STMT,
    "program"  -> Token.PROGRAM,
    "var"      -> Token.VAR,
    "while"    -> Token.WHILE_STMT,
    ";"        -> Token.SEMI_COL,
    ":"        -> Token.COLON,
    "."        -> Token.PERIOD,
    "Integer"  -> Token.INTEGER,
//    "<=" -> Token.LESS_OP,
//    ">=" -> Token.GREATER_OP,
    "end" -> Token.END,
    "do"  -> Token.DO_STMT,
    "true" -> Token.TRUE,
    "false" -> Token.FALSE,
    "then" -> Token.THEN_STMT,
    "else" -> Token.ELSE_STMT
  )
  val BASIC_OP = Map(       // dont think I need this
    "+"  -> Token.ADD_OP,
    "-"  -> Token.SUB_OP,
    "*"  -> Token.MUL_OP,
    "/"  -> Token.DIV_OP,
    "<"  -> Token.GREATER_OP,
    ">"  -> Token.LESS_OP,
    "="  -> Token.EQUAL_OP,
  )

//  val ARITHMETIC_EXPR = Map(
//    "<=" -> Token.LESS_OP,
//    ">=" -> Token.GREATER_OP,
//  )
  val ASSGM_STMT = Map(
    ":=" -> Token.ASSGM_STMT
  )




  def main(args: Array[String]): Unit = {
    // check if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    val lex = new LexicalAnalyzer(args(0))
    val it = lex.iterator
    while (it.hasNext) {
      val lexemeUnit = it.next()
      println(lexemeUnit)
    }
  } // end main method
} // end LexicalAnalyzer object