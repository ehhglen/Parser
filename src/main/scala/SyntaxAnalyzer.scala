/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg01 - Syntax Analyzer
 * Student(s) Name(s):
 */

/*
program      = ´program´ identifier body ´.´
identifier   = letter { ( letter | digit ) }
body         = [ var_sct ] block
var_sct      = ´var´ var_dcl { ´;´ var_dcl }
var_dcl      = identifier { identifier } ´:´ type
type         = ´Integer´ | ´Boolean´
block        = ´begin´ stmt { ´;´ stmt } ´end´
stmt         = assgm_stmt | read_stmt | write_stmt | if_stmt | while_stmt | block
assgm_stmt   = identifier ´:=´ expr
read_stmt    = ´read´ identifier
write_stmt   = ´write´ ( identifier | literal )
if_stmt      = ´if´ bool_expr ´then´ stmt [ ´else´ stmt ]
while_stmt   = ´while´ bool_expr ´do´ stmt
expr         = arithm_expr | bool_expr
arithm_expr  = arithm_expr ( ´+´ | ´-´ ) term | term
term         = term ´*´ factor | factor
factor       = identifier | int_literal
literal      = int_literal | bool_literal
int_literal  = digit { digit }
bool_litreal = ´true´ | ´false´
bool_expr    = bool_literal | arithm_expr ( ´>´ | ´>=´ | ´=´ | ´<=´ | ´<´ ) arithm_expr
letter       = ´a´ | ´b´ | ´c´ | ´d´ | ´e´ | ´f´ | ´g´ | ´h´ | ´i´ | ´j´ | ´k´ | ´l´ | ´m´ | ´n´ | ´o´ | ´p´ | ´q´ | ´r´ | ´s´ | ´t´ | ´u´ | ´v´ | ´w´ | ´x´ | ´y´ | ´z´ | ´A´ | ´B´ | ´C´ | ´D´ | ´E´ | ´F´ | ´G´ | ´H´ | ´I´ | ´J´ | ´K´ | ´L´ | ´M´ | ´N´ | ´O´ | ´P´ | ´Q´ | ´R´ | ´S´ | ´T´ | ´U´ | ´V´ | ´W´ | ´X´ | ´Y´ | ´Z´
digit        = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
*/

class SyntaxAnalyzer(private var source: String) {

  private var it = new LexicalAnalyzer(source).iterator
  private var lexemeUnit: LexemeUnit = null

  private def getLexemeUnit() = {
    if (lexemeUnit == null)
      lexemeUnit = it.next()
  }

  def parse(): Tree = {
    parseProgram()
  }

  // TODO: finish the syntax analyzer
  // TODOd: program = `program` identifier body `.`
  private def parseProgram() = {
    // create a tree with label "program"
    val tree = new Tree("program")
    getLexemeUnit()

    if (lexemeUnit.getToken() == Token.PROGRAM) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
      tree.add(parseIdentifier())
      lexemeUnit = null
      tree.add(parseBody())
      if (lexemeUnit.getToken() == Token.PUNCTUATOR) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
      }
      else
        throw new Exception("Syntax Analyzer Error: Period Expected!")
    }
    else
      throw new Exception("Syntax Analyzer Error: Program Expected!")
    // return the tree
    tree
  } // end parseProgram

  // TODOd: identifier = letter { ( letter | digit ) }
  private def parseIdentifier() = new Tree("identifier: '" + lexemeUnit.getLexeme() + "'")

  // TODO: body = [ var_sct ] block
  private def parseBody() = {
    val tree = new Tree("body")
    getLexemeUnit()
    if (lexemeUnit.getToken() != Token.EOF) {
      tree.add(parseVar_Sct())
      tree.add(parseBlock())
    } else
      throw new Exception("Syntax Analyzer Error: body expected!")
    tree
  }

  // TODO: var_sct = ´var´ var_dcl { ´;´ var_dcl }
  private def parseVar_Sct() = {
    val tree = new Tree("var_sct")
    getLexemeUnit()
    if (lexemeUnit.getToken() == Token.VAR) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
    }
    var done = false
    while (!done) {
      tree.add(parseVar_Dcl())
      if (lexemeUnit.getToken() == Token.SEMI_COL) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
      }
      else
        done = true
    }
    //    while (!done) {
    //      if(lexemeUnit.getToken() == Token.VAR) {
    //        tree.add(new Tree(lexemeUnit.getLexeme()))
    //        lexemeUnit = null
    //        getLexemeUnit()
    ////        tree.add(parseVar_Dcl())
    //        if(lexemeUnit.getToken() == Token.SEMI_COL) {
    //          tree.add(new Tree(lexemeUnit.getLexeme()))
    //          lexemeUnit = null
    //          getLexemeUnit()
    //        }
    //      }
    //    }
    tree
  }

  // TODO: var_dcl = identifier { identifier } ´:´ type
  private def parseVar_Dcl() = {
    val tree = new Tree("var_dcl")
    getLexemeUnit()
    var done = false
    while(!done) {
      tree.add(parseIdentifier())
      if(lexemeUnit.getToken() == Token.COLON) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
      }
      else
        done = true
    }
    tree
  }
  // TODO: type = ´Integer´ | ´Boolean´
  private def parseType() = {
    val tree = new Tree("type")
    getLexemeUnit()
    if(lexemeUnit.getToken() != Token.EOF) {
      if(lexemeUnit.getToken() == Token.INTEGER || lexemeUnit.getToken() == Token.BOOLEAN) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
      }
    }
    tree
  }

  // block = ´begin´ stmt { ´;´ stmt } ´end´
  private def parseBlock() = {
    val tree = new Tree("block")
    getLexemeUnit()
    if(lexemeUnit.getToken() == Token.BEGIN_STMT) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      tree.add(parseStmt())
    }
     tree
  } // end parseBlock

  // stmt = assgm_stmt | read_stmt | write_stmt | if_stmt | while_stmt | block
  private def parseStmt() = {
    val tree = new Tree("stmt")
    getLexemeUnit()
    if(lexemeUnit.getToken() == Token.ASSGM_STMT) {
      tree.add(parseAssgmStmt())
      lexemeUnit = null
    }
     else if(lexemeUnit.getToken() == Token.READ_STMT) {
      tree.add(parseReadStmt())
    lexemeUnit = null
  }
    else if(lexemeUnit.getToken() == Token.WRITE_STMT) {
      tree.add(parseWriteStmt())
      lexemeUnit = null
    }
    else if(lexemeUnit.getToken() == Token.IF_STMT) {
      tree.add(parseIfStmt())
      lexemeUnit = null
    }
    else if(lexemeUnit.getToken() == Token.WHILE_STMT) {
      tree.add(parseWhileStmt())
      lexemeUnit = null
    }
    else if(lexemeUnit.getToken() == Token.BEGIN_STMT) {
      tree.add(parseBlock())
      lexemeUnit = null
    }
    else
      throw new Exception("Syntax Analyzer Error: assignment, read, write, if, while, begin statement expected!")
    // return the tree
    tree
  } // end parseStmt

  // assgm_stmt   = identifier ´:=´ expr
  private def parseAssgmStmt() = {
    val tree = new Tree("assgm_stmt")
    getLexemeUnit()
    if(lexemeUnit.getToken() == Token.IDENTIFIER) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
      if(lexemeUnit.getToken() == Token.ASSGM_STMT) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parseExpr())
      }
      else
        throw new Exception("Syntax Analyzer Error: Assignment statement expected!")
    }
    else
      throw new Exception("Syntax Analyzer Error: Identifier expected!")
    tree
  } // end parseAssgmStmt

  // read_stmt = ´read´ identifier
  private def parseReadStmt() = {
    val tree = new Tree("read_stmt")
    getLexemeUnit()
    if(lexemeUnit.getToken() == Token.READ_STMT) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
      tree.add(parseIdentifier())
    }
    else
      throw new Exception("Syntax Analyzer Error: Read statement expected!")
    tree
  } // end parseReadStmt


} // end SyntaxAnalyzer

object SyntaxAnalyzer {
  def main(args: Array[String]): Unit = {
    // check if source file was passed through the command-line
    if (args.length != 1) {
      print("Missing source file!")
      System.exit(1)
    }

    val syntaxAnalyzer = new SyntaxAnalyzer(args(0))
    val parseTree = syntaxAnalyzer.parse()
    print(parseTree)
  }
}


