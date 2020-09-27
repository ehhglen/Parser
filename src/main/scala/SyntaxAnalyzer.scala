/*
 * CS3210 - Principles of Programming Languages - Fall 2020
 * Instructor: Thyago Mota
 * Description: Prg01 - Syntax Analyzer
 * Student(s) Name(s): Echglene Woy
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
      if (lexemeUnit.getToken() == Token.VAR){
        throw new Exception("Syntax Analyzer Error: Identifier expected!")
      }
      tree.add(parseIdentifier())
      tree.add(parseBody())
        if (lexemeUnit.getToken() == Token.PERIOD) {
          tree.add(new Tree(lexemeUnit.getLexeme()))
          lexemeUnit = null
          getLexemeUnit()
          if (lexemeUnit.getToken() == Token.IDENTIFIER){
            throw new Exception ("Syntax Analyzer Error: EOF expected!")

          }
        }
        else
          throw new Exception("Syntax Analyzer Error: period expected!")
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
    if (lexemeUnit.getToken() != Token.EOF) {
      tree.add(parseVar_Sct())
      tree.add(parseBlock())
    } else
      throw new Exception("Syntax Analyzer Error: body expected!")
    tree
  }

  // TODOd: var_sct = ´var´ var_dcl { ´;´ var_dcl }
  private def parseVar_Sct() = {
    val tree = new Tree("var_sct")
    lexemeUnit = null
    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.VAR){
      throw new Exception ("Syntax Analyzer Error: begin expected!")
    }

    else if (lexemeUnit.getToken() == Token.VAR) {
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
    tree
  } // end parseVar_Sct

  // TODOd: var_dcl = identifier { identifier } ´:´ type
  private def parseVar_Dcl() = {
    val tree = new Tree("var_dcl")
    getLexemeUnit()
    var done = false
    while(!done) {
      if(lexemeUnit.getToken() == Token.IDENTIFIER) {
        tree.add(parseIdentifier())
        lexemeUnit = null
        getLexemeUnit()
        if (lexemeUnit.getToken() == Token.INTEGER || lexemeUnit.getToken() == Token.BOOLEAN){
          throw new Exception ("Syntax Analyzer Error: colon expected!")
        }
      }
      else
        done = true
    }
    if(lexemeUnit.getToken() == Token.COLON || lexemeUnit.getToken() == Token.ASSGM_STMT) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
      tree.add(parseType())
    }
    lexemeUnit = null
    getLexemeUnit()
    tree
  } // end parseVar_Dcl

  // TODO: type = ´Integer´ | ´Boolean´
  private def parseType() = {
    val tree = new Tree("type")
    getLexemeUnit()

    if(lexemeUnit.getToken() != Token.EOF) {
      if(lexemeUnit.getToken() == Token.INTEGER || lexemeUnit.getToken() == Token.BOOLEAN ) { // WAS Token.BOOLEAN
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
      }
//      else
//        throw new Exception("Syntax Analyzer Error: type expected!") // TODO: Won't run source 3 unless boolean gets resolved
    }
    tree
  }

  // block = ´begin´ stmt { ´;´ stmt } ´end´
  private def parseBlock():Tree = {
    val tree = new Tree("block")
    getLexemeUnit()

    if(lexemeUnit.getToken() != Token.EOF) {
      if(lexemeUnit.getToken() == Token.BEGIN_STMT) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parseStmt())

        var done = false
        while (!done) {
          tree.add(parseStmt())
          if (lexemeUnit.getToken() == Token.SEMI_COL) {
            tree.add(new Tree(lexemeUnit.getLexeme()))
            lexemeUnit = null
            getLexemeUnit()
          }
          else
            done = true
        }
        if(lexemeUnit.getToken() == Token.END) {
          tree.add(new Tree(lexemeUnit.getLexeme()))
          lexemeUnit = null
          getLexemeUnit()
        }
//        else {
//          throw new Exception("Syntax Analyzer Error: end expected!")
//        }
      }


    }
    tree
  } // end parseBlock

  // stmt = assgm_stmt | read_stmt | write_stmt | if_stmt | while_stmt | block
  private def parseStmt(): Tree= {
    val tree = new Tree("stmt")
    getLexemeUnit()
    if(lexemeUnit.getToken() == Token.IDENTIFIER) {
      tree.add(parseAssgmStmt())
      //lexemeUnit = null
    }
    if(lexemeUnit.getToken() == Token.READ_STMT) {
      tree.add(parseReadStmt())
    //lexemeUnit = null
    }
    if(lexemeUnit.getToken() == Token.WRITE_STMT) {
      tree.add(parseWriteStmt())
      //lexemeUnit = null
    }
    if(lexemeUnit.getToken() == Token.IF_STMT) {
      tree.add(parseIfStmt())
      //lexemeUnit = null
    }
    if(lexemeUnit.getToken() == Token.WHILE_STMT) {
      tree.add(parseWhileStmt())
      //lexemeUnit = null
    }
    if(lexemeUnit.getToken() == Token.BEGIN_STMT) {
      tree.add(parseBlock())
      lexemeUnit = null
      getLexemeUnit()
    }
//    else
//      throw new Exception("Syntax Analyzer Error: assignment, read, write, if, while, begin statement expected!")
    // return the tree
    tree
  } // end parseStmt

  // assgm_stmt   = identifier ´:=´ expr
  private def parseAssgmStmt() = {
    val tree = new Tree("assgm_stmt")
    getLexemeUnit()
    if(lexemeUnit.getToken() == Token.IDENTIFIER) {
      tree.add(parseIdentifier())
      lexemeUnit = null
      getLexemeUnit()
      if(lexemeUnit.getToken() == Token.ASSGM_STMT) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parseExpr())
      }
      else
        throw new Exception("Syntax Analyzer Error: assignment expected!")
    }
//    else
//      throw new Exception("Syntax Analyzer Error: Identifier expected!")
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
      lexemeUnit = null
      getLexemeUnit()
    }
    else
      throw new Exception("Syntax Analyzer Error: Read statement expected!")
    tree
  } // end parseReadStmt

  // write_stmt = ´write´ ( identifier | literal )
  private def parseWriteStmt() = {
    val tree = new Tree("write_stmt")
    getLexemeUnit()

    if(lexemeUnit.getToken() == Token.WRITE_STMT) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
      if(lexemeUnit.getToken() == Token.IDENTIFIER) {
        tree.add(parseIdentifier())
        lexemeUnit = null
        getLexemeUnit()
      }
      else if(lexemeUnit.getToken() == Token.LITERAL) {
        tree.add(parseLiteral())
        lexemeUnit = null
        getLexemeUnit()
      }
      else
        throw new Exception("Syntax Analyzer Error: Identifier or literal expected!")
    }
    else {
      throw new Exception("Syntax Analyzer Error: Write statement expected!")
    }
    tree
  } // end parseWriteStmt

  // if_stmt = ´if´ bool_expr ´then´ stmt [ ´else´ stmt ]
  private def parseIfStmt() = {
    // TODOd: finish this function
    val tree = new Tree("if_stmt")
    getLexemeUnit()

    if(lexemeUnit.getToken() != Token.EOF) {

      if(lexemeUnit.getToken() == Token.IF_STMT) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parseBoolExpr())
        if(lexemeUnit.getToken() == Token.THEN_STMT) {
          tree.add(new Tree(lexemeUnit.getLexeme()))
          lexemeUnit = null
          getLexemeUnit()
          tree.add(parseStmt())
          if (lexemeUnit.getToken() == Token.ELSE_STMT) {
            tree.add(new Tree(lexemeUnit.getLexeme()))
            lexemeUnit = null
            getLexemeUnit()
            tree.add(parseStmt())
          }
          //        tree.add(new Tree(lexemeUnit.getLexeme())) // testing
          //        tree.add(parseStmt())
          //        tree.add(parseBoolExpr())
        }
      }
      else{
        throw new Exception("Syntax Analyzer Error: then expected!")}

//      if(lexemeUnit.getToken() == Token.ELSE_STMT) {
//        tree.add(new Tree(lexemeUnit.getLexeme()))
//        lexemeUnit = null
//        getLexemeUnit()
//        tree.add(parseStmt())
//      }
    }
    tree
  } // end parseIfStmt

  private def parseWhileStmt() = {
    // TODO: finish this function
    val tree = new Tree("while_stmnt")
    getLexemeUnit()
    if (lexemeUnit.getToken() == Token.WHILE_STMT) {
      tree.add(new Tree(lexemeUnit.getLexeme()))
      lexemeUnit = null
      getLexemeUnit()
      tree.add(parseBoolExpr())
      if (lexemeUnit.getToken() == Token.DO_STMT) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
      }
      else
        throw new Exception("Syntax Analyzer Error: do expected!")

    }

    else
      throw new Exception("Syntax Analyzer Error: \"if\" expected!")
    tree
  } // end parseWhileStmt



  // literal = int_literal | bool_literal
  private def parseLiteral() = {
    val tree = new Tree("literal")
    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      if(lexemeUnit.getToken() == Token.INTEGER) {
        tree.add(parseIntLiteral())
        lexemeUnit = null
      }
      else if(lexemeUnit.getToken() == Token.BOOLEAN) {
        tree.add(parseBoolLiteral())
        lexemeUnit = null
      }
      else
        throw new Exception("Syntax Analyzer Error: Boolean literal expected!")
    }
    else
      throw new Exception("Syntax Analyzer Error: Integer literal expected!")

    tree
  } // end parseLiteral()

  private def parseIntLiteral() = new Tree("int_literal: '" + lexemeUnit.getLexeme() + "'")

  // int_literal = digit { digit }
//  private def parseIntLiteral() = {
//    val tree = new Tree("int_literal: '" + lexemeUnit.getLexeme() + "'")
//    lexemeUnit = null
//    getLexemeUnit()
//
//    tree
//  }
  // bool_literal = ´true´ | ´false´
  private def parseBoolLiteral() = {
    val tree = new Tree("bool_literal")
    getLexemeUnit()
    if(lexemeUnit.getToken() != Token.EOF) {
      if(lexemeUnit.getToken() == Token.TRUE || lexemeUnit.getToken() == Token.FALSE) { // Maybe need to split Token.BOOLEAN to Token.True || Token.False?
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
      }
//      else
//        throw new Exception("Syntax Analyzer Error: True or False Expected!")
    }
    tree
  } // end parseBoolLiteral

  // expr = arithm_expr | bool_expr
  private def parseExpr() = {
    val tree = new Tree("expr")
    getLexemeUnit()
    if (lexemeUnit.getToken() != Token.EOF) {
      // TODOd: if token is an arithmetic_expr, add result of "parseArithExpr" as new branch and reset lexemeUnit
      if (lexemeUnit.getToken() == Token.ARITHMETIC_EXPR || lexemeUnit.getToken() == Token.IDENTIFIER || lexemeUnit.getToken() == Token.INT_LITERAL) { // this should be arithm_expr?
        tree.add(parseArithExpr())
        //lexemeUnit = null // always set lexemeUnit to null after consuming it // if uncomment, must use else if below
      }
//      else if(lexemeUnit.getToken() == Token.IDENTIFIER) {
//        tree.add(parseArithExpr())
//      }
      // TODOd: if token is a bool_expr, add result of "parseBoolExpr" as new branch and reset lexemeUnit
      else if (lexemeUnit.getToken() == Token.TRUE  || lexemeUnit.getToken() == Token.FALSE) {
        tree.add(parseBoolExpr())
      }
      else
        throw new Exception("Syntax Analyzer Error: identifier or (int) literal expected!")
    }
    tree
  } // end parseExpr

  // arithm_expr = term arithm_expr'
  private def parseArithExpr() = {
    val tree = new Tree("arithm_expr")
    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      tree.add(parseTerm())
      tree.add(parseArithExprPrime())
    }
    else
      throw new Exception("Syntax Analyzer Error: Arithmetic Expression expected!")
    tree
  } // end parseArithExpr

  //term = term ´*´ factor | factor
  private def parseTerm() = {
    // TODOd: create a tree with label "term"
    val tree = new Tree("term")
    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      tree.add(parseFactor())
      tree.add(parseTermPrime())
    }
    else
      throw new Exception("Syntax Analyzer Error: factor expected!")

    tree
  } // end parseTerm
  private def parseFactor(): Tree = {
    val tree = new Tree("factor")
    getLexemeUnit()

    // TODOd: if token is NOT EOF
    if (lexemeUnit.getToken() != Token.EOF) {
      // TODOd: if token is an identifier, add result of "parseIdentifier" as new branch and reset lexemeUnit
      if (lexemeUnit.getToken() == Token.IDENTIFIER) {
        tree.add(parseIdentifier())
        lexemeUnit = null // always set lexemeUnit to null after consuming it
      }
      // TODOd: if token is a Int_literal, add result of "parseLiteral" as new branch and reset lexemeUnit
      else if (lexemeUnit.getToken() == Token.INT_LITERAL) { // maybe this needs to be INT_LITERAL?
        tree.add(parseIntLiteral())
        lexemeUnit = null // always set lexemeUnit to null after consuming it
      }

    }
    // TODOd: otherwise, throw an exception saying that "identifier, literal or opening parenthesis" was expected
    else
      throw new Exception("Syntax Analyzer Error: identifier or \"(int) literal\" expected!")

    // TODOd: return the tree
    tree
  } // end parseFactor

  // term = term ´*´ factor | factor
  private def parseTermPrime(): Tree = {
    val tree = new Tree("term'")
    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      if (lexemeUnit.getToken() == Token.MUL_OP) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null // always set lexemeUnit to null after consuming it
        tree.add(parseFactor())
        tree.add(parseTermPrime())


      }
      // else means "epsilon" production
    }
    // TODOd: return the tree
    tree
  } // end parseTermPrime

  // arithm_expr  = arithm_expr ( ´+´ | ´-´ ) term | term
  private def parseArithExprPrime(): Tree = {
    val tree = new Tree("arithm_expr'")
    getLexemeUnit()

    if (lexemeUnit.getToken() != Token.EOF) {
      // TODO: if token is "+" or "-", add token as new branch and reset lexemeUnit;
      //  then add result of "parseTerm" and "parseExpressionPrime" as new branches
      if (lexemeUnit.getToken() == Token.ADD_OP || lexemeUnit.getToken() == Token.SUB_OP) {
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null // always set lexemeUnit to null after consuming it
        tree.add(parseTerm())
        tree.add(parseArithExprPrime())
      }
    }

//    var done = false
//    while (!done){
//      getLexemeUnit()
//      if (lexemeUnit.getToken() != Token.EOF) {
//        if (lexemeUnit.getToken() == Token.ADD_OP || lexemeUnit.getToken() == Token.SUB_OP) {
//          tree.add(new Tree(lexemeUnit.getLexeme()))
//          lexemeUnit = null // always set lexemeUnit to null after consuming it
//          tree.add(parseTerm())
//          tree.add(parseArithExprPrime())
//
//        }
//        // else means "epsilon" production
//        else {
//          done = true
//        }
//      }
//    }
    tree
  } // end parseArithExprPrime

  // bool_expr  = bool_literal | arithm_expr ( ´>´ | ´>=´ | ´=´ | ´<=´ | ´<´ ) arithm_expr
  private def parseBoolExpr() = {
    val tree = new Tree("bool_expr")
    getLexemeUnit()

    if(lexemeUnit.getToken() == Token.IDENTIFIER) {
      tree.add(parseArithExpr())
      getLexemeUnit()
      if(lexemeUnit.getToken() == Token.ARITHMETIC_EXPR){
        tree.add(new Tree(lexemeUnit.getLexeme()))
        lexemeUnit = null
        getLexemeUnit()
        tree.add(parseArithExpr())
      }
      else{
        throw new Exception ("Syntax Analyzer Error: relational operator expected!")
      }

    }
    if(lexemeUnit.getToken() == Token.TRUE || lexemeUnit.getToken() == Token.FALSE){
      //        tree.add(new Tree(lexemeUnit.getLexeme()))
      //        lexemeUnit = null
      //        getLexemeUnit()
      tree.add(parseBoolLiteral())
    }
    // return the tree
    tree
  }

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


