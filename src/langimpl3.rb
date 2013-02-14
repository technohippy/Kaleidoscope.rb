require 'llvm/core'
require 'llvm/execution_engine'
require 'llvm/transforms/scalar'
require_relative './compat'

LLVM.init_x86

# Error* - These are little helper functions for error handling.
def Error  str; $stderr.print "Error: #{str}\n"; 0 end
def ErrorP str; Error(str) end
def ErrorF str; Error(str) end
def ErrorV str; Error(str) end

module Toy

  #===----------------------------------------------------------------------===//
  # Lexer
  #===----------------------------------------------------------------------===//

  # The lexer returns tokens [0-255] if it is an unknown character, otherwise one
  # of these for known things.
  TOK_EOF = -1

  # commands
  TOK_DEF = -2
  TOK_EXTERN = -3

  # primary
  TOK_IDENTIFIER = -4
  TOK_NUMBER = -5

  $IdentifierStr = nil # Filled in if TOK_IDENTIFIER
  $NumVal = 0          # Filled in if TOK_NUMBER

  # gettok - Return the next token from standard input.
  def self.gettok
    $LastChar ||= ' '

    # Skip any whitespace.
    $LastChar = getchar while isspace $LastChar

    if isalpha $LastChar # identifier: [a-zA-Z][a-zA-Z0-9]*
      $IdentifierStr = $LastChar
      $IdentifierStr += $LastChar while isalnum($LastChar = getchar)

      return TOK_DEF if $IdentifierStr == 'def'
      return TOK_EXTERN if $IdentifierStr == 'extern'
      return TOK_IDENTIFIER
    end

    if isdigit($LastChar) or $LastChar == '.' # Number: [0-9.]+
      num_str = ''
      begin
        num_str += $LastChar
        $LastChar = getchar
      end while isdigit($LastChar) or $LastChar == '.'

      $NumVal = num_str.to_f
      return TOK_NUMBER
    end

    if $LastChar == '#'
      # Comment until end of line.
      begin $LastChar = getchar
      end while $LastChar != EOF and $LastChar != "\n" and $LastChar != "\r"

      return gettok unless $LastChar == EOF
    end

    # Check for end of file.  Don't eat the EOF.
    return TOK_EOF if $LastChar == EOF

    # Otherwise, just return the character as its ascii value.
    this_char = $LastChar
    $LastChar = getchar
    this_char
  end

  #===----------------------------------------------------------------------===//
  # Abstract Syntax Tree (aka Parse Tree)
  #===----------------------------------------------------------------------===//

  # ExprAST - Base class for all expression nodes.
  class ExprAST
  end

  # NumberExprAST - Expression class for numeric literals like "1.0".
  class NumberExprAST < ExprAST
    attr_accessor :val
    def initialize val 
      self.val = val
    end

    def to_s
      "NumberExprAST(#{@val})"
    end
  end

  # VariableExprAST - Expression class for referencing a variable, like "a".
  class VariableExprAST < ExprAST
    attr_accessor :name
    def initialize name
      self.name = name
    end

    def to_s
      "VariableExprAST(#{@name})"
    end
  end

  # BinaryExprAST - Expression class for a binary operator.
  class BinaryExprAST < ExprAST
    attr_accessor :op, :lhs, :rhs
    def initialize op, lhs, rhs
      self.op, self.lhs, self.rhs = op, lhs, rhs
    end

    def to_s
      "BinaryExprAST(#{@op}, #{@lhs}, #{@rhs})"
    end
  end

  # CallExprAST - Expression class for function calls.
  class CallExprAST < ExprAST
    attr_accessor :callee, :args
    def initialize callee, args
      self.callee, self.args = callee, args
    end

    def to_s
      "CallExprAST(#{@callee}, #{@args})"
    end
  end

  # PrototypeAST - This class represents the "prototype" for a function,
  # which captures its name, and its argument names (thus implicitly the number
  # of arguments the function takes).
  class PrototypeAST < ExprAST
    attr_accessor :name, :args
    def initialize name, args
      self.name, self.args = name, args
    end

    def to_s
      "PrototypeExprAST(#{@name}, #{@args})"
    end
  end

  # FunctionAST - This class represents a function definition itself.
  class FunctionAST < ExprAST
    attr_accessor :proto, :body
    def initialize proto, body
      self.proto, self.body = proto, body
    end

    def to_s
      "FuntionAST(#{@proto}, #{@body})"
    end
  end

  #===----------------------------------------------------------------------===#
  # Parser
  #===----------------------------------------------------------------------===#

  # CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
  # token the parser is looking at.  getNextToken reads another token from the
  # lexer and updates CurTok with its results.
  $CurTok = nil
  def self.get_next_token
    $CurTok = gettok
  end

  # BinopPrecedence - This holds the precedence for each binary operator that is
  # defined.
  $BinopPrecedence = {}

  # GetTokPrecedence - Get the precedence of the pending binary operator token.
  def self.GetTokPrecedence
    return -1 unless isascii $CurTok

    # Make sure it's a declared binop.
    tok_prec = $BinopPrecedence[$CurTok]
    return -1 if tok_prec.nil? or tok_prec <= 0
    tok_prec
  end

  # identifierexpr
  #   ::= identifier
  #   ::= identifier '(' expression* ')'
  def self.ParseIdentifierExpr
    idName = $IdentifierStr

    get_next_token # eat identifier.

    return VariableExprAST.new idName unless $CurTok == '(' # Simple variable ref.

    # Call.
    get_next_token # eat (
    args = []
    unless $CurTok == ')'
      loop do
        arg = ParseExpression()
        return 0 unless arg
        args.push arg

        break if $CurTok == ')'

        return Error("Expected ')' or ',' in argument list") unless $CurTok == ','
        get_next_token
      end
    end

    # Eat the ')'.
    get_next_token

    CallExprAST.new idName, args
  end

  # numberexpr ::= number
  def self.ParseNumberExpr
    result = NumberExprAST.new $NumVal
    get_next_token
    result
  end

  # parenexpr ::= '(' expression ')'
  def self.ParseParenExpr
    get_next_token # eat (.
    v = ParseExpression()
    return 0 unless v

    return Error("expected ')'") unless $CurTok == ')'
    get_next_token # eat ).
    v
  end

  # primary
  #   ::= identifierexpr
  #   ::= numberexpr
  #   ::= parenexpr
  def self.ParsePrimary
    case $CurTok
      when TOK_IDENTIFIER; ParseIdentifierExpr()
      when TOK_NUMBER; ParseNumberExpr()
      when '('; ParseParenExpr()
      else Error("unknown token when expecting an expression: #{$CurTok}")
    end
  end

  # binoprhs
  #   ::= ('+' primary)*
  def self.ParseBinOpRHS expr_prec, lhs
    loop do
      tok_prec = GetTokPrecedence()

      # If this is a binop that binds at least as tightly as the current binop,
      # consume it, otherwise we are done.
      return lhs if tok_prec < expr_prec

      # Okay, we know this is a binop.
      bin_op = $CurTok
      get_next_token # eat binop

      # Parse the primary expression after the binary operator.
      rhs = ParsePrimary()
      return 0 unless rhs

      # If BinOp binds less tightly with RHS than the operator after RHS, let
      # the pending operator take RHS as its LHS.
      next_prec = GetTokPrecedence()
      if tok_prec < next_prec
        rhs = ParseBinOpRHS tok_prec + 1, rhs
        return 0 if rhs == 0
      end

      # Merge LHS/RHS.
      lhs = BinaryExprAST.new bin_op, lhs, rhs
    end
  end

  # expression
  #   ::= primary binoprhs
  def self.ParseExpression
    lhs = ParsePrimary()
    return 0 unless lhs

    ParseBinOpRHS 0, lhs
  end

  # prototype
  #   ::= id '(' id* ')'
  def self.ParsePrototype
    return ErrorP('Expected function name in prototype') unless $CurTok == TOK_IDENTIFIER

    fn_name = $IdentifierStr
    get_next_token

    return ErrorP("Expected '(' in prototype") unless $CurTok == '('

    arg_names = []
    arg_names << $IdentifierStr while get_next_token == TOK_IDENTIFIER
    return ErrorP("Expected ')' in prototype") unless $CurTok == ')'

    # success.
    get_next_token # eat ')'.

    PrototypeAST.new fn_name, arg_names
  end

  # definition ::= 'def' prototype expression
  def self.ParseDefinition
    get_next_token # eat def.
    proto = ParsePrototype()
    return 0 if proto == 0

    if e = ParseExpression()
      return FunctionAST.new(proto, e)
    end
    return 0
  end

  # toplevelexpr ::= expression
  def self.ParseTopLevelExpr
    if e = ParseExpression()
      proto = PrototypeAST.new '', []
      return FunctionAST.new proto, e
    end
    0
  end

  # external ::= 'extern' prototype
  def self.ParseExtern
    get_next_token # eat extern.
    ParsePrototype()
  end

  #===----------------------------------------------------------------------===#
  # Code Generation
  #===----------------------------------------------------------------------===#
  $TheModule = nil
  $Builder = nil
  $NamedValues = {}

  class NumberExprAST
    def codegen
      LLVM::Double.from_f @val
    end
  end

  class VariableExprAST
    def codegen
      # Look this variable up in the function.
      v = $NamedValues[@name]
      v ? v : ErrorV("Unknown variable name: #{@name}")
    end
  end

  class BinaryExprAST
    def codegen
      l = @lhs.codegen
      r = @rhs.codegen
      return 0 if l == 0 or r == 0

      case @op
        when '+'; $Builder.fadd l, r, 'addtmp'
        when '-'; $Builder.fsub l, r, 'subtmp'
        when '*'; $Builder.fmul l, r, 'multmp'
        when '<' 
          l = $Builder.fcmp l, r, 'cmptmp'
          # Convert bool 0/1 to double 0.0 or 1.0
          $Builder.ui2fp l, :double, 'booltmp'
        else; ErrorV("invalid binary operator: #{@op}")
      end
    end
  end

  class CallExprAST
    def codegen
      # Look up the name in the global module table.
      callee_f = $TheModule.functions[@callee]
      return ErrorV('Unknown function referenced') if callee_f == 0

      # If argument mismatch error.
      return ErrorV('Incorrect # arguments passed') if callee_f.params.size != @args.size

      args_v = []
      @args.each do |arg|
        args_v << arg.codegen
        return 0 if args_v.last == 0
      end

      $Builder.call callee_f, *args_v
    end
  end

  class PrototypeAST
    def codegen
      # Make the function type:  double(double,double) etc.
      $TheModule.functions.add(@name, [LLVM::Double] * @args.size, LLVM::Double) do |f, n|
        # If F conflicted, there was already something named 'Name'.  If it has a
        # body, don't allow redefinition or reextern.
        if f.name != @name
          # Delete the one we just made and get the existing one.
          $TheModule.functions.delete f
          f = $TheModule.functions[@name]

          # If F already has a body, reject this.
          unless f.basic_blocks.size == 0
            ErrorF("redefinition of function")
            return 0
          end

          # If F took a different number of args, reject.
          unless f.params.size == @args.size
            ErrorF("redefinition of function with different # args")
            return 0
          end
        end

        # Set names for all arguments.
        @args.each_with_index do |arg, i|
          ai = f.params[i]
          ai.name = arg

          # Add arguments to variable symbol table.
          $NamedValues[arg] = ai
        end

        f
      end
    end
  end

  class FunctionAST
    def codegen
      $NamedValues.clear

      the_function = @proto.codegen
      return 0 if the_function == 0

      # Create a new basic block to start insertion into.
      the_function.basic_blocks.append('entry').build do |builder|
        $Builder = builder

        if ret_val = @body.codegen
          # Finish off the function.
          $Builder.ret ret_val

          # Validate the generated code, checking for consistency.
          #verifyFunction(*TheFunction);

          return the_function
        end

        # Error reading body, remove function.
        LLVM::C.delete_function the_function
        0
      end
    end
  end

  #===----------------------------------------------------------------------===//
  # Top-Level parsing and JIT Driver
  #===----------------------------------------------------------------------===//

  def self.HandleDefinition
    if f = ParseDefinition()
      if lf = f.codegen
        $stderr.print 'Read function definition:'
        lf.dump
      end
    else
      # Skip token for error recovery.
      get_next_token
    end
  end

  def self.HandleExtern
    if f = ParseExtern()
      if lf = f.codegen
        $stderr.print 'Read extern:'
        lf.dump
      end
    else
      # Skip token for error recovery.
      get_next_token
    end
  end

  def self.HandleTopLevelExpression
    # Evaluate a top-level expression into an anonymous function.
    if f = ParseTopLevelExpr()
      if lf = f.codegen
        $stderr.print 'Read top-level expression:'
        lf.dump
      end
    else
      # Skip token for error recovery.
      get_next_token
    end
  end

  def self.MainLoop
    loop do
      $stderr.print 'ready> '
      case $CurTok
        when TOK_EOF;    return
        when ';';        get_next_token # ignore top-level semicolons.
        when TOK_DEF;    HandleDefinition()
        when TOK_EXTERN; HandleExtern()
        else             HandleTopLevelExpression()
      end
    end
  end

  #===----------------------------------------------------------------------===//
  # Main driver code.
  #===----------------------------------------------------------------------===//
  def self.main
    #LLVMContext &Context = getGlobalContext();

    # Install standard binary operators.
    # 1 is lowest precedence.
    $BinopPrecedence['<'] = 10
    $BinopPrecedence['+'] = 20
    $BinopPrecedence['-'] = 30
    $BinopPrecedence['*'] = 40 # highest.

    # Prime the first token.
    $stderr.print 'ready> '
    get_next_token

    # Make the module, which holds all the code.
    $TheModule = LLVM::Module.new "my cool jit"

    # Run the main "interpreter loop" now.
    MainLoop()

    # Print out all of the generated code.
    $TheModule.dump

    return 0;
  end
end

Toy.main
