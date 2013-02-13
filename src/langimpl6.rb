require 'llvm/core'
require 'llvm/execution_engine'
require 'llvm/transforms/scalar'

require "llvm/core"
require 'llvm/transforms/ipo'
require 'llvm/core/pass_manager'
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

  # control
  TOK_IF = -6
  TOK_THEN = -7
  TOK_ELSE = -8
  TOK_FOR = -9
  TOK_IN = -10

  # operators
  TOK_BINARY = -11
  TOK_UNARY = -12

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
      return TOK_IF if $IdentifierStr == 'if' 
      return TOK_THEN if $IdentifierStr == 'then' 
      return TOK_ELSE if $IdentifierStr == 'else' 
      return TOK_FOR if $IdentifierStr == 'for' 
      return TOK_IN if $IdentifierStr == 'in' 
      return TOK_BINARY if $IdentifierStr == 'binary' 
      return TOK_UNARY if $IdentifierStr == 'unary' 
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

  # UnaryExprAST - Expression class for a unary operator.
  class UnaryExprAST < ExprAST
    attr_accessor :opcode, :operand
    def initialize opcode, operand
      self.opcode, self.operand = opcode, operand
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

  # IfExprAST - Expression class for if/then/else.
  class IfExprAST < ExprAST
    attr_accessor :cond, :thn, :els
    def initialize cond, thn, els
      self.cond, self.thn, self.els = cond, thn, els
    end

    def to_s
      "IfExprAST(#{@cond}, #{@thn}, #{@els})"
    end
  end

  # ForExprAST - Expression class for for/in.
  class ForExprAST < ExprAST
    attr_accessor :var_name, :start, :last, :step, :body
    def initialize var_name, start, last, step, body
      self.var_name, self.start, self.last, self.step, self.body = var_name, start, last, step, body
    end

    def to_s
      "ForExprAST(#{@var_name}, #{@start}, #{@last}, #{@step}, #{@body})"
    end
  end

  # PrototypeAST - This class represents the "prototype" for a function,
  # which captures its name, and its argument names (thus implicitly the number
  # of arguments the function takes), as well as if it is an operator.
  class PrototypeAST < ExprAST
    attr_accessor :name, :args, :is_operator, :precedence
    def initialize name, args, is_operator=false, precedence=0
      self.name, self.args, self.is_operator, self.precedence = name, args, is_operator, precedence
    end

    def unary_op?
      @is_operator and @args.size == 1
    end

    def binary_op?
      @is_operator and @args.size == 2
    end

    def operator_name
      raise 'not operator' unless unary_op? or binary_op?
      @name[-1].to_s
    end

    def binary_precedence
      @precedence
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

  # ifexpr ::= 'if' expression 'then' expression 'else' expression
  def self.ParseIfExpr
    get_next_token # eat the if.

    # condition.
    cond = ParseExpression()
    return 0 if cond == 0

    return Error("expected then: #{$CurTok}") unless $CurTok == TOK_THEN
    get_next_token # eat the then

    thn = ParseExpression()
    return 0 if thn == 0

    return Error("expected else: #{$CurTok}") unless $CurTok == TOK_ELSE

    get_next_token

    els = ParseExpression()
    return 0 if els == 0

    IfExprAST.new cond, thn, els
  end

  # forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
  def self.ParseForExpr
    get_next_token # eat the for.

    return Error("expected identifier after for: #{$CurTok}") unless $CurTok == TOK_IDENTIFIER

    idName = $IdentifierStr
    get_next_token # eat identifier.

    return Error("expected '=' after for: #{$CurTok}") unless $CurTok == '='
    get_next_token # eat '='.

    start = ParseExpression()
    return 0 if start == 0
    return Error("expected ',' after for start value: #{$CurTok}") unless $CurTok == ','
    get_next_token

    last = ParseExpression()
    return 0 if last == 0

    # The step value is optional.
    step = 0
    if $CurTok == ','
      get_next_token
      step = ParseExpression()
      return 0 if step == 0
    end

    return Error("expected 'in' after for: #{$CurTok}") unless $CurTok == TOK_IN
    get_next_token # eat 'in'.

    body = ParseExpression()
    return 0 if body == 0

    return ForExprAST.new(idName, start, last, step, body)
  end

  # primary
  #   ::= identifierexpr
  #   ::= numberexpr
  #   ::= parenexpr
  #   ::= ifexpr
  #   ::= forexpr
  def self.ParsePrimary
    case $CurTok
      when TOK_IDENTIFIER; ParseIdentifierExpr()
      when TOK_NUMBER; ParseNumberExpr()
      when '('; ParseParenExpr()
      when TOK_IF; ParseIfExpr()
      when TOK_FOR; ParseForExpr()
      else Error("unknown token when expecting an expression: #{$CurTok}")
    end
  end

  # unary
  #   ::= primary
  #   ::= '!' unary
  def self.ParseUnary
    # If the current token is not an operator, it must be a primary expr.
    return ParsePrimary() if !isascii($CurTok) || $CurTok == '(' || $CurTok == ','

    # If this is a unary operator, read it.
    opc = $CurTok
    get_next_token
    if operand = ParseUnary()
      return UnaryExprAST.new opc, operand 
    end
    0
  end

  # binoprhs
  #   ::= ('+' unary)*
  def self.ParseBinOpRHS expr_prec, lhs
    loop do
      tok_prec = GetTokPrecedence()

      # If this is a binop that binds at least as tightly as the current binop,
      # consume it, otherwise we are done.
      return lhs if tok_prec < expr_prec

      # Okay, we know this is a binop.
      bin_op = $CurTok
      get_next_token # eat binop

      # Parse the unary expression after the binary operator.
      rhs = ParseUnary()
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
  #   ::= unary binoprhs
  def self.ParseExpression
    lhs = ParseUnary()
    return 0 unless lhs

    ParseBinOpRHS 0, lhs
  end

  # prototype
  #   ::= id '(' id* ')'
  #   ::= binary LETTER number? (id, id)
  #   ::= unary LETTER (id)
  def self.ParsePrototype
    fn_name = nil

    kind = 0 # 0 = identifier, 1 = unary, 2 = binary.
    binary_precedence = 30

    case $CurTok
      when TOK_IDENTIFIER
        fn_name = $IdentifierStr
        kind = 0
        get_next_token
      when TOK_UNARY
        get_next_token
        return ErrorP("Expected unary operator: #{$CurTok}") unless isascii $CurTok
        fn_name = 'unary'
        fn_name += $CurTok
        kind = 1
        get_next_token
      when TOK_BINARY
        get_next_token
        return ErrorP("Expected binary operator: #{$CurTok}") unless isascii $CurTok
        fn_name = 'binary'
        fn_name += $CurTok
        kind = 2
        get_next_token

        # Read the precedence if present.
        if $CurTok == TOK_NUMBER
          return ErrorP("Invalid precedecnce: must be 1..100: #{$NumVal}") if $NumVal < 1 || $NumVal > 100
          binary_precedence = $NumVal
          get_next_token
        end
      else
        return ErrorP("Expected function name in prototype: #{$CurTok}")
    end

    return ErrorP("Expected '(' in prototype: #{$CurTok}") unless $CurTok == '('
    
    arg_names = []
    arg_names.push $IdentifierStr while get_next_token == TOK_IDENTIFIER
    return ErrorP("Expected ')' in prototype: #{$CurTok}") unless $CurTok == ')'

    # success.
    get_next_token # eat ')'.

    # Verify right number of names for operator.
    return ErrorP("Invalid number of operands for operator: #{arg_names.size}") if kind != 0 && arg_names.size != kind

    PrototypeAST.new fn_name, arg_names, kind != 0, binary_precedence
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
  $TheFPM = nil

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

  class UnaryExprAST
    def codegen
      operand_v = @operand.codegen
      return 0 if operand_v == 0

      f = $TheModule.functions["unary#{@opcode}"]
      return ErrorV("Unknown unary operator: #{@opcode}") if f.nil?

      $Builder.call f, operand_v, 'unop'
    end
  end

  class BinaryExprAST
    def codegen
      l = @lhs.codegen
      r = @rhs.codegen
      return 0 if l == 0 or r == 0

      case @op
        when '+'; return $Builder.fadd l, r, 'addtmp'
        when '-'; return $Builder.fsub l, r, 'subtmp'
        when '*'; return $Builder.fmul l, r, 'multmp'
        when '<' 
          l = $Builder.fcmp :ult, l, r, 'cmptmp'
          # Convert bool 0/1 to double 0.0 or 1.0
          return $Builder.ui2fp l, LLVM::Double, 'booltmp'
      end

      # If it wasn't a builtin binary operator, it must be a user defined one. Emit
      # a call to it.
      f = $TheModule.functions["binary#{@op}"]
      #assert(F && "binary operator not found!");

      ops = [l, r]
      $Builder.call f, l, r, 'binop'
    end
  end

  class CallExprAST
    def codegen
      # Look up the name in the global module table.
      callee_f = $TheModule.functions[@callee]
      return ErrorV("Unknown function referenced: #{@callee}") if callee_f == 0 or callee_f.nil?

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

  class IfExprAST
    def codegen
      cond_v = cond.codegen
      return 0 if cond_v == 0

      # Convert condition to a bool by comparing equal to 0.0.
      cond_v = $Builder.fcmp :one, cond_v, LLVM.Float(0.0), 'ifcond'

      the_function = $Builder.insert_block.parent

      # Create blocks for the then and else cases.  Insert the 'then' block at the
      # end of the function.
      then_bb = the_function.basic_blocks.append 'then'
      else_bb = the_function.basic_blocks.append 'else'
      merge_bb = the_function.basic_blocks.append 'ifcont'

      $Builder.cond cond_v, then_bb, else_bb

      # Emit then value.
      $Builder.position then_bb, nil

      then_v = @thn.codegen
      #return 0 if then_v == 0

      $Builder.br merge_bb
      # Codegen of 'Then' can change the current block, update ThenBB for the PHI.
      then_bb = $Builder.insert_block

      # Emit else block.
      $Builder.position else_bb, nil

      else_v = @els.codegen
      #return 0 if else_v == 0

      $Builder.br merge_bb
      # Codegen of 'Else' can change the current block, update ElseBB for the PHI.
      else_bb = $Builder.insert_block

      # Emit merge block.
      $Builder.position merge_bb, nil

      pn = $Builder.phi LLVM::Double, {then_bb => then_v, else_bb => else_v}, 'iftmp'
      pn
    end
  end

  class ForExprAST
    def codegen
      # Output this as:
      #   ...
      #   start = startexpr
      #   goto loop
      # loop:
      #   variable = phi [start, loopheader], [nextvariable, loopend]
      #   ...
      #   bodyexpr
      #   ...
      # loopend:
      #   step = stepexpr
      #   nextvariable = variable + step
      #   endcond = endexpr
      #   br endcond, loop, endloop
      # outloop:

      # Emit the start code first, without 'variable' in scope.
      start_val = @start.codegen
      return 0 if start_val == 0

      # Make the new basic block for the loop header, inserting after current
      # block.
      the_function = $Builder.insert_block.parent
      preheader_bb = $Builder.insert_block
      loop_bb = the_function.basic_blocks.append 'loop'

      # Insert an explicit fall through from the current block to the LoopBB.
      $Builder.br loop_bb

      # Start insertion in LoopBB.
      $Builder.position loop_bb, nil

      # Start the PHI node with an entry for Start.
      variable = $Builder.phi LLVM::Double, {preheader_bb => start_val}, @var_name

      # Within the loop, the variable is defined equal to the PHI node.  If it
      # shadows an existing variable, we have to restore it, so save it now.
      old_val = $NamedValues[@var_name]
      $NamedValues[@var_name] = variable

      # Emit the body of the loop.  This, like any other expr, can change the
      # current BB.  Note that we ignore the value computed by the body, but don't
      # allow an error.
      return 0 if @body.codegen == 0

      # Emit the step value.
      step_val = nil
      if @step
        step_val = @step.codegen
        return 0 if step_val == 0
      else
        # If not specified, use 1.0.
        step_val = 1.0
      end

      next_var = $Builder.fadd variable, step_val, 'nextvar'

      # Compute the end condition.
      end_cond = last.codegen
      return 0 if end_cond == 0

      # Convert condition to a bool by comparing equal to 0.0.
      end_cond = $Builder.fcmp :one, end_cond, LLVM::Float(0.0), 'loopcond'

      # Create the "after loop" block and insert it.
      loop_end_bb = $Builder.insert_block
      after_bb = the_function.basic_blocks.append 'afterloop'

      # Insert the conditional branch into the end of LoopEndBB.
      $Builder.cond end_cond, loop_bb, after_bb

      # Any new code will be inserted in AfterBB.
      $Builder.position after_bb, nil

      # Add a new entry to the PHI node for the backedge.
      variable.add_incoming loop_end_bb => next_var

      # Restore the unshadowed variable.
      if old_val
        $NamedValues[@var_name] = old_val
      else
        $NamedValues.delete @var_name
      end

      # for expr always returns 0.0.
      LLVM::Constant.null LLVM::Double
    end
  end

  class PrototypeAST
    def codegen
      # Make the function type:  double(double,double) etc.
      $TheModule.functions.add(@name, [LLVM::Double] * @args.size, LLVM::Double) do |f, n|
        # If F conflicted, there was already something named 'Name'.  If it has a
        # body, don't allow redefinition or reextern.
        if f.name != @name
          # TODO
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

      # If this is an operator, install it.
      $BinopPrecedence[@proto.operator_name] = @proto.binary_precedence if @proto.binary_op?

      # Create a new basic block to start insertion into.
      the_function.basic_blocks.append('entry').build do |builder|
        $Builder = builder

        if ret_val = @body.codegen
          # Finish off the function.
          $Builder.ret ret_val

          # Validate the generated code, checking for consistency.
          the_function.verify

          # Optimize the function.
          $TheFPM.run the_function

          return the_function
        end

        # Error reading body, remove function.
        LLVM::C.delete_function the_function

        $BinopPrecedence.delete $proto.operator_name if @proto.binary_op?
        0
      end
    end
  end

  #===----------------------------------------------------------------------===//
  # Top-Level parsing and JIT Driver
  #===----------------------------------------------------------------------===//

  $TheExecutionEngine = nil

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

        # JIT the function, returning a function pointer.
        #void *FPtr = TheExecutionEngine->getPointerToFunction(LF);

        # Cast it to the right type (takes no arguments, returns a double) so we
        # can call it as a native function.
        $stderr.print "Evaluated to #{
          $TheExecutionEngine.run_function(lf).to_f(LLVM::Double)}\n"
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
    #InitializeNativeTarget();
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

    # Create the JIT.  This takes ownership of the module.
    $TheExecutionEngine = LLVM::JITCompiler.new $TheModule

    our_fpm = LLVM::FunctionPassManager.new $TheExecutionEngine, $TheModule

    # Set up the optimizer pipeline.  Start with registering info about how the
    # target lays out data structures.
    #OurFPM.add(new DataLayout(*TheExecutionEngine->getDataLayout()));
    # Provide basic AliasAnalysis support for GVN.
    LLVM::C.add_basic_alias_analysis_pass our_fpm
    # Do simple "peephole" optimizations and bit-twiddling optzns.
    our_fpm.instcombine!
    # Reassociate expressions.
    our_fpm.reassociate!
    # Eliminate Common SubExpressions.
    our_fpm.gvn!
    # Simplify the control flow graph (deleting unreachable blocks, etc).
    our_fpm.simplifycfg!

    # Set the global so the code gen can use this.
    $TheFPM = our_fpm

    # Run the main "interpreter loop" now.
    MainLoop()

    $TheFPM = 0

    # Print out all of the generated code.
    $TheModule.dump

    return 0;
  end
end

Toy.main
