' Toy+ - Console version, integer version, includes math functions.
' by Ed_Davis - 2004 - 2019
'
' This is based on my Visual Basic Script version of Toy - circa 2004
' Which is in turn based on my C version of Toy - circa 2003
' There are also versions for Euphoria, BCX and Pascal
'------------------------------------------------------------------------
'  fbc -s gui toygt.bas
'------------------------------------------------------------------------
'
' Grammar:
'pgm = [subs|functions] stmt_seq [subs|functions] .
'sub = "sub" ident optional_parms stmt_seq "end sub" .
'subs = sub {sub} .
'function = "function" ident optional_parms stmt_seq "end function" .
'functions = function {function} .
'optional_parms = ["(" [float ident {"," ident}] ")"] .
'stmt_seq = {print_stmt | halt_stmt | while_stmt | assign | if_stmt | for_stmt | decl | return_stmt | call_stmt} .
'decl = "float" ident {"," ident} .
'print_stmt = "print" (string | expr) {, (string | expr) } .
'halt_stmt = "halt" .
'while_stmt = "while" expr stmt_seq "wend" .
'for_stmt = "for" ident = expr1 to expr2 [step expr3] stmt_seq next.
'assign = ident "=" expr .
'if_stmt = "if" expr "then" stmt_seq {"elseif" expr "then" stmt_seq} ["else" stmt_seq] "end if" .
'return_stmt = "return" [expr]
'call_stmt = "call" ident optional_args
'optional_args = ["(" [expr {"," expr}] ")"] .
'exp    = orexp .
'orexp  = andexp {"or" andexp} .
'andexp = eqlexp {"and" eqlexp} .
'eqlexp = relexp {eqlop relexp} .
'relexp = addexp {relop addexp} .
'addexp = mulexp {addop mulexp} .
'mulexp = factor {mulop factor} .
'factor = '(' exp ')' | number | ident | fun([parms]) .
'unary_exp  = "not" | "-" .
'eqlop = "=" | "<>"
'relop = "<" | "<=" | ">" | ">="
'addop = "+" | "-"
'mulop = "*" | "/" | "mod" | "\" (integer division) .
'
'Comments are denoted by the single quote, and extend until the end-of-line
'
'Math-functions ->cos(n),sin(n),tan(n),sqr(n),log(n),rnd(n),rand(n),int(n),abs(n),atn(n),rgb(r,g,b)
'
' example program - find primes:
'
' primes(100)
'
' sub primes(float lim)
'   float n, k, p
'   n = 1
'   while n < lim
'       k = 3
'       p = 1
'       n = n + 2
'       while k * k <= n and p
'           p = n \ k * k <> n
'           k = k + 2
'       wend
'       if p then
'           print n, " is prime"
'       end if
'   wend
' end sub
'
' example program - fibonacci numbers
'
' print fib(10)
'
' function fib(float n)
'     if n < 2 then
'         return n
'     end if
'
'     return fib(n - 1) + fib(n - 2)
' end function
'
'
' example program - towers of Hanoi
'
' float FROM_PEG, TO_PEG, USING
' FROM_PEG = 1
' TO_PEG   = 3
' USING    = 2
'
' hanoi(6, FROM_PEG, TO_PEG, USING, 0)
'
' sub hanoi(float n, from_peg, to_peg, using, level)
'     level = level + 1
'     if n > 0 then
'         hanoi(n - 1, from_peg, using, to_peg, level)
'         print "move ", from_peg, " --> ", to_peg, " at level ", level
'         hanoi(n - 1, using, to_peg, from_peg, level)
'     end if
'     level = level - 1
' end sub
'

#include "vbcompat.bi"

const DOUBLE_QUOTE as string = chr(34), SINGLE_QUOTE as string = chr(39)

' variables for the lexer

dim shared cur_line as string    ' text of current line
dim shared as string cur_ch      ' the current character
dim shared as long sym           ' the current symbol (keyword, operator, etc)
dim shared as string token       ' text version of current symbol

dim shared as long cur_col
dim shared as long cur_line_num
dim shared as long error_line
dim shared as long error_col
dim shared as boolean interactive
dim shared as boolean listing_wanted

type Key_words
    keyword as string
    sym as long
end type

const as long MAX_KEYWORDS = 26, MAX_GSYMTAB = 300, MAX_LSYMTAB = 100, MAX_CODE = 4000, _
    MAX_STACK = 28000, MAX_CNT_STR = 200, MAX_PENDING = 100

dim shared key_words_tab(MAX_KEYWORDS) as Key_words

' symbol table - all program identifiers stored here

type Symbol_table
    ident       as string
    data_offset as long
    id_type     as long
    value       as long     ' primary datatype
    nargs       as long
end type

' list of pending (undeclared) functions

type Pending_list
    name   as string
    offset as long
    nargs  as long  ' actual number of arguments in this call
end type

dim shared g_sym_tab(MAX_GSYMTAB) as Symbol_table      ' all global symbols
dim shared l_sym_tab(MAX_LSYMTAB) as Symbol_table      ' all local symbols
dim shared as long g_sym_tab_used, l_sym_tab_used      ' highest used index
dim shared as long g_data_offset, l_data_offset        ' highest used data offset - symbol table version

dim shared pending_list(MAX_PENDING) as Pending_list   ' sub fixups that need to be applied
dim shared as long pending_list_used                   ' highest used entry

' Virtual Machine

dim shared as long code_index ' highest used code position
dim shared as long last_label ' last label target
dim shared as long code_start ' first instruction index
dim shared as long g_data_size, l_data_size ' highest used data entry - VM version
dim shared as long code_arr(MAX_CODE) ' code store
dim shared as string string_pool(MAX_CNT_STR)
dim shared as long max_str_pool_used

' Parser

dim shared as long current_scope   ' either global or local
dim shared as long is_function     ' processing a function - as opposed to a sub
dim shared as long g_nargs         ' number of arguments for current sub/function
dim shared as long last_opcode

' Equates for the symbol type, e.g. what type of symbol have we just read
' the lexer sets variable sym to one of these

enum Symbol
    sym_unknown                ' 0
    sym_eoi                    ' 1
    sym_string_const           ' 2
    sym_lparen                 ' 3
    sym_rparen                 ' 4
    sym_comma                  ' 5
    sym_real_const             ' 6
    sym_integer_const          ' 7
    sym_ident                  ' 8
    sym_print                  ' 9
    sym_while                  '10
    sym_wend                   '11
    sym_end                    '12
    sym_halt                   '13
    sym_if                     '14
    sym_then                   '15
    sym_else                   '16
    sym_elseif                 '17
    sym_for                    '18
    sym_to                     '19
    sym_step                   '20
    sym_next                   '21
    sym_float_var              '22
    sym_whtspc                 '23
    sym_sub                    '24
    sym_call                   '25
    sym_function               '26
    sym_return                 '27
    sym_exponent 'new          '28
    sym_neg                    '29
    sym_multiply               '30
    sym_divide                 '31
    sym_int_div                '32
    sym_mod                    '33
    sym_plus                   '34
    sym_minus                  '35
    sym_equal                  '36
    sym_neq                    '37
    sym_lss                    '38
    sym_leq                    '39
    sym_gtr                    '40
    sym_geq                    '41
    sym_not                    '42
    sym_and                    '43
    sym_or                     '44
    sym_eqv      'new          '45
    sym_imp      'new          '46
    sym_xor      'new          '47
    sym_exit     'new          '48
end enum

const as long left_assoc = 1, right_assoc = 0

' Instructions for the virtual machine

enum Instruction
    op_push_int
    op_load_int_var
    op_stor
    op_add
    op_sub
    op_mul
    op_div
    op_mod
    op_lss
    op_leq
    op_gtr
    op_geq
    op_equal
    op_neq
    op_jz
    op_jmp
    op_neg
    op_not
    op_and
    op_or
    op_prt_int
    op_prt_nl
    op_prt_str
    op_halt
    op_int_div
    op_math_fun
    op_call
    op_ret
    op_enter
    op_retf
    op_pop
    op_pow      'new
    op_eqv      'new
    op_imp      'new
    op_xor      'new
    op_load_int_var_l
    op_load_int_var_g
    op_stor_l
    op_stor_g
end enum

enum math_functions
    fun_abs       '       0
    fun_acos      'new    1
    fun_asin      'new    2
    fun_atn       '       3
    fun_cos       '       4
    fun_exp       'new    5
    fun_fix       'new    6
    fun_frac      'new    7
    fun_int       '       8
    fun_log       '       9
    fun_rand      '      10
    fun_rnd       '      11
    fun_sgn       'new   12
    fun_sin       '      13
    fun_sqr       '      14
    fun_tan       '      15
    fun_atan2     'new   16
end enum

const as long type_var         = 1
const as long type_const_float = 2
const as long type_sub         = 3
const as long type_function    = 4

const as long global_scope     = 1
const as long local_scope      = 2

declare sub main()
declare function getcmd(st as string) as string
declare sub run_pgm(interactive as boolean, fn as string)
declare function handle_loaded(s as string, p as integer) as string
declare function get_parm(s as string, p as integer) as string
declare sub repl()
declare sub init_lex()
declare sub next_char() ' get the next char
declare sub skip_white_space()
declare sub skip_comment()
declare sub next_sym() ' determine the next symbol
declare function get_string() as long
declare sub get_digits()
declare sub get_ident()
declare function search_key_words() as long
declare sub init_pending_list()
declare function lookup_pending(s as string) as long
declare sub install_pending(s as string, offset as long, nargs as long)
declare sub init_g_sym_tab()
declare sub init_l_sym_tab()
declare function get_data_size() as long
declare function find_sym_tab(ident as string, byref address as long, byref extent as long, byref id_type as long) as long
declare sub insert_sym_tab(the_type as long)
declare sub insert_sym_tab_var(extent as long)
declare sub insert_sym_tab_double()
declare sub insert_sym_tab_sub()
declare sub insert_sym_tab_fun()
declare function is_in_sym_tab(byref address as long, byref extent as long, byref id_type as long) as long
declare sub init_code()
declare sub set_data_size(size_to_set as long)
declare function get_cur_loc() as long
declare sub emit_at(location as long, operand as long)
declare sub emitx(x as long)
declare function emit1(opcode as long) as long
declare function emit2(opcode as long, operand as long) as long
declare function emit3(opcode as long, operand1 as long, operand2 as long) as long
declare sub patch_jmp_to_current(fix_addr as long)
declare sub interpret()
declare sub emit_op(symbol as long)
declare function accept(symbol as long) as integer
declare sub expect(symbol as long)
declare function is_binary_operator(symbol as long) as long
declare function is_relational_operator(symbol as long) as long
declare function unary_prec(symbol as long) as long
declare function binary_prec(symbol as long) as long
declare function associativity(symbol as long) as long
declare sub paren_expr()
declare sub primary()
declare sub expr(p as long)
declare sub return_stmt()
declare sub call_stmt2(id as string, sym_pos as long, address as long, pop_ret as long)
declare sub call_stmt()
declare sub assign_stmt()
declare sub if_stmt()
declare sub halt_stmt()
declare sub while_stmt()
declare sub for_stmt()
declare function add_to_string_tab(s as string) as integer
declare sub print_stmt()
declare sub stmt_seq()
declare sub variable_decl()
declare function fixup_parms() as long
declare sub subs()
declare sub process_pending()
declare sub parse()
declare sub error_msg(msg as string)
declare sub press_a_key()
declare sub init_keywords()
declare function instr_st(x as long) as string
declare sub list_code()
declare function is_alpha(byval ch as string) as long
declare function is_print(byval ch as string) as long
declare function is_numeric(byval ch as string) as long
declare function is_alnum(byval ch as string) as long

main()

sub main()
    dim as string filename = command

    Randomize()
    init_keywords()

    if filename <> "" then
        interactive = false
        listing_wanted = true
        run_pgm(interactive, filename)
    else
        interactive = true
        listing_wanted = false
        repl()
    end if
end sub

sub repl()
    dim filename as string

    init_g_sym_tab()
    init_l_sym_tab()
    filename = ""
    do
        line input ">", cur_line

        dim p as integer
        dim cmd as string
        dim parm as string

        if cur_line = "" then continue do

        p = instr(cur_line, " ")
        if p = 0 then
            cmd = cur_line
        else
            cmd = mid(cur_line, 1, p - 1)
        end if

        select case cmd
            case "load"
                if p = 0 then
                    print "Load requires a filename"
                else
                    filename = handle_loaded(cur_line, p)
                end if
            case "run"
                if p > 0 then
                    filename = handle_loaded(cur_line, p)
                elseif p = 0 then
                    if filename = "" then
                        print "Nothing loaded to run"
                    end if
                end if
                if filename <> "" then
                    interactive = false
                    cur_line = ""
                    run_pgm(interactive, filename)
                    interactive = true
                end if
            case "quit", "bye", "exit": system
            case else
                ' run a single command
                run_pgm(interactive, filename)
        end select
    loop
end sub

function handle_loaded(s as string, p as integer) as string
    dim parm as string

    parm = get_parm(s, p)
    if FileExists(parm) then return parm

    dim e as integer
    e = Err()
    print "File not found:"; parm; " error:"; e
    return ""
end function

sub run_pgm(interactive as boolean, filename as string)
    if not interactive then
        open filename for input as #1
        init_g_sym_tab()
        init_l_sym_tab()
    end if
    init_pending_list()
    init_code()
    init_lex()
    next_char()
    parse()
    list_code()
    interpret()
    if not interactive then close #1
end sub

function get_parm(s as string, p as integer) as string
    s = mid(s, p)
    s = trim(s)
    if mid(s, 1, 1) = DOUBLE_QUOTE then
        s = mid(s, 2)
    end if
    if mid(s, len(s), 1) = DOUBLE_QUOTE then
        s = mid(s, 1, len(s) - 1)
    end if
    return (s)
end function

sub init_lex()
    cur_line_num = 0
    cur_col = 0

    cur_ch = ""
end sub

sub next_line()         ' read the next line of input from the source file
    cur_ch  = ""        ' empty cur_ch means end-of-file
    if interactive then exit sub
    if eof(1) then exit sub
    line input #1, cur_line
    cur_line = cur_line + chr(10)
    cur_line_num += 1
    cur_col = 1
end sub

sub next_char()         ' get the next char
    cur_col += 1
    if cur_col > len(cur_line) then next_line
    if cur_col <= len(cur_line) then cur_ch = mid(cur_line, cur_col, 1)
end sub

sub skip_white_space()
    do
        select case cur_ch
            case "":   return
            case " ", chr(9), chr(10), chr(13): next_char()
            case else: return
        end select
    loop
end sub

sub skip_comment()
    do
        next_char()
        if cur_ch = "" then return
        if asc(cur_ch) = 13 or asc(cur_ch) = 10 then return
    loop
end sub

'function removeeol(byval s as string) as string
'    if mid(s, len(s)) = chr(10) or mid(s, len(s)) = chr(10) then
'        s = mid(s, 1, len(s) - 1)
'    endif
'    if mid(s, len(s)) = chr(10) or mid(s, len(s)) = chr(10) then
'        s = mid(s, 1, len(s) - 1)
'    endif
'    return (s)
'end function

sub gobble(if_false as Symbol, second_ch as String, if_true as Symbol)
    sym = if_false
    next_char()
    if cur_ch = second_ch then
        sym = if_true
        next_sym()
    end if
end sub

sub next_sym() ' determine the next symbol
    token = ""
    skip_white_space()
    error_line = cur_line_num: error_col = cur_col
'    print cur_line_num; ": ";  mid(removeeol(cur_line), cur_col); "***cur_ch:["; cur_ch; "]";
    select case cur_ch
        case "":  sym = sym_eoi
        case "+": sym = sym_plus:     next_char()
        case "-": sym = sym_minus:    next_char()
        case "*": sym = sym_multiply: next_char()
        case "/": sym = sym_divide:   next_char()
        case "\": sym = sym_int_div:  next_char()
        case ",": sym = sym_comma:    next_char()
        case "(": sym = sym_lparen:   next_char()
        case ")": sym = sym_rparen:   next_char()
        case "=": sym = sym_equal:    next_char()
        case "^": sym = sym_exponent: next_char()
        case DOUBLE_QUOTE: sym = get_string()   ' a double quote
        case SINGLE_QUOTE: skip_comment(): next_sym() ' a single quote - comment until the end of line
        case "<"
            sym = sym_lss
            next_char()
            if cur_ch = ">" then
                sym = sym_neq
                next_char()
            elseif cur_ch = "=" then
                sym = sym_leq
                next_char()
            end if
        case ">": gobble(sym_gtr, "=", sym_geq)
        case else
            if is_numeric(cur_ch) or cur_ch = "." then
                get_digits()
            elseif is_alpha(cur_ch) then
                get_ident()
            else
                error_msg("unrecognized character: " & cur_ch & " asc = " & asc(cur_ch))
            end if
    end select
'    print " token:";token; " sym:"; sym
end sub

function get_string() as long
    dim as long start_line

    token = ""
    start_line = error_line
    next_char()
    while cur_ch <> DOUBLE_QUOTE
        if cur_ch = "" then
            error_msg("eof found in string")
        end if
        if error_line > start_line then
            error_msg("string must be on one line")
        end if
        token = token & cur_ch
        next_char()
    wend
    if cur_ch = DOUBLE_QUOTE then next_char()
    return sym_string_const
end function

sub get_digits()
    dim n_decimals as long

    token = ""
    n_decimals = 0
    while is_numeric(cur_ch) or cur_ch = "."
        if cur_ch = "." then n_decimals += 1
        token = token & cur_ch
        next_char()
    wend
    if n_decimals > 1 then error_msg("number contains more than 1 decimal point")
    sym = sym_real_const
    if n_decimals = 0 then
        sym = sym_integer_const
    end if
end sub

sub get_ident()
    token = ""
    while is_alnum(cur_ch) or cur_ch = "_"
        token = token & cur_ch
        next_char()
    wend
    sym = search_key_words()
end sub

' look for a key word - return either the matching sym or sym_ident
function search_key_words() as long
    dim as long i

    for i = 1 to MAX_KEYWORDS
        if token = key_words_tab(i).keyword then
            return key_words_tab(i).sym
        end if
    next

    return sym_ident
end function

'------ Pending list ---------------------------------------------------
sub init_pending_list()
    pending_list_used = 0
end sub

function lookup_pending(s as string) as long
    dim as long i
    for i = 1 to pending_list_used
        if s = pending_list(i).name then return i
    next

    return 0
end function

sub install_pending(s as string, offset as long, nargs as long)
    if pending_list_used >= MAX_PENDING then
        error_msg("Pending list exhausted")
    end if

    pending_list_used += 1
    pending_list(pending_list_used).name   = s
    pending_list(pending_list_used).offset = offset
    pending_list(pending_list_used).nargs  = nargs
end sub

'------ Symbol table ---------------------------------------------------

sub init_g_sym_tab()
    g_data_offset = 1
    g_sym_tab_used = 0
end sub

sub init_l_sym_tab()
    l_data_offset = 1
    l_sym_tab_used = 0
end sub

function get_data_size() as long
    return g_data_offset
end function

function find_sym_tab(ident as string, byref address as long, byref extent as long, byref id_type as long) as long
    dim as long i
    for i = 1 to l_sym_tab_used
        if ident = l_sym_tab(i).ident then
            address = l_sym_tab(i).data_offset
            extent  = local_scope
            id_type = l_sym_tab(i).id_type
            return i
        end if
    next

    for i = 1 to g_sym_tab_used
        if ident = g_sym_tab(i).ident then
            address = g_sym_tab(i).data_offset
            extent  = global_scope
            id_type = g_sym_tab(i).id_type
            return i
        end if
    next

    return 0
end function

sub insert_sym_tab(the_type as long)
    if g_sym_tab_used >= MAX_GSYMTAB then
        error_msg("Symbol table exhausted")
    end if

    if find_sym_tab(token, 0, 0, 0) > 0 then
        error_msg(token & " has already been defined")
    end if
    g_sym_tab_used += 1
    g_sym_tab(g_sym_tab_used).ident   = token
    g_sym_tab(g_sym_tab_used).id_type = the_type
end sub

sub insert_sym_tab_var(extent as long)
    if extent = global_scope then
        insert_sym_tab(type_var)
        g_sym_tab(g_sym_tab_used).data_offset = g_data_offset
        g_data_offset += 1
    else
        l_sym_tab_used += 1
        l_sym_tab(l_sym_tab_used).ident   = token
        l_sym_tab(l_sym_tab_used).id_type = type_var
        l_sym_tab(l_sym_tab_used).data_offset = l_data_offset
        l_data_offset += 1
    end if
end sub

sub insert_sym_tab_double()
    insert_sym_tab(type_const_float)

    g_sym_tab(g_sym_tab_used).data_offset = g_data_offset
    g_data_offset += 1
    g_sym_tab(g_sym_tab_used).value      = val(token)
end sub

sub insert_sym_tab_sub()
    insert_sym_tab(type_sub)

    g_sym_tab(g_sym_tab_used).data_offset = get_cur_loc()
    g_sym_tab(g_sym_tab_used).nargs       = 0
end sub

sub insert_sym_tab_fun()
    insert_sym_tab(type_function)

    g_sym_tab(g_sym_tab_used).data_offset = get_cur_loc()
    g_sym_tab(g_sym_tab_used).nargs       = 0
end sub

' see if an ident exists in the symbol table - long return
function is_in_sym_tab(byref address as long, byref extent as long, byref id_type as long) as long
    dim as long i

    i = find_sym_tab(token, address, extent, id_type)
    if i = 0 then return false
    return true
end function

'------ virtual machine -------------------------------------------------------------

' code generator

sub init_code()
    code_index = 1
    last_label = 0
    code_start = 0
    g_data_size = 0
    l_data_size = 0
    max_str_pool_used = 0
end sub

sub set_data_size(size_to_set as long)
    g_data_size = size_to_set
end sub

function get_cur_loc() as long
    return code_index
end function

sub emit_at(location as long, operand as long)
    code_arr(location) = operand
end sub

sub emitx(x as long)
    if code_index >= MAX_CODE then
        error_msg("code array exhausted: " & str(code_index))
    end if
    code_arr(code_index) = x
    code_index += 1
end sub

function emit1(opcode as long) as long
    dim as long location = code_index
    last_opcode = opcode
    emitx(opcode)
    return location
end function

function emit2(opcode as long, operand as long) as long
    dim as long location = code_index
    emit1(opcode)
    emitx(operand)
    return location
end function

function emit3(opcode as long, operand1 as long, operand2 as long) as long
    dim as long location = code_index

    if opcode = op_load_int_var then
        if operand1 = global_scope then
            emit2(op_load_int_var_g, operand2)
        elseif operand1 = local_scope then
            emit2(op_load_int_var_l, operand2)
        else
            error_msg("Error 1 in code generator")
        end if

    elseif opcode = op_stor then
        if operand1 = global_scope then
            emit2(op_stor_g, operand2)
        elseif operand1 = local_scope then
            emit2(op_stor_l, operand2)
        else
            error_msg("Error 2 in code generator")
        end if
    else
        error_msg("Error 3 in code generator")
        emit1(opcode)
        emitx(operand1)
        emitx(operand2)
    end if
    return location
end function

sub patch_jmp_to_current(fix_addr as long)
    ' skip over opcode
    last_label = code_index
    emit_at(fix_addr + 1, code_index)
end sub

Function rnd_range(first As Double, last As Double) As Double
    return Rnd * (last - first) + first
End Function

' virtual machine interpreter

sub interpret()
    dim as long pc, sp, bp, i
    dim as long opcode, operand, retval
    dim as long stack(MAX_STACK), argf, argf2     'primary datatype

    ' load constant numbers to stack
    for i = 1 to g_sym_tab_used
        if g_sym_tab(i).id_type = type_const_float then
            stack(g_sym_tab(i).data_offset) = g_sym_tab(i).value
        end if
    next

    if listing_wanted then
        print "code_index|g_data_size: "; code_index; " "; g_data_size; " data: "
        for i = 1 to g_data_size
            print stack(i)
        next
        print "Running..."
    end if
    pc = code_start
    sp = g_data_size
    bp = &Hffff
    do
        opcode = code_arr(pc)
        pc += 1
        operand = code_arr(pc)

        select case opcode
            case op_push_int
                sp += 1
                stack(sp) = operand
                pc += 1

            case op_load_int_var_l
                sp += 1
                stack(sp) = stack(operand + bp)
                pc += 1

            case op_load_int_var_g
                sp += 1
                stack(sp) = stack(operand)
                pc += 1

            case op_stor_l
                stack(operand + bp) = stack(sp)
                sp -= 1
                pc += 1

            case op_stor_g
                stack(operand) = stack(sp)
                sp -= 1
                pc += 1

            case op_pop:     sp -= 1
            case op_call
                sp += 1: stack(sp) = pc + 1
                sp += 1: stack(sp) = bp
                bp = sp
                pc = operand
            case op_enter
                sp += operand: pc += 1
                if sp + 32 >= MAX_STACK then error_msg("out of stack space")
            case op_retf
                retval = stack(sp)
                sp = bp
                bp = stack(sp): sp -= 1
                pc = stack(sp): sp -= 1
                sp -= operand
                sp += 1
                stack(sp) = retval
            case op_ret
                sp = bp
                bp = stack(sp): sp -= 1
                pc = stack(sp): sp -= 1
                sp -= operand
            case op_jz
                if stack(sp) = 0 then
                    pc = operand
                else
                    pc += 1
                end if
                sp -= 1
            case op_jmp:    pc = operand
            case op_add:     sp -= 1: stack(sp) += stack(sp + 1)
            case op_sub:     sp -= 1: stack(sp) -= stack(sp + 1)
            case op_mul:     sp -= 1: stack(sp) *= stack(sp + 1)
            case op_div
                sp -= 1
                if stack(sp + 1) = 0 then error_msg("divide by zero")
                stack(sp) /= stack(sp + 1)
            case op_int_div
                sp -= 1
                if stack(sp + 1) = 0 then error_msg("divide by zero")
                stack(sp) \= stack(sp + 1)
            case op_mod
                sp -= 1
                if stack(sp + 1) = 0 then error_msg("divide by zero")
                stack(sp) = stack(sp) mod stack(sp + 1)
            case op_pow:     sp -= 1: stack(sp) = stack(sp) ^ stack(sp + 1)
            case op_eqv:     sp -= 1: stack(sp) = stack(sp) eqv stack(sp + 1)
            case op_imp:     sp -= 1: stack(sp) = stack(sp) imp stack(sp + 1)
            case op_xor:     sp -= 1: stack(sp) = stack(sp) xor stack(sp + 1)
            case op_or:      sp -= 1: stack(sp) = ((stack(sp) <> 0) or (stack(sp + 1) <> 0))
            case op_and:     sp -= 1: stack(sp) = ((stack(sp) <> 0) and (stack(sp + 1) <> 0))
            case op_neq:     sp -= 1: stack(sp) = (stack(sp) <> stack(sp + 1))
            case op_equal:   sp -= 1: stack(sp) = (stack(sp) = stack(sp + 1))
            case op_lss:     sp -= 1: stack(sp) = (stack(sp) < stack(sp + 1))
            case op_leq:     sp -= 1: stack(sp) = (stack(sp) <= stack(sp + 1))
            case op_gtr:     sp -= 1: stack(sp) = (stack(sp) > stack(sp + 1))
            case op_geq:     sp -= 1: stack(sp) = (stack(sp) >= stack(sp + 1))
            case op_neg:     stack(sp) = -stack(sp)
            case op_not:     stack(sp) = not stack(sp)

            case op_prt_str: print string_pool(operand); : pc += 1
            case op_prt_int: print stack(sp); : sp -= 1
            case op_prt_nl:  print

            case op_math_fun
                argf = stack(sp)
                pc += 1
                select case operand
                    case fun_abs:  stack(sp) = abs(argf)
                    case fun_acos: stack(sp) = acos(argf)
                    case fun_asin: stack(sp) = asin(argf)
                    case fun_atn:  stack(sp) = atn(argf)
                    case fun_cos:  stack(sp) = cos(argf)
                    case fun_exp:  stack(sp) = exp(argf)
                    case fun_fix:  stack(sp) = fix(argf)
                    case fun_frac: stack(sp) = frac(argf)
                    case fun_int:  stack(sp) = int(argf)
                    case fun_log:  stack(sp) = log(argf)
                    case fun_rand: stack(sp) = rnd_range(0, argf)
                    case fun_rnd:  stack(sp) = rnd(argf)
                    case fun_sgn:  stack(sp) = sgn(argf)
                    case fun_sin:  stack(sp) = sin(argf)
                    case fun_sqr:  stack(sp) = sqr(argf)
                    case fun_tan:  stack(sp) = tan(argf)
                    case fun_atan2:
                        argf2 = stack(sp - 1)
                        sp -= 1
                        stack(sp) = atan2(argf2, argf)
                end select
            case op_halt:    exit do
            case else:       error_msg("Unknown opcode " & str(opcode))
        end select
    loop
    if listing_wanted then
        print "Finished..."
    end if
end sub

' parser ----------------------------------------------------------------------

sub emit_op(symbol as long)
    select case symbol
        case sym_or:       emit1(op_or)
        case sym_and:      emit1(op_and)
        case sym_equal:    emit1(op_equal)
        case sym_neq:      emit1(op_neq)
        case sym_lss:      emit1(op_lss)
        case sym_leq:      emit1(op_leq)
        case sym_gtr:      emit1(op_gtr)
        case sym_geq:      emit1(op_geq)
        case sym_plus:     emit1(op_add)
        case sym_minus:    emit1(op_sub)
        case sym_multiply: emit1(op_mul)
        case sym_divide:   emit1(op_div)
        case sym_mod:      emit1(op_mod)
        case sym_neg:      emit1(op_neg)
        case sym_not:      emit1(op_not)
        case sym_int_div:  emit1(op_int_div)
        case sym_exponent: emit1(op_pow)
        case sym_eqv:      emit1(op_eqv)
        case sym_imp:      emit1(op_imp)
        case sym_xor:      emit1(op_xor)
    end select
end sub

function accept(symbol as long) as integer
    if symbol <> sym then return false
    next_sym()
    return true
end function

sub expect(symbol as long)
    if not accept(symbol) then
        error_msg("unexpected token - expected: " & str(symbol) & " got: " & str(sym))
    end if
end sub

function is_binary_operator(symbol as long) as long
    select case symbol
        case sym_or, sym_and
        case sym_equal, sym_neq
        case sym_lss, sym_leq, sym_gtr, sym_geq
        case sym_plus, sym_minus
        case sym_multiply, sym_divide, sym_mod, sym_int_div
        case sym_exponent, sym_eqv, sym_imp, sym_xor

        case else: return false
    end select
    return true
end function

function is_relational_operator(symbol as long) as long
    select case symbol
        case sym_equal, sym_neq
        case sym_lss, sym_leq, sym_gtr, sym_geq

        case else
            return false
    end select
    return true
end function

function unary_prec(symbol as long) as long
    select case symbol
        case sym_neg: return 12
        case sym_not: return  6
        case else:    return  0
    end select
end function

function binary_prec(symbol as long) as long
    select case symbol
        case sym_exponent:                                           return 13
        case sym_multiply, sym_divide:                               return 11
        case sym_int_div:                                            return 10
        case sym_mod:                                                return  9
        case sym_plus, sym_minus:                                    return  8
        case sym_lss, sym_leq, sym_gtr, sym_geq, sym_equal, sym_neq: return  7
        case sym_and:                                                return  5
        case sym_or:                                                 return  4
        case sym_xor:                                                return  3
        case sym_eqv:                                                return  2
        case sym_imp:                                                return  1
        case else:                                                   return  0
    end select
end function

function associativity(symbol as long) as long
    return left_assoc
end function

sub paren_expr()
    next_sym()
    expr(0)
    if not accept(sym_rparen) then error_msg("expecting ')'")
end sub

sub primary()
    dim as long address, extent, id_type, sym_pos
    dim as long p, fun
    dim as string id

    select case sym
        case sym_integer_const
            emit2(op_push_int, val(token))
            next_sym()
        case sym_real_const
            if not is_in_sym_tab(address, 0, 0) then
                insert_sym_tab_double()
                address = g_sym_tab(g_sym_tab_used).data_offset
            end if
            emit3(op_load_int_var, global_scope, address)
            next_sym()
        case sym_ident
            id = token
            sym_pos = find_sym_tab(id, address, extent, id_type)
            if sym_pos <> 0 then
                if id_type = type_var then
                    emit3(op_load_int_var, extent, address)
                    next_sym()
                elseif extent = global_scope then
                    if id_type = type_sub then error_msg("primary: '" & token & "': sub not allowed in expression")
                    if id_type = type_function then
                        next_sym()
                        call_stmt2(id, sym_pos, address, false)
                    end if
                else
                    error_msg("primary: '" & token & "' must be a variable or a function")
                end if
            else
                p = instr(" abs    acos   asin   atn    cos    exp    fix    frac   int    log    rand   rnd    sgn    sin    sqr    tan    atan2 ", " " & token & " ")
                          '12345671234567123456712345671234567123456712345671234567123456712345671234567123456712345671234567123456712345671234567
                          '1      2      3      4      5      6      7      8      9      10     11     12     13     14     15     16     17
                if p > 0 then
                    fun = ((p - 1) / 7)     ' 7 = width of each function above - we count from 0, so don't add 1
                    if fun <= 15 then       ' 1st 16 take a single parameter (0..15 = 16)
                        next_sym()          'skip function name
                        paren_expr()
                    elseif fun = 16 then
                        next_sym()
                        expect(sym_lparen)
                        expr(0)                 ' arg1
                        expect(sym_comma)
                        expr(0)                 ' arg2
                        expect(sym_rparen)
                    end if
                    emit1(op_math_fun)
                    emitx(fun)
                else
                    next_sym()
                    if sym = sym_lparen then
                        call_stmt2(id, 0, 0, false)
                    else
                        error_msg("primary: '" & token & "' has not been defined")
                    end if
                end if
            end if
        case sym_lparen
            paren_expr()
        case sym_minus
            next_sym()
            if sym = sym_real_const then
                token = "-" & token
                if not is_in_sym_tab(address, 0, 0) then
                    insert_sym_tab_double()
                    address = g_sym_tab(g_sym_tab_used).data_offset
                end if
                next_sym()
                emit3(op_load_int_var, global_scope, address)
            elseif sym = sym_integer_const then
                emit2(op_push_int, -val(token))
                next_sym()
            else
                expr(unary_prec(sym_neg))
                emit_op(sym_neg)
            end if
        case sym_not
            next_sym()
            expr(unary_prec(sym_not))
            emit_op(sym_not)
        case else
            error_msg("expecting a primary")
    end select
end sub

sub expr(p as long)
    primary()
    while is_binary_operator(sym) and binary_prec(sym) >= p
        dim as long op = sym
        dim as long tmp = 0

        next_sym()
        if associativity(op) = left_assoc then tmp = 1
        expr(binary_prec(op) + tmp)
        emit_op(op)
        if is_relational_operator(op) and is_relational_operator(sym) then
            error_msg("consecutive relational operators not allowed")
        end if
    wend
end sub

sub return_stmt()
    expect(sym_return)
    if current_scope = global_scope then
        emit1(op_halt)
    elseif is_function then
        expr(0)
        emit2(op_retf, g_nargs)
    else
        emit2(op_ret, g_nargs)
    end if
end sub

sub call_stmt2(id as string, sym_pos as long, address as long, pop_ret as long)
    dim as long nargs

    nargs = 0
    if accept(sym_lparen) then
        if sym <> sym_rparen then
            do
                expr(0)
                nargs += 1
                if not accept(sym_comma) then exit do
            loop
        end if
        expect(sym_rparen)
    end if
    if sym_pos <> 0 then
        emit2(op_call, address)
        if nargs <> g_sym_tab(sym_pos).nargs then
            error_msg(id & " expects " & g_sym_tab(sym_pos).nargs & " args, but " & nargs & " were found.")
        end if
    else
        dim as long location = get_cur_loc()
        emit2(op_call, 0)
        ' add id, location to pending list
        install_pending(id, location, nargs)
    end if
    if pop_ret and g_sym_tab(sym_pos).id_type = type_function then
        emit1(op_pop)
    end if
end sub

sub call_stmt()
    dim id as string
    dim as long address, sym_pos

    expect(sym_call)
    id = token
    if not accept(sym_ident) then error_msg("sub name expected")
    sym_pos = find_sym_tab(id, address, 0, 0)
    call_stmt2(id, sym_pos, address, true)
end sub

' ident = expr | ident() | ident
' ???what about when called from "for i=expr" - need to check for that, and require assignment
sub assign_stmt()
    dim as long address, extent

    dim as string id  = token
    dim as long sym_pos   = find_sym_tab(id, address, extent, 0)

    expect(sym_ident)
    if not accept(sym_equal) then
        call_stmt2(id, sym_pos, address, true)
    else
        if sym_pos = 0 then error_msg("assign: '" & id & "' has not been defined")
        expr(0)
        emit3(op_stor, extent, address)
    end if
end sub

' if expr then stmt_seq (else stmt_seq) end if
sub if_stmt()
    dim as long fix1, fix2

    fix2 = -1
    next_sym()
    expr(0)
    expect(sym_then)
    fix1 = emit2(op_jz, 0)                       ' jump to next elseif/else
    stmt_seq()
    if sym = sym_elseif or sym = sym_else then
        fix2 = emit2(op_jmp, 0)                  ' jump to endif
    end if
    patch_jmp_to_current(fix1)                   ' resolve conditional jmp
    if sym = sym_elseif then
        if_stmt()
    else
        if accept(sym_else) then
            stmt_seq()
        end if
        expect(sym_end)
        expect(sym_if)
    end if
    if fix2 <> -1 then
        patch_jmp_to_current(fix2)
    end if
end sub

sub halt_stmt()
    emit1(op_halt)
    next_sym()
end sub

' while expr stmts wend
sub while_stmt()
    dim as long top
    dim as long fix1

    expect(sym_while)
    top = get_cur_loc()
    expr(0)
    fix1 = emit2(op_jz, 0)
    stmt_seq()
    emit2(op_jmp, top)
    patch_jmp_to_current(fix1)
    expect(sym_wend)
end sub

' for var = expr1 to expr2 [step expr3]
'    stmt_seq
' next
sub for_stmt()
    dim as long address, extent, loop_start, jmp_out_of_loop, skip_for_incr, incr_for, cmp_addr

    incr_for = 0
    expect(sym_for)
    ' var = expr1
    if sym <> sym_ident then error_msg("for: " & "expecting variable, found: " & token)
    is_in_sym_tab(address, extent, 0)
    assign_stmt()

    ' to expr2
    expect(sym_to)
    loop_start = get_cur_loc()
    emit3(op_load_int_var, extent, address)
    expr(0)
    cmp_addr = get_cur_loc()
    emit1(op_leq)
    jmp_out_of_loop = emit2(op_jz, 0)

    ' [step expr3]
    if accept(sym_step) then
        skip_for_incr = emit2(op_jmp, 0)
        incr_for = get_cur_loc()

        ' hack alert - using code array directly
        if sym = sym_minus then
            code_arr(cmp_addr) = op_geq
        end if
        expr(0)
        emit3(op_load_int_var, extent, address)
        emit1(op_add)
        emit3(op_stor, extent, address)
        emit2(op_jmp, loop_start)
        patch_jmp_to_current(skip_for_incr)
    end if

    ' stmt_seq
    stmt_seq()
    expect(sym_next)
    if incr_for <> 0 then
        emit2(op_jmp, incr_for)
    else
        emit3(op_load_int_var, extent, address)
        emit2(op_push_int, 1)
        emit1(op_add)
        emit3(op_stor, extent, address)

        emit2(op_jmp, loop_start)
    end if
    patch_jmp_to_current(jmp_out_of_loop)
end sub

function add_to_string_tab(s as string) as integer
    dim i as integer

    for i = 1 to max_str_pool_used
        if string_pool(i) = s then return i
    next
    if max_str_pool_used >= MAX_CNT_STR then error_msg("out of string pool space")
    max_str_pool_used += 1
    string_pool(max_str_pool_used) = s
    return max_str_pool_used
end function

' print expr|string {, expr|string}
sub print_stmt()
    do
        next_sym()
        if sym = sym_string_const then
            emit2(op_prt_str, add_to_string_tab(token))
            next_sym()
        else
            expr(0)
            emit1(op_prt_int)
        end if
        if sym <> sym_comma then exit do
    loop
    emit1(op_prt_nl)
end sub

' {print | halt | while | assign | if}
sub stmt_seq()
    do
        select case sym
            case sym_print:     print_stmt()
            case sym_halt:      halt_stmt()
            case sym_if:        if_stmt()
            case sym_while:     while_stmt()
            case sym_for:       for_stmt()
            case sym_ident:     assign_stmt()
            case sym_call:      call_stmt()
            case sym_return:    return_stmt()

            case sym_float_var: variable_decl()
            case sym_eoi, sym_end, sym_else, sym_elseif, sym_wend, sym_next, sym_sub, sym_function
                exit do
            case else:          error_msg("unrecognized statement: (" & str(sym) & ") " & token)
        end select
    loop
end sub

' {integer ident {, ident}}
sub variable_decl()
    dim as long address, extent

    while sym = sym_float_var
        expect(sym_float_var)
        do
            if is_in_sym_tab(address, extent, 0) and extent = current_scope then
                error_msg("'" & token & "' has already been defined")
            end if
            insert_sym_tab_var(current_scope)
            expect(sym_ident)
            if not accept(sym_comma) then exit do
        loop
    wend
end sub

' Currently, subroutine frame is 2
' This makes the formula: fixup = (nargs + 2)
' Adjust each parm by subtracting fixup.
function fixup_parms() as long
    dim as long fixup, i

    if l_sym_tab_used = 0 then return 0
    fixup = l_data_offset + 1
    for i = 1 to l_sym_tab_used
        l_sym_tab(i).data_offset = l_sym_tab(i).data_offset - fixup
    next
    l_data_offset = 1
    return l_sym_tab_used
end function

' sub ident() stmt_seq end sub
sub subs()
    dim as long address, save_scope, nargs, ndx, fixup

    save_scope    = current_scope
    current_scope = local_scope
    g_nargs       = 0

    while sym = sym_sub or sym = sym_function
        is_function = false
        if sym = sym_function then is_function = true
        init_l_sym_tab()
        next_sym()
        if sym <> sym_ident then
            error_msg("sub/fun name expected")
        else
            if is_in_sym_tab(address, 0, 0) then
                error_msg("'" & token & "' has already been defined")
            end if
            if is_function then
                insert_sym_tab_fun()
            else
                insert_sym_tab_sub()
            end if
            ndx = g_sym_tab_used    ' hack - need a better interface
            expect(sym_ident)
            nargs = 0
            if accept(sym_lparen) then
                variable_decl()
                nargs = fixup_parms()
                g_nargs = nargs
                g_sym_tab(ndx).nargs = nargs
                expect(sym_rparen)
            end if
            fixup = emit2(op_enter, 0)
            stmt_seq()
            expect(sym_end)
            if is_function then
                expect(sym_function)
                if last_opcode <> op_retf orelse get_cur_loc() <= last_label then
                    emit2(op_push_int, 0)
                    emit2(op_retf, nargs)
                end if
            else
                expect(sym_sub)
                print "code index: "; get_cur_loc(); " last_label: "; last_label
                if last_opcode <> op_ret orelse get_cur_loc() <= last_label then
                    emit2(op_ret, nargs)
                end if
            end if
            emit_at(fixup + 1, l_data_offset)
        end if
    wend
    current_scope = save_scope
    is_function   = false
    g_nargs       = 0
end sub

sub process_pending()
    dim as long i, sym_pos, address, extent, id_type

    for i = 1 to pending_list_used
        sym_pos = find_sym_tab(pending_list(i).name, address, extent, id_type)
        if sym_pos = 0 then error_msg(pending_list(i).name & " not found")
        if extent <> global_scope or (id_type <> type_sub and id_type <> type_function) then
            error_msg(pending_list(i).name & " defined, but not as sub/fun")
        end if
        if g_sym_tab(sym_pos).nargs <> pending_list(i).nargs then
            error_msg(pending_list(i).name & " expects " & g_sym_tab(sym_pos).nargs & " args, but " & pending_list(i).nargs & " were found.")
        end if
        emit_at(pending_list(i).offset + 1, address)
    next
end sub

'
sub parse()
    next_sym()

    current_scope = global_scope
    subs()
    code_start = get_cur_loc()
    stmt_seq()
    if last_opcode <> op_halt orelse get_cur_loc() <= last_label then
        emit1(op_halt)
    end if
    subs()
    if sym <> sym_eoi then
        error_msg("a statement was expected")
    end if
    set_data_size(get_data_size())
    process_pending()
end sub

'------ other stuff -----------------------------------------

sub error_msg(msg as string)
    print "line="; error_line; ", col="; error_col; ": "; msg
    if not interactive then system
end sub

'-------------- utilities -----------------------------------

sub press_a_key
    dim junk as long
    input "Press enter", junk
end sub

function is_alpha(byval ch as string) as long
    is_alpha = (ch <> "") and Asc(UCase(ch)) >= Asc("A") and Asc(UCase(ch)) <= Asc("Z")
end function

function is_print(byval ch as string) as long
    is_print = (ch <> "") and Asc(ch) >= Asc(" ") and Asc(ch) <= Asc("~")
end function

function is_numeric(byval ch as string) as long
    is_numeric = (ch <> "") and Asc(ch) >= Asc("0") and Asc(ch) <= Asc("9")
end function

function is_alnum(byval ch as string) as long
    return is_alpha(ch) or is_numeric(ch)
end function

' ----------------------------------------------------------------------

sub init_keywords()
    key_words_tab( 1).sym = sym_and       : key_words_tab( 1).keyword = "and"
    key_words_tab( 2).sym = sym_call      : key_words_tab( 2).keyword = "call"
    key_words_tab( 3).sym = sym_else      : key_words_tab( 3).keyword = "else"
    key_words_tab( 4).sym = sym_elseif    : key_words_tab( 4).keyword = "elseif"
    key_words_tab( 5).sym = sym_end       : key_words_tab( 5).keyword = "end"
    key_words_tab( 6).sym = sym_eqv       : key_words_tab( 6).keyword = "eqv"
    key_words_tab( 7).sym = sym_exit      : key_words_tab( 7).keyword = "exit"
    key_words_tab( 8).sym = sym_imp       : key_words_tab( 8).keyword = "imp"
    key_words_tab( 9).sym = sym_float_var : key_words_tab( 9).keyword = "float"   ' really:"float"
    key_words_tab(10).sym = sym_for       : key_words_tab(10).keyword = "for"
    key_words_tab(11).sym = sym_function  : key_words_tab(11).keyword = "function"
    key_words_tab(12).sym = sym_halt      : key_words_tab(12).keyword = "halt"
    key_words_tab(13).sym = sym_if        : key_words_tab(13).keyword = "if"
    key_words_tab(14).sym = sym_mod       : key_words_tab(14).keyword = "mod"
    key_words_tab(15).sym = sym_next      : key_words_tab(15).keyword = "next"
    key_words_tab(16).sym = sym_not       : key_words_tab(16).keyword = "not"
    key_words_tab(17).sym = sym_or        : key_words_tab(17).keyword = "or"
    key_words_tab(18).sym = sym_print     : key_words_tab(18).keyword = "print"
    key_words_tab(19).sym = sym_return    : key_words_tab(19).keyword = "return"
    key_words_tab(20).sym = sym_step      : key_words_tab(20).keyword = "step"
    key_words_tab(21).sym = sym_sub       : key_words_tab(21).keyword = "sub"
    key_words_tab(22).sym = sym_then      : key_words_tab(22).keyword = "then"
    key_words_tab(23).sym = sym_to        : key_words_tab(23).keyword = "to"
    key_words_tab(24).sym = sym_wend      : key_words_tab(24).keyword = "wend"
    key_words_tab(25).sym = sym_while     : key_words_tab(25).keyword = "while"
    key_words_tab(26).sym = sym_xor       : key_words_tab(26).keyword = "xor"
end sub

function instr_st(x as long) as string
    select case x
      case op_push_int     : return "pushi"
      case op_load_int_var : return "loadi"
      case op_stor         : return "stor"
      case op_add          : return "add"
      case op_sub          : return "sub"
      case op_mul          : return "mul"
      case op_div          : return "div"
      case op_mod          : return "mod"
      case op_lss          : return "lss"
      case op_leq          : return "leq"
      case op_gtr          : return "gtr"
      case op_geq          : return "geq"
      case op_equal        : return "equal"
      case op_neq          : return "neq"
      case op_jz           : return "jz"
      case op_jmp          : return "jmp"
      case op_neg          : return "neg"
      case op_not          : return "not"
      case op_and          : return "and"
      case op_or           : return "or"
      case op_prt_int      : return "prti"
      case op_prt_nl       : return "prtnl"
      case op_prt_str      : return "prts"
      case op_halt         : return "halt"
      case op_int_div      : return "idiv"
      case op_math_fun     : return "math_fun"
      case op_call         : return "call"
      case op_ret          : return "ret"
      case op_enter        : return "enter"
      case op_retf         : return "retf"
      case op_pop          : return "pop"
      case op_pow          : return "power"
      case op_eqv          : return "eqv"
      case op_imp          : return "imp"
      case op_xor          : return "xor"
      case else
        error_msg("unexpected opcode " & str(x))
    end select
end function

' code lister
sub list_code()
    dim as long i
    dim as long last_code

    dim as string st, pst, listing
    dim as long tmp

    if not listing_wanted then exit sub
    print
    print "Code listing..."
    last_code = code_index
    i = 1
    st = ""
    listing = ""
    do
        st = i & " "
        select case code_arr(i)
            case op_load_int_var
                i += 1
                if code_arr(i) = global_scope then
                    st = st & " loadi " & code_arr(i + 1)
                else
                    st = st & " loadi [bp] " & code_arr(i + 1)
                end if
                i += 1

            case op_stor
                i += 1
                if code_arr(i) = global_scope then
                    st = st & " storei " & code_arr(i + 1)
                else
                    st = st & " storei [bp] " & code_arr(i + 1)
                end if
                i += 1

            case op_load_int_var_l
                i += 1
                st = st & " loadi [bp] " & code_arr(i + 1)
            case op_load_int_var_g
                i += 1
                st = st & " loadi " & code_arr(i + 1)
            case op_stor_l
                i += 1
                st = st & " storei [bp] " & code_arr(i + 1)
            case op_stor_g
                i += 1
                st = st & " storei " & code_arr(i + 1)

            case op_push_int:     i += 1: st = st & " pushi " & code_arr(i)
            case op_jmp:          i += 1: st = st & " jmp   " & code_arr(i)
            case op_jz:           i += 1: st = st & " jz    " & code_arr(i)
            case op_prt_str:      i += 1: st = st & " prts  " & string_pool(code_arr(i))
            case op_call:         i += 1: st = st & " call  " & code_arr(i)
            case op_enter:        i += 1: st = st & " enter " & code_arr(i)
            case op_ret:          i += 1: st = st & " ret   " & code_arr(i)
            case op_retf:         i += 1: st = st & " retf  " & code_arr(i)

            case op_math_fun
                i += 1

                select case code_arr(i)
                    case fun_abs:  st = st & " abs"
                    case fun_acos: st = st & " acod"
                    case fun_asin: st = st & " asin"
                    case fun_atn:  st = st & " atn"
                    case fun_atan2:st = st & " atan2"
                    case fun_cos:  st = st & " cos"
                    case fun_exp:  st = st & " exp"
                    case fun_fix:  st = st & " fix"
                    case fun_frac: st = st & " frac"
                    case fun_int:  st = st & " int"
                    case fun_log:  st = st & " log"
                    case fun_rand: st = st & " rand"
                    case fun_rnd:  st = st & " rnd"
                    case fun_sgn:  st = st & " sgn"
                    case fun_sin:  st = st & " sin"
                    case fun_sqr:  st = st & " sqr"
                    case fun_tan:  st = st & " tan"
                    case else
                        error_msg("unexpected math fun " & str(code_arr(i)))
                end select

            case else
                st = st & " " & instr_st(code_arr(i))
        end select
        i += 1
        print st
        listing = listing & st & chr(10)
        st = ""
        if i >= last_code then exit do
    loop
    print st
'    putfile "code.listing", listing
end sub
