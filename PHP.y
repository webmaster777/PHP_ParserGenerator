%name PHP

%include {
    static public $transTable = array();

    function __construct()
    {
        if (!count(self::$transTable)) {
            $start = 240; // start nice and low to be sure
            while (token_name($start) == 'UNKNOWN') {
                $start++;
            }
            $hash = array_flip(self::$yyTokenName);
            $map =
                array(
                    ord(',') => self::T_COMMA,
                    ord('=') => self::T_EQUAL,
                    ord('?') => self::T_QUESTION,
                    ord(':') => self::T_COLON,
                    ord('|') => self::T_PIPE,
                    ord('^') => self::T_CIRCUMFLEX,
                    ord('&') => self::T_AMPERSAND,
                    ord('<') => self::T_SMALLER,
                    ord('>') => self::T_GREATER,
                    ord('+') => self::T_PLUS,
                    ord('-') => self::T_MINUS,
                    ord('.') => self::T_DOT,
                    ord('*') => self::T_STAR,
                    ord('/') => self::T_SLASH,
                    ord('%') => self::T_PERCENT,
                    ord('!') => self::T_EXCLAMATION,
                    ord('~') => self::T_TILDE,
                    ord('@') => self::T_AT,
                    ord('[') => self::T_SQUARE_OPEN,
                    ord('(') => self::T_PAREN_OPEN,
                    ord(')') => self::T_PAREN_CLOSE,
                    ord(';') => self::T_SEMI_COLON,
                    ord('{') => self::T_CURLY_OPEN,
                    ord('}') => self::T_CURLY_CLOSE,
                    ord('`') => self::T_BACKQUOTE,
                    ord('$') => self::T_DOLLAR,
                    ord(']') => self::T_SQUARE_CLOSE,
                    ord('"') => self::T_DQUOTE,
                    ord("'") => self::T_SQUOTE,
                );
            for ($i = $start; $i < self::YYERRORSYMBOL + $start; $i++) {
                $lt = token_name($i);
                if (!isset($hash[$lt])) {
                    continue;
                }
                $lt = ($lt == 'T_ML_COMMENT') ? 'T_COMMENT' : $lt;
                $lt = ($lt == 'T_DOUBLE_T_COLON') ?  'T_PAAMAYIM_NEKUT_DOTAYIM' : $lt;
//                echo "$lt has hash? ".$hash[$lt]."\n";
//                continue;
                
                //echo "compare $lt with {$tokens[$i]}\n";
                $map[$i] = $hash[$lt];
            }
            //print_r($map);
            // set the map to false if nothing in there.
            self::$transTable = $map;
        }
    }
}

%left T_INCLUDE T_INCLUDE_ONCE T_EVAL T_REQUIRE T_REQUIRE_ONCE.
%left T_COMMA.
%left T_LOGICAL_OR.
%left T_LOGICAL_XOR.
%left T_LOGICAL_AND.
%right T_PRINT.
%left T_EQUAL T_T_PLUS_EQUAL T_T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL.
%left T_QUESTION T_COLON.
%left T_BOOLEAN_OR.
%left T_BOOLEAN_AND.
%left T_PIPE.
%left T_CIRCUMFLEX.
%left T_AMPERSAND.
%nonassoc T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL.
%nonassoc T_SMALLER T_IS_SMALLER_OR_EQUAL T_GREATER T_IS_GREATER_OR_EQUAL.
%left T_SL T_SR.
%left T_PLUS T_MINUS T_DOT.
%left T_STAR T_SLASH T_PERCENT.
%right T_EXCLAMATION.
%nonassoc T_INSTANCEOF.
%right T_TILDE T_INC T_DEC T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST T_AT.
%right T_SQUARE_OPEN.
%nonassoc T_NEW T_CLONE.
%left T_ELSEIF.
%left T_ELSE.
%left T_ENDIF.
%right T_STATIC T_ABSTRACT T_FINAL T_PRIVATE T_PROTECTED T_PUBLIC.

start ::= top_statement_list.

top_statement_list ::= top_statement_list top_statement.
top_statement_list ::= .

top_statement ::= statement.
top_statement ::= function_declaration_statement.
top_statement ::= class_declaration_statement.
top_statement ::= T_HALT_COMPILER T_PAREN_OPEN T_PAREN_CLOSE T_SEMI_COLON.

statement ::= unticked_statement.

unticked_statement ::= T_CURLY_OPEN inner_statement_list T_CURLY_CLOSE.
unticked_statement ::= T_IF T_PAREN_OPEN expr T_PAREN_CLOSE statement elseif_list else_single.
unticked_statement ::= T_IF T_PAREN_OPEN expr T_PAREN_CLOSE T_COLON inner_statement_list new_elseif_list new_else_single T_ENDIF T_COLON.
unticked_statement ::= T_WHILE T_PAREN_OPEN expr T_PAREN_CLOSE while_statement.
unticked_statement ::= T_DO statement T_WHILE T_PAREN_OPEN expr T_PAREN_CLOSE T_SEMI_COLON.
unticked_statement ::= T_FOR 
			T_PAREN_OPEN
				for_expr
			T_COLON 
				for_expr
			T_SEMI_COLON
				for_expr
			T_PAREN_CLOSE
			for_statement.
unticked_statement ::= T_SWITCH T_PAREN_OPEN expr T_PAREN_CLOSE switch_case_list.
unticked_statement ::= T_BREAK T_SEMI_COLON.
unticked_statement ::= T_BREAK expr T_SEMI_COLON.
unticked_statement ::= T_CONTINUE T_SEMI_COLON.
unticked_statement ::= T_CONTINUE expr T_SEMI_COLON.
unticked_statement ::= T_RETURN T_SEMI_COLON.
unticked_statement ::= T_RETURN expr_without_variable T_SEMI_COLON.
unticked_statement ::= T_RETURN variable T_SEMI_COLON.
unticked_statement ::= T_GLOBAL global_var_list T_SEMI_COLON.
unticked_statement ::= T_STATIC static_var_list T_SEMI_COLON.
unticked_statement ::= T_ECHO echo_expr_list T_SEMI_COLON.
unticked_statement ::= T_INLINE_HTML.
unticked_statement ::= expr T_SEMI_COLON.
unticked_statement ::= T_USE use_filename T_SEMI_COLON.
unticked_statement ::= T_UNSET T_PAREN_OPEN unset_variables T_PAREN_OPEN T_SEMI_COLON.
unticked_statement ::= T_FOREACH T_PAREN_OPEN variable T_AS 
		foreach_variable foreach_optional_arg T_PAREN_CLOSE
		foreach_statement.
unticked_statement ::= T_FOREACH T_PAREN_OPEN expr_without_variable T_AS 
		w_variable foreach_optional_arg T_PAREN_CLOSE
		foreach_statement.
unticked_statement ::= T_DECLARE T_PAREN_OPEN declare_list T_PAREN_CLOSE declare_statement.
unticked_statement ::= T_SEMI_COLON.
unticked_statement ::= T_TRY T_CURLY_OPEN inner_statement_list T_CURLY_CLOSE
		T_CATCH T_PAREN_OPEN
		fully_qualified_class_name
		T_VARIABLE T_PAREN_CLOSE
		T_CURLY_OPEN inner_statement_list T_CURLY_CLOSE
		additional_catches.
unticked_statement ::= T_THROW expr T_SEMI_COLON.

additional_catches ::= non_empty_additional_catches.
additional_catches ::= .

non_empty_additional_catches ::= additional_catch.
non_empty_additional_catches ::= non_empty_additional_catches additional_catch.

additional_catch ::= T_CATCH T_PAREN_OPEN fully_qualified_class_name T_VARIABLE T_PAREN_CLOSE T_CURLY_OPEN inner_statement_list T_CURLY_CLOSE.

inner_statement_list ::= inner_statement_list inner_statement.
inner_statement_list ::= .

inner_statement ::= statement.
inner_statement ::= function_declaration_statement.
inner_statement ::= class_declaration_statement.
inner_statement ::= T_HALT_COMPILER T_PAREN_OPEN T_PAREN_CLOSE T_SEMI_COLON.

statement ::= unticked_statement.

function_declaration_statement ::= unticked_function_declaration_statement.

class_declaration_statement ::= unticked_class_declaration_statement.

unticked_function_declaration_statement ::=
		T_FUNCTION is_reference T_STRING T_PAREN_OPEN parameter_list T_PAREN_CLOSE
		T_CURLY_OPEN inner_statement_list T_CURLY_CLOSE.

unticked_class_declaration_statement ::=
		class_entry_type T_STRING(C) extends_from
			implements_list
			T_CURLY_OPEN
				class_statement_list
			T_CURLY_CLOSE.
unticked_class_declaration_statement ::=
		interface_entry T_STRING
			interface_extends_list
			T_CURLY_OPEN
				class_statement_list
			T_CURLY_CLOSE.

class_entry_type ::= T_CLASS.
class_entry_type ::= T_ABSTRACT T_CLASS.
class_entry_type ::= T_FINAL T_CLASS.

extends_from ::= T_EXTENDS fully_qualified_class_name.
extends_from ::= .

interface_entry ::= T_INTERFACE.

interface_extends_list ::= T_EXTENDS interface_list.
interface_extends_list ::= .

implements_list ::= .
implements_list ::= T_IMPLEMENTS interface_list.

interface_list ::= fully_qualified_class_name.
interface_list ::= interface_list T_COMMA fully_qualified_class_name.

expr ::= r_variable.
expr ::= expr_without_variable.

expr_without_variable ::= T_LIST T_PAREN_OPEN assignment_list T_PAREN_CLOSE T_EQUAL expr.
expr_without_variable ::= variable T_EQUAL expr.
expr_without_variable ::= variable T_EQUAL T_AMPERSAND variable.
expr_without_variable ::= variable T_EQUAL T_AMPERSAND T_NEW class_name_reference ctor_arguments.
expr_without_variable ::= T_NEW class_name_reference ctor_arguments.
expr_without_variable ::= T_CLONE expr.
expr_without_variable ::= variable T_T_PLUS_EQUAL expr.
expr_without_variable ::= variable T_T_MINUS_EQUAL expr.
expr_without_variable ::= variable T_MUL_EQUAL expr.
expr_without_variable ::= variable T_DIV_EQUAL expr.
expr_without_variable ::= variable T_CONCAT_EQUAL expr.
expr_without_variable ::= variable T_MOD_EQUAL expr.
expr_without_variable ::= variable T_AND_EQUAL expr.
expr_without_variable ::= variable T_OR_EQUAL expr.
expr_without_variable ::= variable T_XOR_EQUAL expr.
expr_without_variable ::= variable T_SL_EQUAL expr.
expr_without_variable ::= variable T_SR_EQUAL expr.
expr_without_variable ::= rw_variable T_INC.
expr_without_variable ::= T_INC rw_variable.
expr_without_variable ::= rw_variable T_DEC.
expr_without_variable ::= T_DEC rw_variable.
expr_without_variable ::= expr T_BOOLEAN_OR expr.
expr_without_variable ::= expr T_BOOLEAN_AND expr.
expr_without_variable ::= expr T_LOGICAL_OR expr.
expr_without_variable ::= expr T_LOGICAL_AND expr.
expr_without_variable ::= expr T_LOGICAL_XOR expr.
expr_without_variable ::= expr T_PIPE expr.
expr_without_variable ::= expr T_AMPERSAND expr.
expr_without_variable ::= expr T_CIRCUMFLEX expr.
expr_without_variable ::= expr T_DOT expr.
expr_without_variable ::= expr T_PLUS expr.
expr_without_variable ::= expr T_MINUS expr.
expr_without_variable ::= expr T_STAR expr.
expr_without_variable ::= expr T_SLASH expr.
expr_without_variable ::= expr T_PERCENT expr.
expr_without_variable ::= expr T_SL expr.
expr_without_variable ::= expr T_SR expr.
expr_without_variable ::= T_PLUS expr.
expr_without_variable ::= T_MINUS expr.
expr_without_variable ::= T_EXCLAMATION expr.
expr_without_variable ::= T_TILDE expr.
expr_without_variable ::= expr T_IS_IDENTICAL expr.
expr_without_variable ::= expr T_IS_NOT_IDENTICAL expr.
expr_without_variable ::= expr T_IS_EQUAL expr.
expr_without_variable ::= expr T_IS_NOT_EQUAL expr.
expr_without_variable ::= expr T_SMALLER expr.
expr_without_variable ::= expr T_IS_SMALLER_OR_EQUAL expr.
expr_without_variable ::= expr T_GREATER expr.
expr_without_variable ::= expr T_IS_GREATER_OR_EQUAL expr.
expr_without_variable ::= expr T_INSTANCEOF class_name_reference.
expr_without_variable ::= T_PAREN_OPEN expr T_PAREN_CLOSE.
expr_without_variable ::= expr T_QUESTION
		expr T_COLON
		expr.
expr_without_variable ::= internal_functions_in_yacc.
expr_without_variable ::= T_INT_CAST expr.
expr_without_variable ::= T_DOUBLE_CAST expr.
expr_without_variable ::= T_STRING_CAST expr.
expr_without_variable ::= T_ARRAY_CAST expr.
expr_without_variable ::= T_OBJECT_CAST expr.
expr_without_variable ::= T_BOOL_CAST expr.
expr_without_variable ::= T_UNSET_CAST expr.
expr_without_variable ::= T_EXIT exit_expr.
expr_without_variable ::= T_AT expr.
expr_without_variable ::= scalar.
expr_without_variable ::= T_ARRAY T_PAREN_OPEN array_pair_list T_PAREN_CLOSE.
expr_without_variable ::= T_BACKQUOTE encaps_list T_BACKQUOTE.
expr_without_variable ::= T_PRINT expr.

exit_expr ::= T_PAREN_OPEN T_PAREN_CLOSE.
exit_expr ::= T_PAREN_OPEN expr T_PAREN_CLOSE.
exit_expr ::= .

common_scalar ::=
		T_LNUMBER
	   |T_DNUMBER
	   |T_CONSTANT_ENCAPSED_STRING
	   |T_LINE
	   |T_FILE
	   |T_CLASS_C
	   |T_METHOD_C
	   |T_FUNC_C.

/* compile-time evaluated scalars */
static_scalar ::= common_scalar.
static_scalar ::= T_STRING.
static_scalar ::= T_ARRAY T_PAREN_OPEN static_array_pair_list T_PAREN_CLOSE.
static_scalar ::= static_class_constant.

static_array_pair_list ::= non_empty_static_array_pair_list.
static_array_pair_list ::= non_empty_static_array_pair_list T_COMMA.
static_array_pair_list ::= .

non_empty_static_array_pair_list ::= non_empty_static_array_pair_list T_COMMA static_scalar T_DOUBLE_ARROW static_scalar.
non_empty_static_array_pair_list ::= non_empty_static_array_pair_list T_COMMA static_scalar.
non_empty_static_array_pair_list ::= static_scalar T_DOUBLE_ARROW static_scalar.
non_empty_static_array_pair_list ::= static_scalar.

static_class_constant ::= T_STRING T_PAAMAYIM_NEKUT_DOTAYIM T_STRING.

foreach_optional_arg ::= T_DOUBLE_ARROW foreach_variable.
foreach_optional_arg ::= .

foreach_variable ::= w_variable.
foreach_variable ::= T_AMPERSAND w_variable.

for_statement ::= statement.
for_statement ::= T_COLON inner_statement_list T_ENDFOR T_SEMI_COLON.

foreach_statement ::= statement.
foreach_statement ::= T_COLON inner_statement_list T_ENDFOREACH T_SEMI_COLON.


declare_statement ::= statement.
declare_statement ::= T_COLON inner_statement_list T_ENDDECLARE T_SEMI_COLON.

declare_list ::= T_STRING T_EQUAL static_scalar.
declare_list ::= declare_list T_COMMA T_STRING T_EQUAL static_scalar.

switch_case_list ::= T_CURLY_OPEN case_list T_CURLY_CLOSE.
switch_case_list ::= T_CURLY_OPEN T_SEMI_COLON case_list T_CURLY_CLOSE.
switch_case_list ::= T_COLON case_list T_ENDSWITCH T_SEMI_COLON.
switch_case_list ::= T_COLON T_SEMI_COLON case_list T_ENDSWITCH T_SEMI_COLON.

case_list ::= case_list T_CASE expr case_separator.
case_list ::= case_list T_DEFAULT case_separator inner_statement_list.
case_list ::= .

case_separator ::= T_COLON|T_SEMI_COLON.

while_statement ::= statement.
while_statement ::= T_COLON inner_statement_list T_ENDWHILE T_SEMI_COLON.

elseif_list ::= elseif_list T_ELSEIF T_PAREN_OPEN expr T_PAREN_CLOSE statement.
elseif_list ::= .

new_elseif_list ::= new_elseif_list T_ELSEIF T_PAREN_OPEN expr T_PAREN_CLOSE T_COLON inner_statement_list .
new_elseif_list ::= .

else_single ::= T_ELSE statement.
else_single ::= .

new_else_single ::= T_ELSE T_COLON inner_statement_list.
new_else_single ::= .

parameter_list ::= non_empty_parameter_list.
parameter_list ::= .

non_empty_parameter_list ::= optional_class_type T_VARIABLE.
non_empty_parameter_list ::= optional_class_type T_AMPERSAND T_VARIABLE.
non_empty_parameter_list ::= optional_class_type T_AMPERSAND T_VARIABLE T_EQUAL static_scalar.
non_empty_parameter_list ::= optional_class_type T_VARIABLE T_EQUAL static_scalar.
non_empty_parameter_list ::= non_empty_parameter_list T_COMMA optional_class_type T_VARIABLE.
non_empty_parameter_list ::= non_empty_parameter_list T_COMMA optional_class_type T_AMPERSAND T_VARIABLE.
non_empty_parameter_list ::= non_empty_parameter_list T_COMMA optional_class_type T_AMPERSAND T_VARIABLE T_EQUAL static_scalar.
non_empty_parameter_list ::= non_empty_parameter_list T_COMMA optional_class_type T_VARIABLE T_EQUAL static_scalar.


optional_class_type ::= T_STRING|T_ARRAY.
optional_class_type ::= .

function_call_parameter_list ::= non_empty_function_call_parameter_list.
function_call_parameter_list ::= .

non_empty_function_call_parameter_list ::= expr_without_variable.
non_empty_function_call_parameter_list ::= variable.
non_empty_function_call_parameter_list ::= T_AMPERSAND w_variable.
non_empty_function_call_parameter_list ::= non_empty_function_call_parameter_list T_COMMA expr_without_variable.
non_empty_function_call_parameter_list ::= non_empty_function_call_parameter_list T_COMMA variable.
non_empty_function_call_parameter_list ::= non_empty_function_call_parameter_list T_COMMA T_AMPERSAND w_variable.

global_var_list ::= global_var_list T_COMMA global_var.
global_var_list ::= global_var.

global_var ::= T_VARIABLE.
global_var ::= T_DOLLAR r_variable.
global_var ::= T_DOLLAR T_CURLY_OPEN expr T_CURLY_CLOSE.


static_var_list ::= static_var_list T_COMMA T_VARIABLE.
static_var_list ::= static_var_list T_COMMA T_VARIABLE T_EQUAL static_scalar.
static_var_list ::= T_VARIABLE.
static_var_list ::= T_VARIABLE T_EQUAL static_scalar.

class_statement_list ::= class_statement_list class_statement.
class_statement_list ::= .

class_statement ::= variable_modifiers class_variable_declaration T_SEMI_COLON.
class_statement ::= class_constant_declaration T_SEMI_COLON.
class_statement ::= method_modifiers T_FUNCTION is_reference T_STRING T_PAREN_OPEN parameter_list T_PAREN_CLOSE method_body.


method_body ::= T_SEMI_COLON. /* abstract method */
method_body ::= T_CURLY_OPEN inner_statement_list T_CURLY_CLOSE.

variable_modifiers ::= non_empty_member_modifiers.
variable_modifiers ::= T_VAR.

method_modifiers ::= non_empty_member_modifiers.
method_modifiers ::= .

non_empty_member_modifiers ::= member_modifier.
non_empty_member_modifiers ::= non_empty_member_modifiers member_modifier.

member_modifier ::= T_PUBLIC|T_PROTECTED|T_PRIVATE|T_STATIC|T_ABSTRACT|T_FINAL.

class_variable_declaration ::= class_variable_declaration T_COMMA T_VARIABLE.
class_variable_declaration ::= class_variable_declaration T_COMMA T_VARIABLE T_EQUAL static_scalar.
class_variable_declaration ::= T_VARIABLE.
class_variable_declaration ::= T_VARIABLE T_EQUAL static_scalar.

class_constant_declaration ::= class_constant_declaration T_COMMA T_STRING T_EQUAL static_scalar.
class_constant_declaration ::= T_CONST T_STRING T_EQUAL static_scalar.

echo_expr_list ::= echo_expr_list T_COMMA expr.
echo_expr_list ::= expr.

unset_variables ::= unset_variable.
unset_variables ::= unset_variables T_COMMA unset_variable.

unset_variable ::= variable.

use_filename ::= T_CONSTANT_ENCAPSED_STRING.
use_filename ::= T_CURLY_OPEN T_CONSTANT_ENCAPSED_STRING T_CURLY_CLOSE.

r_variable ::= variable.

w_variable ::= variable.

rw_variable ::= variable.

variable ::= base_variable_with_function_calls T_OBJECT_OPERATOR object_property method_or_not variable_properties.
variable ::= base_variable_with_function_calls.

variable_properties ::= variable_properties variable_property.
variable_properties ::= .

variable_property ::= T_OBJECT_OPERATOR object_property method_or_not.

method_or_not ::= T_PAREN_OPEN function_call_parameter_list T_PAREN_CLOSE.
method_or_not ::= .

variable_without_objects ::= reference_variable.
variable_without_objects ::= simple_indirect_reference reference_variable.

static_member ::= fully_qualified_class_name T_PAAMAYIM_NEKUT_DOTAYIM variable_without_objects.

base_variable_with_function_calls ::= base_variable.
base_variable_with_function_calls ::= function_call.

base_variable ::= reference_variable.
base_variable ::= simple_indirect_reference reference_variable.
base_variable ::= static_member.
	
reference_variable ::= reference_variable T_SQUARE_OPEN dim_offset T_SQUARE_CLOSE.
reference_variable ::= reference_variable T_CURLY_OPEN expr T_CURLY_CLOSE.
reference_variable ::= compound_variable.

compound_variable ::= T_VARIABLE.
compound_variable ::= T_DOLLAR T_CURLY_OPEN expr T_CURLY_CLOSE.

dim_offset ::= expr.
dim_offset ::= .

object_property ::= object_dim_list.
object_property ::= variable_without_objects.

object_dim_list ::= object_dim_list T_SQUARE_OPEN dim_offset T_SQUARE_CLOSE.
object_dim_list ::= object_dim_list T_CURLY_OPEN expr T_CURLY_CLOSE.
object_dim_list ::= variable_name .

variable_name ::= T_STRING.
variable_name ::= T_CURLY_OPEN expr T_CURLY_CLOSE.

simple_indirect_reference ::= T_DOLLAR.
simple_indirect_reference ::= simple_indirect_reference T_DOLLAR.

assignment_list ::= assignment_list T_COMMA assignment_list_element.
assignment_list ::= assignment_list_element.

assignment_list_element ::= variable.
assignment_list_element ::= T_LIST T_PAREN_OPEN assignment_list T_PAREN_CLOSE.
assignment_list_element ::= .

array_pair_list ::= non_empty_array_pair_list possible_comma.
array_pair_list ::= .

non_empty_array_pair_list ::= non_empty_array_pair_list T_COMMA expr T_DOUBLE_ARROW expr.
non_empty_array_pair_list ::= non_empty_array_pair_list T_COMMA expr.
non_empty_array_pair_list ::= expr T_DOUBLE_ARROW expr.
non_empty_array_pair_list ::= expr.
non_empty_array_pair_list ::= non_empty_array_pair_list T_COMMA expr T_DOUBLE_ARROW T_AMPERSAND w_variable.
non_empty_array_pair_list ::= non_empty_array_pair_list T_COMMA T_AMPERSAND w_variable.
non_empty_array_pair_list ::= expr T_DOUBLE_ARROW T_AMPERSAND w_variable.
non_empty_array_pair_list ::= T_AMPERSAND w_variable.

encaps_list ::= encaps_list encaps_var.
encaps_list ::= encaps_list T_STRING.
encaps_list ::= encaps_list T_NUM_STRING.
encaps_list ::= encaps_list T_ENCAPSED_AND_WHITESPACE.
encaps_list ::= encaps_list T_CHARACTER.
encaps_list ::= encaps_list T_BAD_CHARACTER.
encaps_list ::= encaps_list T_SQUARE_OPEN.
encaps_list ::= encaps_list T_SQUARE_CLOSE.
encaps_list ::= encaps_list T_CURLY_OPEN.
encaps_list ::= encaps_list T_CURLY_CLOSE.
encaps_list ::= encaps_list T_OBJECT_OPERATOR.
encaps_list ::= .



encaps_var ::= T_VARIABLE.
encaps_var ::= T_VARIABLE T_SQUARE_OPEN encaps_var_offset T_SQUARE_CLOSE.
encaps_var ::= T_VARIABLE T_OBJECT_OPERATOR T_STRING.
encaps_var ::= T_T_DOLLAR_OPEN_CURLY_BRACES expr T_CURLY_CLOSE.
encaps_var ::= T_T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME T_SQUARE_OPEN expr T_SQUARE_CLOSE T_CURLY_CLOSE.
encaps_var ::= T_CURLY_OPEN variable T_CURLY_CLOSE.

encaps_var_offset ::= T_STRING|T_NUM_STRING|T_VARIABLE.

internal_functions_in_yacc ::= T_ISSET T_PAREN_OPEN isset_variables T_PAREN_CLOSE.
internal_functions_in_yacc ::= T_EMPTY T_PAREN_OPEN variable T_PAREN_CLOSE.
internal_functions_in_yacc ::= T_INCLUDE expr.
internal_functions_in_yacc ::= T_INCLUDE_ONCE expr.
internal_functions_in_yacc ::= T_EVAL T_PAREN_OPEN expr T_PAREN_CLOSE.
internal_functions_in_yacc ::= T_REQUIRE expr.
internal_functions_in_yacc ::= T_REQUIRE_ONCE expr.

isset_variables ::= variable.
isset_variables ::= isset_variables T_COMMA variable.

class_constant ::= fully_qualified_class_name T_PAAMAYIM_NEKUT_DOTAYIM T_STRING.

fully_qualified_class_name ::= T_STRING.

function_call ::= T_STRING T_PAREN_OPEN function_call_parameter_list T_PAREN_CLOSE.
function_call ::= fully_qualified_class_name T_PAAMAYIM_NEKUT_DOTAYIM T_STRING T_PAREN_OPEN function_call_parameter_list T_PAREN_CLOSE.
function_call ::= fully_qualified_class_name T_PAAMAYIM_NEKUT_DOTAYIM variable_without_objects T_PAREN_OPEN function_call_parameter_list T_PAREN_CLOSE.
function_call ::= variable_without_objects T_PAREN_OPEN function_call_parameter_list T_PAREN_CLOSE.

scalar ::= T_STRING.
scalar ::= T_STRING_VARNAME.
scalar ::= class_constant.
scalar ::= common_scalar.
scalar ::= T_DQUOTE encaps_list T_DQUOTE.
scalar ::= T_SQUOTE encaps_list T_SQUOTE.
scalar ::= T_START_HEREDOC encaps_list T_END_HEREDOC.

class_name_reference ::= T_STRING.
class_name_reference ::= dynamic_class_name_reference.

dynamic_class_name_reference ::= base_variable T_OBJECT_OPERATOR object_property dynamic_class_name_variable_properties.
dynamic_class_name_reference ::= base_variable.

dynamic_class_name_variable_properties ::= dynamic_class_name_variable_properties dynamic_class_name_variable_property.
dynamic_class_name_variable_properties ::= .

dynamic_class_name_variable_property ::= T_OBJECT_OPERATOR object_property.

ctor_arguments ::= T_PAREN_OPEN function_call_parameter_list T_PAREN_CLOSE.
ctor_arguments ::= .

possible_comma ::= T_COMMA.
possible_comma ::= .

for_expr ::= non_empty_for_expr.
for_expr ::= .

non_empty_for_expr ::= non_empty_for_expr T_COMMA expr.
non_empty_for_expr ::= expr.

is_reference ::= T_AMPERSAND.
is_reference ::= .
