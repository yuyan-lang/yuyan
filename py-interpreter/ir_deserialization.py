
from __future__ import annotations

from ast_util import *

import json
from enum import Enum, auto
from tqdm import tqdm 

class TokenType(Enum):
    IDENTIFIER = auto()
    STRING_LITERAL = auto()
    NUMBER = auto()
    DOT = auto()
    OPEN_SQUARE_BRACKET = auto()
    CLOSE_SQUARE_BRACKET = auto()
    OPEN_PARENTHESIS = auto()
    CLOSE_PARENTHESIS = auto()
    SEMICOLON = auto()
    COLON = auto()
    COMMA = auto()
    NODE_TYPE = auto() # value is NodeType
    NODE = auto()  # value is Node
    EOF = auto()  # End of input


@dataclass
class Token:
    type: TokenType
    value: Any = None


def unescape_string(s: str) -> str:
    """
    Reverts a json-escaped string,
    e.g. unescape_string('"a\\"b"') -> 'a"b'
    """
    return json.loads(s)

class Tokenizer:
    def __init__(self, input_str: str, filename: Optional[str]):
        self.input_str = input_str
        self.pos = 0
        self.cur_line = 1
        self.cur_col = 1
        self.filename = filename
        self.pbar = tqdm(total=len(input_str), desc=f"Parsing {filename if filename else '<None>'}")

    def increment_pos(self):
        self.pos += 1
        self.pbar.update(1)
        if self.input_str[self.pos-1] == '\n':
            self.cur_line += 1
            self.cur_col = 0
        else:
            self.cur_col += 1

    def show_cur_pos(self):
        return f"{self.filename if self.filename else '<None>'}:{self.cur_line}:{self.cur_col}"



    def next_token(self) -> Token:
        if self.pos >= len(self.input_str):
            if self.pbar:
                self.pbar.close()
                self.pbar = None
            return Token(TokenType.EOF)

        current_char = self.input_str[self.pos]

        if current_char.isspace():
            self.increment_pos()
            return self.next_token()

        if current_char == '.':
            self.increment_pos()
            return Token(TokenType.DOT)
        
        if current_char == '[':
            self.increment_pos()
            return Token(TokenType.OPEN_SQUARE_BRACKET)
        
        if current_char == ']':
            self.increment_pos()
            return Token(TokenType.CLOSE_SQUARE_BRACKET)

        if current_char == ';':
            self.increment_pos()
            return Token(TokenType.SEMICOLON)
        
        if current_char == '(':
            self.increment_pos()
            return Token(TokenType.OPEN_PARENTHESIS)

        if current_char == ')':
            self.increment_pos()
            return Token(TokenType.CLOSE_PARENTHESIS)

        if current_char == ':':
            self.increment_pos()
            return Token(TokenType.COLON)

        if current_char == ',':
            self.increment_pos()
            return Token(TokenType.COMMA)
        

        if current_char == '"':
            return self._parse_string_literal()

        if current_char.isdigit():
            return self._parse_number()

        # Default: parse identifier
        return self._parse_identifier()

    def _parse_string_literal(self) -> Token:
        assert self.input_str[self.pos] == '"'
        self.increment_pos()
        start = self.pos
        while self.pos < len(self.input_str):
            if self.input_str[self.pos] == '\\':
                # self.pos += 2  # Skip escaped character
                self.increment_pos()
                self.increment_pos()
            elif self.input_str[self.pos] == '"':
                string_value = unescape_string(self.input_str[start-1:self.pos+1])
                self.increment_pos()
                return Token(TokenType.STRING_LITERAL, string_value)
            else:
                self.increment_pos()
        raise ValueError("Unterminated string literal")

    def _parse_number(self) -> Token:
        start = self.pos
        while self.pos < len(self.input_str) and self.input_str[self.pos].isdigit():
            self.increment_pos()
        return Token(TokenType.NUMBER, int(self.input_str[start:self.pos]))

    def _parse_identifier(self) -> Token:
        start = self.pos
        while self.pos < len(self.input_str) and self.input_str[self.pos] not in ".[];\"():":
            self.increment_pos()
        return Token(TokenType.IDENTIFIER, self.input_str[start:self.pos])


class ShiftReduceParser:
    def __init__(self, tokenizer: Tokenizer):
        self.tokenizer = tokenizer
        self.stack : List[Token] = []
        self.parsed_nodes : Dict[str, Abt] = {}
        self.current_token = self.tokenizer.next_token()

    def parse(self) -> Dict[str, Abt]:
        try:
            while True:
                self.shift_or_reduce()
                if len(self.stack) == 0 and self.current_token.type == TokenType.EOF:
                    break
            return self.parsed_nodes
        except ValueError as e:
            print("Error encountered when parsing file ", self.tokenizer.show_cur_pos())
        
            raise 


    def pop_stack_sep_end_by(self, sep: TokenType, end: TokenType) -> List[Token]:
        """
        pops ... end ELEM sep ELEM ... sep ELEM
        i.e. ... end ((ELEM sep)* ELEM)?
        """
        result: List[Token] = []
        if len(self.stack) == 0:
            raise ValueError("Unexpected end of input", result, sep, end, self.current_token)
        
        if self.stack[-1].type == end:
            self.stack.pop()
            return result

        while True:
            if len(self.stack) <= 1:
                raise ValueError("Unexpected end of input", result, sep, end, self.current_token)
            elif self.stack[-2].type == end:
                result.append(self.stack.pop())
                self.stack.pop()
                break
            elif self.stack[-2].type == sep:
                result.append(self.stack.pop())
                self.stack.pop()
            else:
                raise ValueError("Unexpected token", result, sep, end, self.current_token)
        
        return list(reversed(result))


    def shift_or_reduce(self):
        # print(self.tokenizer.show_cur_pos(), self.current_token)

        # (start) "name" : node -> store result
        if len(self.stack) == 3 and self.stack[-3].type == TokenType.STRING_LITERAL and self.stack[-2].type == TokenType.COLON and self.stack[-1].type == TokenType.NODE:
            node = self.stack.pop()
            self.stack.pop()
            name = self.stack.pop()
            self.parsed_nodes[name.value] = node.value
            return

        # always reduce bindings first
        # id . node -> Binding id node
        if (len(self.stack) >= 3 and self.stack[-3].type == TokenType.IDENTIFIER and self.stack[-2].type == TokenType.DOT 
            and (self.stack[-1].type == TokenType.NODE or self.stack[-1].type == TokenType.STRING_LITERAL or self.stack[-1].type == TokenType.NUMBER)):
            node = self.stack.pop()
            self.stack.pop()
            id = self.stack.pop()
            node_value = map_node_arg_type(node)
            self.stack.append(Token(TokenType.NODE, Binding(id.value if id.value != "_" else None, node_value)))
            return

        def shift():
            self.stack.append(self.current_token)
            self.current_token = self.tokenizer.next_token()


        match self.current_token.type:
            case TokenType.EOF:
                if len(self.stack) != 0:
                    raise ValueError("Unexpected end of input", self.stack)
                return
            case TokenType.IDENTIFIER | TokenType.STRING_LITERAL | TokenType.NUMBER | TokenType.SEMICOLON | TokenType.COLON | TokenType.COMMA:
                shift()
                return
            case TokenType.OPEN_PARENTHESIS:
                if len(self.stack) == 0:
                    raise ValueError("Unexpected token", self.current_token)
                if self.stack[-1].type != TokenType.IDENTIFIER:
                    raise ValueError(f"Expected IDENTIFIER, got {self.stack[-1]}")
                shift()
                return
            case TokenType.OPEN_SQUARE_BRACKET:
                if len(self.stack) == 0:
                    raise ValueError("Unexpected token", self.current_token)
                if self.stack[-1].type != TokenType.NODE_TYPE and self.stack[-1].type != TokenType.IDENTIFIER:
                    raise ValueError(f"Expected NODE_TYPE or IDENTIFIER, got {self.stack[-1]}")
                shift()
                return
            case TokenType.DOT:
                if len(self.stack) == 0:
                    raise ValueError("Unexpected token", self.current_token)
                if self.stack[-1].type != TokenType.IDENTIFIER:
                    raise ValueError(f"Expected IDENTIFIER, got {self.stack[-1]}")
                shift()
                return
            case TokenType.CLOSE_PARENTHESIS:
                args = self.pop_stack_sep_end_by(TokenType.COMMA, TokenType.OPEN_PARENTHESIS)
                node_type_str = self.stack.pop()
                if node_type_str.type != TokenType.IDENTIFIER:
                    raise ValueError(f"Expected IDENTIFIER, got {node_type_str}")
                assert node_type_str.type == TokenType.IDENTIFIER, f"Expected IDENTIFIER, got {node_type_str}"
                nt : NodeType = ir_to_ast_node(node_type_str.value, args)
                self.stack.append((Token(TokenType.NODE_TYPE, nt)))
                self.current_token = self.tokenizer.next_token()
                return
            case TokenType.CLOSE_SQUARE_BRACKET:
                args = self.pop_stack_sep_end_by(TokenType.SEMICOLON, TokenType.OPEN_SQUARE_BRACKET)
                node_type_str = self.stack.pop()
                if node_type_str.type == TokenType.IDENTIFIER:
                    node_type = ir_to_ast_node(node_type_str.value, [])
                elif node_type_str.type == TokenType.NODE_TYPE:
                    node_type = node_type_str.value
                else:
                    raise ValueError(f"Expected IDENTIFIER or NODE_TYPE, got {node_type_str}")
                n = N(node_type, [map_node_arg_type(a) for a in args])
                self.stack.append(Token(TokenType.NODE, n))
                self.current_token = self.tokenizer.next_token()
                return
            case _:
                raise ValueError(f"Unexpected token {self.current_token}")
            

def map_node_arg_type(arg: Token) -> Abt:
    if arg.type == TokenType.NODE:
        return arg.value
    if arg.type == TokenType.STRING_LITERAL:
        return FreeVar(arg.value)
    if arg.type == TokenType.NUMBER:
        return BoundVar(arg.value)
    raise ValueError(f"Unexpected token {arg}")
                
                    


def ir_to_ast_node(ir: str, args: List[Token]) -> NodeType:
    match ir, args:
        case "EmptyStructEntry", []:
            return NT_EmptyStructEntry()
        case "StructEntry", []:
            return NT_StructEntry(None)
        case "StructEntry", [Token(TokenType.STRING_LITERAL, label)]:
            return NT_StructEntry((label))
        case "EmptyVal", []:
            return NT_EmptyVal()
        case "FileRef", [Token(TokenType.STRING_LITERAL, filename)]:
            return NT_FileRef((filename))
        case "Builtin", [Token(TokenType.STRING_LITERAL, name)]:
            return NT_Builtin((name))
        case "TupleProj", [Token(TokenType.NUMBER, idx)]:
            return NT_TupleProj(idx)
        case "AnnotatedVar", [Token(TokenType.STRING_LITERAL, annotation)]:
            return NT_AnnotatedVar((annotation))
        case "MultiArgLam", [Token(TokenType.NUMBER, arg_count)]:
            return NT_MultiArgLam(arg_count)
        case "MultiArgFuncCall", [Token(TokenType.NUMBER, arg_count)]:
            return NT_MultiArgFuncCall(arg_count)
        case "TupleCons", []:
            return NT_TupleCons()
        case "DataTupleCons", [Token(TokenType.NUMBER, idx), Token(TokenType.NUMBER, length)]:
            return NT_DataTupleCons(idx, length)
        case "DataTupleProjIdx", []:
            return NT_DataTupleProjIdx()
        case "DataTupleProjTuple", []:
            return NT_DataTupleProjTuple()
        case "IfThenElse", []:
            return NT_IfThenElse()
        case "StringConst", [Token(TokenType.STRING_LITERAL, val)]:
            return NT_StringConst((val))
        case "IntConst", [Token(TokenType.NUMBER, val)]:
            return NT_IntConst(val)
        case "ExternalCall", [Token(TokenType.STRING_LITERAL, name)]:
            return NT_ExternalCall((name))
        case "LetIn", []:
            return NT_LetIn()
        case "ConsecutiveStmt", []:
            return NT_ConsecutiveStmt()
        case "CallCC", []:
            return NT_CallCC()
        case "CallCCRet", []:
            return NT_CallCCRet()
        case "GlobalFuncRef", [Token(TokenType.STRING_LITERAL, name)]:
            return NT_GlobalFuncRef((name))
        case "GlobalFuncDecl", [Token(TokenType.STRING_LITERAL, name)]:
            return NT_GlobalFuncDecl((name))
        case "WriteGlobalFileRef", [Token(TokenType.STRING_LITERAL, filename)]:
            return NT_WriteGlobalFileRef((filename))
        case "UpdateStruct", [Token(TokenType.NUMBER, index)]:
            return NT_UpdateStruct(index)
        case "StructRec", labels:
            assert all(label.type == TokenType.STRING_LITERAL for label in labels)
            return NT_StructRec([(label.value) for label in labels])
        case "DecimalNumber", [Token(TokenType.STRING_LITERAL, integral), Token(TokenType.STRING_LITERAL, fractional)]:
            return NT_DecimalNumber((integral), (fractional))
        case _:
            raise ValueError(f"Unknown IR node type: ", ir, args)
        


        

    
    

def ir_to_asts(input_str: str, filename: Optional[str] = None) -> Dict[str, Abt]:
    # Example usage
    tokenizer = Tokenizer(input_str, filename)
    parser = ShiftReduceParser(tokenizer)
    result = parser.parse()
        
    return result