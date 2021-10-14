%name YuLangLexer;

%let id = [^「」\n\t ];
(* %let id = CCHAR; *)

%defs (
  structure T = YuLangTokens
  type lex_result = T.token
  fun eof() = T.EOF
);

{id} => ( T.ID yytext );
"「" => (T.LEFT_PAREN);
"」" => (T.RIGHT_PAREN) ;

" " | \n | \t => ( skip() );