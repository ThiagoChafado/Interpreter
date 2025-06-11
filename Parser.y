{
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token
  num      { TokenNum $$ }
  true     { TokenTrue }
  false    { TokenFalse }
  '+'      { TokenAdd }
  '*'      { TokenMul }
  '-'      { TokenSub }
  '&&'     { TokenAnd }
  if       { TokenIf }
  then     { TokenThen }
  else     { TokenElse }
  '['      { TokenLBracket }
  ']'      { TokenRBracket }
  ','      { TokenComma }

%left '+'
%left '&&'
%left '*'
%left '-'

%%








-- Regra principal para expressões
Exp :
    Atom                        { $1 }
  | Exp '+' Exp                 { Add $1 $3 }
  | Exp '-' Exp                 { Sub $1 $3 }
  | Exp '*' Exp                 { Mul $1 $3 }
  | Exp '&&' Exp                { And $1 $3 }
  | if Exp then Exp else Exp    { If $2 $4 $6 }

-- Regra para "átomos": literais e expressões que não podem ser quebradas, como listas.
Atom :
    num                         { Num $1 }
  | true                        { BTrue }
  | false                       { BFalse }
  | '[' ']'                     { MyList [] }
  | '[' Exps ']'                { MyList $2 }

-- Regra auxiliar apenas para o CONTEÚDO de uma lista (expressões separadas por vírgula)
Exps :
    Exp                         { [$1] }
  | Exp ',' Exps                { $1 : $3 }

{
parseError :: [Token] -> a
parseError _ = error "Erro Sintático"
}