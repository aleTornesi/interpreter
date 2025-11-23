type lexer = {
    input: string;
    position: int;
    ch: char option;
}



let readChar l =
    let completed_input_consumption = l.position + 1 >= String.length l.input in
    match completed_input_consumption with
    | true -> { input = l.input; position = (l.position + 1); ch = None }
    | false  -> { input = l.input; position = (l.position + 1); ch = Some(String.get l.input (l.position + 1)) }

let peekChar = function
    | {input = i; position = pos; _} when pos == String.length i -> None
    | {input = i; position = pos; _} -> Some(String.get i (pos + 1))

let init s = {input = s; position = -1; ch = None}

let is_letter c =
    let is_uppercase_letter = Char.code c >= Char.code 'A' && Char.code c <= Char.code 'Z' in
    let is_lowercase_letter = Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z' in
    is_lowercase_letter || is_uppercase_letter

let is_digit c = Char.code c >= Char.code '0' && Char.code c <= Char.code '9'

let rec read_identifier l =
    let helper c (s, new_lexer) = String.make 1 c ^ s, new_lexer in
    match l.ch with
    | None -> "", l
    | Some(c) -> match is_letter c || c == '_' with
        | true -> helper c (read_identifier (readChar l))
        | false -> let {input = s; position = p; ch = c} = l in "", {input = s; position = p - 1; ch = c}

let rec read_number l =
    let helper c (s, new_lexer) = String.make 1 c ^ s, new_lexer in
    match l.ch with
    | None -> "", l
    | Some(c) -> match is_digit c with
        | true -> helper c (read_number (readChar l))
        | false -> let {input = s; position = p; ch = c} = l in "", {input = s; position = p - 1; ch = c}

let identifier_matching = function
    | "let" -> (Let: Tokens.t)
    | "fn" -> Function
    | "true" -> True
    | "false" -> False
    | "return" -> Return
    | "if" -> If
    | "else" -> Else
    | s -> Identifier(s)
let peekForEqual l tEq t = match peekChar l with
    | Some('=') -> tEq, readChar l
    | _ -> t, l


let rec get_token l =
    match l.ch with
    | None -> (EOF : Tokens.t), l
    | Some('=') -> peekForEqual l (Equal: Tokens.t) Assign
    | Some(';') -> Semicolon, l
    | Some('(') -> LParenthesis, l
    | Some(')') -> RParenthesis, l
    | Some(',') -> Comma, l
    | Some('+') -> Plus, l
    | Some('{') -> LBrace, l
    | Some('}') -> RBrace, l
    | Some('>') -> GreaterThen, l
    | Some('<') -> LessThen, l
    | Some('!') -> peekForEqual l (NotEqual: Tokens.t) Not
    | Some('-') -> Minus, l
    | Some('*') -> Product, l
    | Some('/') -> Division, l
    | Some(' ')
        | Some('\t')
        | Some('\n')
        | Some('\r') -> get_token (readChar l)
    | Some(c) -> match (is_letter c) || c == '_' with
        | true -> let (s, new_l) = read_identifier l in identifier_matching s, new_l
        | false -> match is_digit c with
            | true -> let (s, new_l) = read_number l in Int(int_of_string(s)), new_l
            | false -> Illegal, l



let parse s =
    let rec helper l = match l with
        | { input = string; position = pos; _} when pos = (String.length string) -> []
        | _ -> let (t, newLexer) = get_token (readChar l) in t::(helper newLexer)
    in
    helper (init s)

