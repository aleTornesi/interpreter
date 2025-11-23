type t =
    Let |
    Int of int |
    Identifier of string |
    Function |
    Illegal |
    EOF |
    Assign |
    Plus |
    Minus |
    Product |
    Division |
    Comma |
    Semicolon |
    LParenthesis |
    RParenthesis |
    LBrace |
    RBrace |
    Equal |
    NotEqual |
    LessThen |
    GreaterThen |
    Not |
    True |
    False |
    If |
    Else |
    Return



let pp ppf = function
    | Let -> Format.fprintf ppf "Let"
    | Int(n) -> Format.fprintf ppf "Int(%d)" n
    | Identifier(id) -> Format.fprintf ppf "Identifier(%s)" id
    | Function -> Format.fprintf ppf "function"
    | Illegal -> Format.fprintf ppf "ILLEGAL"
    | EOF -> Format.fprintf ppf "EOF"
    | Assign -> Format.fprintf ppf "Assign"
    | Plus -> Format.fprintf ppf "Plus"
    | Minus -> Format.fprintf ppf "Minus"
    | Product -> Format.fprintf ppf "Product"
    | Division -> Format.fprintf ppf "Division"
    | Comma -> Format.fprintf ppf "Comma"
    | Semicolon -> Format.fprintf ppf "Semicolon"
    | LParenthesis -> Format.fprintf ppf "Left Parenthesis"
    | RParenthesis -> Format.fprintf ppf "Right Parenthesis"
    | LBrace -> Format.fprintf ppf "Left Brace"
    | RBrace -> Format.fprintf ppf "Right Brace"
    | Equal -> Format.fprintf ppf "Equal"
    | NotEqual -> Format.fprintf ppf "Not equal"
    | LessThen -> Format.fprintf ppf "Less than"
    | GreaterThen -> Format.fprintf ppf "Greater than"
    | Not -> Format.fprintf ppf "Not"
    | True -> Format.fprintf ppf "true"
    | False -> Format.fprintf ppf "false"
    | If -> Format.fprintf ppf "if"
    | Else -> Format.fprintf ppf "else"
    | Return -> Format.fprintf ppf "return"
