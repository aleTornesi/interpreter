let prompt = ">> "

let run () =
    Printf.printf "%s" prompt;
    let line = read_line () in
    let tokens = Parser.parse line in
    List.iter (fun t -> Format.printf "%a, " Tokens.pp t) tokens;
    print_newline ()

let start () =
    while true do
        run ()
    done
