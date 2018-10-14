open System

let one_letter = ['h';'b';'c';'n';'o';'f';'p';'s';'k';'v';'y';'i';'w';'u']

let two_letter = [('h','e');('l','i');('b','e');('n','e');('n','a');
                    ('m','g');('a','l');('s','i');('c','l');('a','r');
                    ('c','a');('s','c');('t','i');('c','r');('m','n');
                    ('f','e');('c','o');('n','i');('c','u');('z','n');
                    ('g','a');('g','e');('a','s');('s','e');('b','r');
                    ('k','r');('r','b');('s','r');('z','r');('n','b');
                    ('m','o');('t','c');('r','u');('r','h');('p','d');
                    ('a','g');('c','d');('i','n');('s','n');('s','b');
                    ('t','e');('x','e');('c','s');('b','a');('h','f');
                    ('t','a');('r','e');('o','s');('i','r');('p','t');
                    ('a','u');('h','g');('t','l');('p','b');('b','i');
                    ('p','o');('a','t');('r','n');('f','r');('r','a');
                    ('r','f');('d','b');('s','g');('b','h');('h','s');
                    ('m','t');('d','s');('r','g');('c','n');('n','h');
                    ('f','i');('m','c');('l','v');('t','s');('o','g');
                    ('l','a');('c','e');('p','r');('n','d');('p','m');
                    ('s','m');('e','u');('g','d');('t','b');('d','y');
                    ('h','o');('e','r');('t','m');('y','b');('l','u');
                    ('a','c');('t','h');('p','a');('n','p');('p','u');
                    ('a','m');('c','m');('b','k');('c','f');('e','s');
                    ('f','m');('m','d');('n','o');('l','r')]
                    
   
let rec find_matches (s : char list) : Option<char list> list =
    let append_char c list =
        List.map (Option.map (fun x -> c::x)) list
    match s with
    | hd::hd2::tail ->
        let one = List.exists ((=) hd) one_letter
        let two = List.exists ((=) (hd,hd2)) two_letter
        match (one, two) with
        | (true , true) ->
            let results_one = 
                find_matches (hd2::tail)
                |> append_char ','
                |> append_char hd 
            let results_two = 
                find_matches tail
                |> append_char ','
                |> append_char hd2
                |> append_char hd
            List.append results_one results_two
        | (false , true) ->
            find_matches (tail)
            |> append_char ','
            |> append_char hd2
            |> append_char hd           
        | (true , false) ->
            find_matches (hd2::tail)
            |> append_char ','
            |> append_char hd 
        | (false , false) ->
            None::[]
    | hd::tail -> 
        if List.exists ((=) hd) one_letter then
            find_matches tail
            |> append_char ','
            |> append_char hd
        else
            None::[]
    | [] -> (Some [])::[]
    
    
[<EntryPoint>]
let main argv = 
    match argv with 
    | [|first|] -> 
        first |> Seq.toList
        |> find_matches
        |> List.iter (Option.iter (fun chars -> chars |> List.toArray |> (fun s -> System.String s) |> printfn "%A"));
        0
    | _ -> failwith "Must have only one argument."