let () =
    let input = In_channel.input_all In_channel.stdin
    in
    print_endline (Example.Elm.format_test input)
