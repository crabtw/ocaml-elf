let () =
  if Array.length Sys.argv <> 2 then
    prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <file>")
  else
    let f = open_in_bin Sys.argv.(1) in
    let buf = String.create (in_channel_length f) in
    let () = really_input f buf 0 (String.length buf) in
    let () = close_in f in
    let elf = Elf.parse buf in
    match elf with
    | None -> prerr_endline "parse failed"
    | Some elf -> Printf.printf "entry: %x\n" (Int64.to_int elf.Elf.e_entry)
