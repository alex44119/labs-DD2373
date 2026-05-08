(**
  server.ml --- Some usefull commands for launching the webpage
*)

let launch_server (port:int) (time:int) =
  let cmd =
    if Sys.os_type = "Win32" then
      Printf.sprintf
        "powershell -Command \"$p = Start-Process python -ArgumentList '-m http.server %d' -PassThru -WindowStyle Hidden; Start-Sleep -Seconds %d; Stop-Process -Id $p.Id\""
        port time
    else
      Printf.sprintf
        "( python3 -m http.server %d > /dev/null 2>&1 & pid=$!; sleep %d; kill $pid ) &"
        port time
  in
  ignore (Sys.command cmd)
;;