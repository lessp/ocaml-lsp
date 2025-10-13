open Test.Import

let client_capabilities = ClientCapabilities.create ()

module T : sig
  val run : (unit Client.t -> 'a Fiber.t) -> 'a
end = struct
  let run f =
    let status, a = Test.run_with_status f in
    let () =
      match status with
      | WEXITED n -> Format.eprintf "ocamllsp finished with code = %d@.%!" n
      | WSIGNALED s -> Format.eprintf "ocamllsp killed with signal = %d@.%!" s
      | WSTOPPED s -> Format.eprintf "ocamllsp stopped with signal = %d@.%!" s
    in
    a
  ;;
end

let test run =
  T.run (fun client ->
    let run_client () =
      Client.start client (InitializeParams.create ~capabilities:client_capabilities ())
    in
    Fiber.fork_and_join_unit run_client (run client))
;;

let%expect_test
    "ocamllsp process exits with code 0 after Shutdown and Exit notifications are sent"
  =
  let run client () =
    let* (_ : InitializeResult.t) = Client.initialized client in
    let* () = Client.request client Shutdown in
    Client.notification client Exit
  in
  test run;
  [%expect
    {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    dropped notification
    Uncaught error when handling notification:
    {
      "params": {
        "message": "Unable to find 'ocamlformat-rpc' binary. Types on hover may not be well-formatted. You need to install either 'ocamlformat' of version > 0.21.0 or, otherwise, 'ocamlformat-rpc' package.",
        "type": 3
      },
      "method": "window/showMessage",
      "jsonrpc": "2.0"
    }
    Error:
    [ { exn =
          "(\"unexpected notification\",\n\
          \ { notification =\n\
          \     { params =\n\
          \         { message =\n\
          \             \"Unable to find 'ocamlformat-rpc' binary. Types on hover may not be well-formatted. You need to install either 'ocamlformat' of version > 0.21.0 or, otherwise, 'ocamlformat-rpc' package.\"\n\
          \         ; type = 3\n\
          \         }\n\
          \     ; method = \"window/showMessage\"\n\
          \     ; jsonrpc = \"2.0\"\n\
          \     }\n\
          \ })"
      ; backtrace =
          "Raised at Stdune__Code_error.raise in file \"otherlibs/stdune/src/code_error.ml\", line 10, characters 30-62\n\
           Called from Lsp_fiber__Rpc.Client.h_on_notification in file \"lsp-fiber/src/rpc.ml\", line 362, characters 17-46\n\
           Called from Fiber__Scheduler.exec in file \"fiber/src/scheduler.ml\", line 73, characters 8-11\n\
           "
      }
    ]
    ocamllsp finished with code = 0
    |}]
;;

let%expect_test "ocamllsp does not exit if only Shutdown notification is sent" =
  let run client () =
    let* (_ : InitializeResult.t) = Client.initialized client in
    Client.request client Shutdown
  in
  test run;
  [%expect
    {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    dropped notification
    Uncaught error when handling notification:
    {
      "params": {
        "message": "Unable to find 'ocamlformat-rpc' binary. Types on hover may not be well-formatted. You need to install either 'ocamlformat' of version > 0.21.0 or, otherwise, 'ocamlformat-rpc' package.",
        "type": 3
      },
      "method": "window/showMessage",
      "jsonrpc": "2.0"
    }
    Error:
    [ { exn =
          "(\"unexpected notification\",\n\
          \ { notification =\n\
          \     { params =\n\
          \         { message =\n\
          \             \"Unable to find 'ocamlformat-rpc' binary. Types on hover may not be well-formatted. You need to install either 'ocamlformat' of version > 0.21.0 or, otherwise, 'ocamlformat-rpc' package.\"\n\
          \         ; type = 3\n\
          \         }\n\
          \     ; method = \"window/showMessage\"\n\
          \     ; jsonrpc = \"2.0\"\n\
          \     }\n\
          \ })"
      ; backtrace =
          "Raised at Stdune__Code_error.raise in file \"otherlibs/stdune/src/code_error.ml\", line 10, characters 30-62\n\
           Called from Lsp_fiber__Rpc.Client.h_on_notification in file \"lsp-fiber/src/rpc.ml\", line 362, characters 17-46\n\
           Called from Fiber__Scheduler.exec in file \"fiber/src/scheduler.ml\", line 73, characters 8-11\n\
           "
      }
    ]
    ocamllsp killed with signal = -7
    |}]
;;

let%expect_test
    "ocamllsp process exits with code 0 after Exit notification is sent (should be 1)"
  =
  let run client () =
    let* (_ : InitializeResult.t) = Client.initialized client in
    Client.notification client Exit
  in
  test run;
  [%expect
    {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    dropped notification
    Uncaught error when handling notification:
    {
      "params": {
        "message": "Unable to find 'ocamlformat-rpc' binary. Types on hover may not be well-formatted. You need to install either 'ocamlformat' of version > 0.21.0 or, otherwise, 'ocamlformat-rpc' package.",
        "type": 3
      },
      "method": "window/showMessage",
      "jsonrpc": "2.0"
    }
    Error:
    [ { exn =
          "(\"unexpected notification\",\n\
          \ { notification =\n\
          \     { params =\n\
          \         { message =\n\
          \             \"Unable to find 'ocamlformat-rpc' binary. Types on hover may not be well-formatted. You need to install either 'ocamlformat' of version > 0.21.0 or, otherwise, 'ocamlformat-rpc' package.\"\n\
          \         ; type = 3\n\
          \         }\n\
          \     ; method = \"window/showMessage\"\n\
          \     ; jsonrpc = \"2.0\"\n\
          \     }\n\
          \ })"
      ; backtrace =
          "Raised at Stdune__Code_error.raise in file \"otherlibs/stdune/src/code_error.ml\", line 10, characters 30-62\n\
           Called from Lsp_fiber__Rpc.Client.h_on_notification in file \"lsp-fiber/src/rpc.ml\", line 362, characters 17-46\n\
           Called from Fiber__Scheduler.exec in file \"fiber/src/scheduler.ml\", line 73, characters 8-11\n\
           "
      }
    ]
    ocamllsp finished with code = 0
    |}]
;;
