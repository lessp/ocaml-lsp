open Test.Import

let%expect_test "completion for variant in labeled argument without underscore" =
  let source =
    {ocaml|module V = struct
  type t = One | Two | Three
end

let make ~(v : V.t) =
  match v with
  | V.One -> "one"
  | V.Two -> "two"
  | V.Three -> "three"

let test = make ~v:|ocaml}
  in
  let position = Position.create ~line:10 ~character:18 in
  Completion.print_completions source position;
  [%expect
    {|
    DEBUG: prefix='~v'
      char_before_cursor='v' (offset=169)
      can_be_hole=false
    DEBUG Complete_by_prefix.complete:
      context: Unknown
      entries count: 0
    No completions
    |}]
;;

let%expect_test "completion for variant in labeled argument with space after colon" =
  let source =
    {ocaml|module V = struct
  type t = One | Two | Three
end

let make ~(v : V.t) =
  match v with
  | V.One -> "one"
  | V.Two -> "two"
  | V.Three -> "three"

let test = make ~v: |ocaml}
  in
  let position = Position.create ~line:10 ~character:19 in
  Completion.print_completions source position;
  [%expect
    {|
    DEBUG: prefix=':'
      char_before_cursor=':' (offset=170)
      can_be_hole=false
    DEBUG Complete_by_prefix.complete:
      context: Application { argument_type=V.t, labels=[] }
      entries count: 2
    Completions:
    {
      "filterText": "_One",
      "kind": 1,
      "label": "One",
      "sortText": "0000",
      "textEdit": {
        "newText": "One",
        "range": {
          "end": { "character": 19, "line": 10 },
          "start": { "character": 19, "line": 10 }
        }
      }
    }
    {
      "filterText": "_Two",
      "kind": 1,
      "label": "Two",
      "sortText": "0001",
      "textEdit": {
        "newText": "Two",
        "range": {
          "end": { "character": 19, "line": 10 },
          "start": { "character": 19, "line": 10 }
        }
      }
    }
    {
      "filterText": "_Three",
      "kind": 1,
      "label": "Three",
      "sortText": "0002",
      "textEdit": {
        "newText": "Three",
        "range": {
          "end": { "character": 19, "line": 10 },
          "start": { "character": 19, "line": 10 }
        }
      }
    }
    {
      "detail": "'b * 'b list -> 'b list",
      "kind": 4,
      "label": "::",
      "sortText": "0000",
      "textEdit": {
        "newText": "::",
        "range": {
          "end": { "character": 19, "line": 10 },
          "start": { "character": 18, "line": 10 }
        }
      }
    }
    {
      "detail": "'a ref -> 'a -> unit",
      "kind": 12,
      "label": ":=",
      "sortText": "0001",
      "textEdit": {
        "newText": ":=",
        "range": {
          "end": { "character": 19, "line": 10 },
          "start": { "character": 18, "line": 10 }
        }
      }
    }
    |}]
;;

let%expect_test "completion for variant with typed hole after space" =
  let source =
    {ocaml|module V = struct
  type t = One | Two | Three
end

let make ~(v : V.t) =
  match v with
  | V.One -> "one"
  | V.Two -> "two"
  | V.Three -> "three"

let test = make ~v: _|ocaml}
  in
  let position = Position.create ~line:10 ~character:20 in
  Completion.print_completions source position;
  [%expect
    {|
    DEBUG: prefix=''
      char_before_cursor=' ' (offset=171)
      can_be_hole=false
    DEBUG Complete_by_prefix.complete:
      context: Application { argument_type=V.t, labels=[~v] }
      entries count: 327
    Completions:
    {
      "filterText": "_One",
      "kind": 1,
      "label": "One",
      "sortText": "0000",
      "textEdit": {
        "newText": "One",
        "range": {
          "end": { "character": 21, "line": 10 },
          "start": { "character": 20, "line": 10 }
        }
      }
    }
    {
      "filterText": "_Two",
      "kind": 1,
      "label": "Two",
      "sortText": "0001",
      "textEdit": {
        "newText": "Two",
        "range": {
          "end": { "character": 21, "line": 10 },
          "start": { "character": 20, "line": 10 }
        }
      }
    }
    {
      "filterText": "_Three",
      "kind": 1,
      "label": "Three",
      "sortText": "0002",
      "textEdit": {
        "newText": "Three",
        "range": {
          "end": { "character": 21, "line": 10 },
          "start": { "character": 20, "line": 10 }
        }
      }
    }
    {
      "kind": 14,
      "label": "in",
      "textEdit": {
        "newText": "in",
        "range": {
          "end": { "character": 20, "line": 10 },
          "start": { "character": 20, "line": 10 }
        }
      }
    }
    {
      "detail": "V.t",
      "kind": 4,
      "label": "One",
      "sortText": "0000",
      "textEdit": {
        "newText": "One",
        "range": {
          "end": { "character": 20, "line": 10 },
          "start": { "character": 20, "line": 10 }
        }
      }
    }
    {
      "detail": "V.t",
      "kind": 4,
      "label": "Three",
      "sortText": "0001",
      "textEdit": {
        "newText": "Three",
        "range": {
          "end": { "character": 20, "line": 10 },
          "start": { "character": 20, "line": 10 }
        }
      }
    }
    {
      "detail": "V.t",
      "kind": 4,
      "label": "Two",
      "sortText": "0002",
      "textEdit": {
        "newText": "Two",
        "range": {
          "end": { "character": 20, "line": 10 },
          "start": { "character": 20, "line": 10 }
        }
      }
    }
    {
      "detail": "'a ref -> 'a",
      "kind": 12,
      "label": "!",
      "sortText": "0003",
      "textEdit": {
        "newText": "!",
        "range": {
          "end": { "character": 20, "line": 10 },
          "start": { "character": 20, "line": 10 }
        }
      }
    }
    {
      "detail": "int -> 'a",
      "kind": 12,
      "label": "exit",
      "sortText": "0004",
      "textEdit": {
        "newText": "exit",
        "range": {
          "end": { "character": 20, "line": 10 },
          "start": { "character": 20, "line": 10 }
        }
      }
    }
    {
      "detail": "string -> 'a",
      "kind": 12,
      "label": "failwith",
      "sortText": "0005",
      "textEdit": {
        "newText": "failwith",
        "range": {
          "end": { "character": 20, "line": 10 },
          "start": { "character": 20, "line": 10 }
        }
      }
    }
    .............
    |}]
;;
