open System

module Foo =
    type Diff = Change of byte [] * Guid | DiffAdd of byte [] | DiffRemove of byte []

    type 'a B = { diff : 'a -> 'a -> Diff ; id : Guid }

    type 'a C = { ser : 'a -> byte [] ; id : Guid }

    type KeyValue =
        | OfBase of byte [] * parserId : Guid
        | OfInt of key : int * value : KeyValue * parserId : Guid
        | OfString of key : string * value : KeyValue * parserId : Guid
        | OfCollection of value : KeyValue []
        | OfMap of value : Map<string, KeyValue>

    let serializeDiff (a : KeyValue) (b : KeyValue) : byte [] =
        match (a, b) with
        | OfBase (a, id), OfBase (b, _) ->
            failwith "???"
        | OfCollection xa, OfCollection xb ->
            failwith "???"
        | OfMap ma, OfMap mb ->
            failwith "???"
        | _ -> failwith "???"

type A = 
    { name: string
      type_: string
      sync: string }
type Records = A list

module Generator =
    let prefix = "(* GENERATED *)\n\nnamespace Generated.Generated\n\ntype LocalDb =\n"

    let mkType records =
        records
        |> List.mapi (
              fun i x -> 
                  if i = 0 then sprintf "    { %s : %s\n" x.name x.type_
                  else if i = records.Length - 1 then sprintf "      %s : %s }\n" x.name x.type_
                  else sprintf "      %s : %s\n" x.name x.type_)
        |> List.fold (+) ""

    let generate (records : Records) = 
        [ prefix; mkType records ]
        |> List.fold (+) ""

module Parser =
  open Legivel.Serialization

  let parse file =
      IO.File.ReadAllText file
      |> Deserialize<Records>
      |> function
         | [ Success { Data = x } ] -> x
         | _ -> failwith "???"

[<EntryPoint>]
let main _ =
    Parser.parse "example.yaml"
    |> Generator.generate
    |> fun x -> IO.File.WriteAllText("result.gen.fsx", x)
    0
