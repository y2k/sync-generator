open System

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
