module App

open System

[<AutoOpen>]
module Predule =
    let inline flip f a b = f b a
    [<CompilerMessage("Incomplete hole", 130)>]
    let TODO() = failwith "not implemented"
    let never() = failwith "not implemented"
    let inline (^) f x = f x
    let (|Match|_|) (pat : string) (inp : string) =
        let m = System.Text.RegularExpressions.Regex.Match(inp, pat) in
        if m.Success
        then Some (List.tail [ for g in m.Groups -> g.Value ])
        else None
    let inline length x = (^x :(member Length: int) x)

module Diff =
    let diffMapsAdd (ma : Map<'k, 'v>) (mb : Map<'k, 'v>) : Map<'k, 'v>=
        let added = Map.filter (fun k _ -> Map.containsKey k ma |> not) mb
        let changed =
            Map.toSeq ma
            |> Seq.choose (fun (k, v) -> if Map.containsKey k mb && Map.find k mb <> v then Some (k, v) else None)
            |> Map.ofSeq
        Map.fold (fun acc k v -> Map.add k v acc) added changed
    let diffMapsRemove (ma : Map<'k, 'v>) (mb : Map<'k, 'v>) : Set<'k> =
        Map.toSeq ma |> Seq.choose (fun (k, _) -> if Map.containsKey k mb then None else Some k) |> Set.ofSeq
    let diffMaps (ma : Map<'k, 'v>) (mb : Map<'k, 'v>) : Map<'k, 'v> * Set<'k> =
        let added = Map.filter (fun k _ -> Map.containsKey k ma |> not) mb
        let removed = Map.toSeq ma |> Seq.choose (fun (k, _) -> if Map.containsKey k mb then None else Some k) |> Set.ofSeq
        let changed =
            Map.toSeq ma
            |> Seq.choose (fun (k, v) -> if Map.containsKey k mb && Map.find k mb <> v then Some (k, v) else None)
            |> Map.ofSeq
        Map.fold (fun acc k v -> Map.add k v acc) added changed, removed
    let applyDiff m changed deleted =
        let m = deleted |> Set.fold (fun acc k -> Map.remove k acc) m
        Map.fold (fun acc k v -> Map.add k v acc) m changed

    module Example =
        type Post = Post
        type Tag = Tag
        type LocalDb =
            { posts : Map<int, Post>
              userTags : Map<string, Tag>
              topTags : Map<string, Tag> }
        type LocalDbDiff =
            { posts_changed : Map<int, Post>
              posts_deleted : Set<int>
              userTags_changed : Map<string, Tag>
              userTags_deleted : Set<string>
              topTags_changed : Map<string, Tag>
              topTags_deleted : Set<string> }
        let diff a b =
            let (pc, pd) = diffMaps a.posts b.posts
            let (uc, ud) = diffMaps a.userTags b.userTags
            let (tc, td) = diffMaps a.topTags b.topTags
            { posts_changed = pc; posts_deleted = pd; 
              userTags_changed = uc; userTags_deleted = ud; 
              topTags_changed = tc; topTags_deleted = td }
        let apply a diff =
            { posts = applyDiff a.posts diff.posts_changed diff.posts_deleted
              userTags = applyDiff a.userTags diff.userTags_changed diff.userTags_deleted
              topTags = applyDiff a.topTags diff.topTags_changed diff.topTags_deleted }
        let main _ =
            let origin =
                { posts = [ 999, Post; 42, Post ] |> Map.ofList
                  userTags = [ "comix", Tag ] |> Map.ofList
                  topTags = [ "ecchi", Tag; "it", Tag ] |> Map.ofList }
            let changed =
                { posts = [ 999, Post ] |> Map.ofList
                  userTags = [ "anime", Tag; "comix", Tag ] |> Map.ofList
                  topTags = [ "ecchi", Tag; "it", Tag ] |> Map.ofList }
            let diff = diff origin changed
            printfn "Diff = %O" diff

            let actual = apply origin diff
            if actual = changed 
                then printfn "Update success"
                else failwithf "Assert failed\nExpected = %O\nActual = %O" origin changed

module Serializer =
    type Buffer = { bytes : byte []; offset : int }
    type C<'a> = 
        { ser : 'a -> byte []
          deser : Buffer -> int -> 'a * int }
    type KeyValue =
        | OfBase of byte [] * parserId : Guid
        | OfInt of key : int * value : KeyValue * parserId : Guid
        | OfString of key : string * value : KeyValue * parserId : Guid
        | OfCollection of value : KeyValue []
        | OfMap of value : Map<string, KeyValue>
    let UnitC : C<unit> =
        { ser = fun _ -> [||]
          deser = fun _ _ -> (), 0 }
    let IntC : C<int> =
        { ser = fun x -> BitConverter.GetBytes(x)
          deser = fun b _ -> BitConverter.ToInt32(b.bytes, b.offset), 4 }
    let StringC : C<string> = 
        { ser = (fun x -> 
            let bs = Text.Encoding.UTF8.GetBytes(x)
            Array.concat [ BitConverter.GetBytes(length bs) ; bs ])
          deser = fun b _ ->
            let len = BitConverter.ToInt32(b.bytes, b.offset)
            Text.Encoding.UTF8.GetString(b.bytes, b.offset + 4, len), len + 4 }
    type ObjModule<'a, 'b, 'k> =
        { toList : 'a -> ('k * 'b) list
          add : 'k -> 'b -> 'a -> 'a
          empty : 'a }
    let commonC (keyC : C<'k>) (valueC : C<'b>) (objModule : ObjModule<'a, 'b, 'k>) : C<'a> = 
        { ser = fun m ->
            objModule.toList m
            |> List.collect (fun (i, x) -> [ keyC.ser i; valueC.ser x ])
            |> Array.concat
            |> fun xs -> Array.concat [ BitConverter.GetBytes(length <| objModule.toList m) ; xs ]
          deser = fun b _ ->
            let count = BitConverter.ToInt32(b.bytes, b.offset)
            List.init count ignore
            |> List.fold 
                (fun (m, l, i) _ -> 
                    let (k, l1) = keyC.deser { b with offset = b.offset + l } 0
                    let (v, l2) = valueC.deser { b with offset = b.offset + l + l1 } i
                    objModule.add k v m, l + l1 + l2, i + 1)
                (objModule.empty, 4, 1) 
            |> fun (a, b, _) -> a, b }
    let MapC (keyC : C<'k>) (vc : C<'v>) : C<Map<'k, 'v>> =
        let objModule : ObjModule<Map<'k, 'v>, 'v, 'k> = 
            { toList = fun m -> Map.toList m
              add = fun k v m -> Map.add k v m
              empty = Map.empty }
        commonC keyC vc objModule
    let SetC (keyC : C<'k>) : C<Set<'k>> =
        let objModule : ObjModule<Set<'k>, unit, 'k> = 
            { toList = fun m -> Set.toList m |> List.map (fun x -> x, ())
              add = fun k _ m -> Set.add k m
              empty = Set.empty }
        commonC keyC UnitC objModule
    let OptionC (valueC : C<'v>) : C<Option<'v>> =
        let objModule : ObjModule<Option<'v>, _, _> = 
            { toList = fun m -> m |> Option.map (fun x -> [(), x]) |> Option.defaultValue []
              add = fun _ x _ -> Some x
              empty = None }
        commonC UnitC valueC objModule

    type Field'<'field, 'instance> = 
        { deserSet : 'instance -> Buffer -> 'instance * int
          getSer : 'instance -> byte [] }
    let recordC (fields : Field'<_, _> list) empty : C<_> =
        { ser = fun m ->
            fields
            |> List.map ^ fun f -> f.getSer m
            |> Array.concat
            |> fun xs -> Array.concat [ BitConverter.GetBytes(fields.Length) ; xs ]
          deser = fun b _ ->
            let count = BitConverter.ToInt32(b.bytes, b.offset)
            List.init count ignore
            |> List.fold 
                (fun (m, l, i) _ ->
                    let deserSet = (fields.[i - 1]).deserSet
                    let (v, l2) = deserSet m { b with offset = b.offset + l }
                    v, l + l2, i + 1)
                (empty, 4, 1) 
            |> fun (a, b, _) -> a, b }

    module Example =
        type Post = { id : int; title: string; comments: int }
        type Tag = Tag
        type LocalDb =
            { posts : Map<int, Post>
              userTags : Map<string, Tag>
              topTags : Map<string, Tag>
              number1 : int
              string1 : string }
        let TagC : C<Tag> = { ser = (fun _ -> [||]); deser = (fun _ _ -> Tag, 0) }

        let PostC : C<Post> =
            recordC
                [ { getSer = fun x -> IntC.ser x.id
                    deserSet = fun x bs -> IntC.deser bs 0 |> fun (f, l) -> { x with id = f }, l }
                  { getSer = fun x -> StringC.ser x.title
                    deserSet = fun x bs -> StringC.deser bs 0 |> fun (f, l) -> { x with title = f }, l }
                  { getSer = fun x -> IntC.ser x.comments
                    deserSet = fun x bs -> IntC.deser bs 0 |> fun (f, l) -> { x with comments = f }, l } ] 
                { id = 0; title = ""; comments = 0 }

        let LocalDbC : C<LocalDb> =
            recordC
                [ { getSer = fun x -> (MapC IntC PostC).ser x.posts
                    deserSet = fun x bs -> (MapC IntC PostC).deser bs 0 |> fun (f, l) -> { x with posts = f }, l }
                  { getSer = fun x -> (MapC StringC TagC).ser x.userTags
                    deserSet = fun x bs -> (MapC StringC TagC).deser bs 0 |> fun (f, l) -> { x with userTags = f }, l }
                  { getSer = fun x -> (MapC StringC TagC).ser x.topTags
                    deserSet = fun x bs -> (MapC StringC TagC).deser bs 0 |> fun (f, l) -> { x with topTags = f }, l }
                  { getSer = fun x -> IntC.ser x.number1
                    deserSet = fun x bs -> IntC.deser bs 0 |> fun (f, l) -> { x with number1 = f }, l }
                  { getSer = fun x -> StringC.ser x.string1
                    deserSet = fun x bs -> StringC.deser bs 0 |> fun (f, l) -> { x with string1 = f }, l } ]
                { posts = Map.empty ; userTags = Map.empty; topTags = Map.empty; number1 = 0; string1 = "" }

        let main () =
            let db = 
              { posts = 
                  [ 999, { id = 999; title = "hello"; comments = 13 }
                    42, { id = 42; title = "world"; comments = -100 } ] |> Map.ofList
                userTags = [ "anime", Tag; "comix", Tag ] |> Map.ofList
                topTags = [ "ecchi", Tag; "it", Tag ] |> Map.ofList
                number1 = 2020
                string1 = "1990" }
            let a = LocalDbC.ser db
            printfn "%A" a
            let (b, len) = LocalDbC.deser { bytes = a; offset = 0 } 0
            printfn "%O (%i)" b len
            if db <> b then failwith "Deserialized result not equals to origin"

type A = 
    { name: string
      type_: string
      sync: string }
type Records = A list

module Generator =
    let prefix = "(* GENERATED *)\n\nnamespace Generated.Generated\n\ntype Post = Post\ntype Tag = Tag\n\ntype LocalDb =\n"

    let mkType records =
        records
        |> List.mapi ^ fun i x -> 
              if i = 0 then sprintf "    { %s : %s\n" x.name x.type_
              else if i = records.Length - 1 then sprintf "      %s : %s }\n" x.name x.type_
              else sprintf "      %s : %s\n" x.name x.type_
        |> List.fold (+) ""
        |> flip (+) "\n"

    let mkDiffType (records : Records) =
        let getMapKeyType = function Match "Map<(.+?), .+?>" [x] -> x | e -> failwithf "%O" e
        records
        |> List.indexed
        |> List.collect ^ fun (i, r) -> 
            [ (if i = 0 
                  then sprintf "    { %s_changed : %s\n" r.name r.type_
                  else sprintf "      %s_changed : %s\n" r.name r.type_)
              (if i = length records - 1
                  then sprintf "      %s_removed : Set<%s> }\n" r.name (getMapKeyType r.type_)
                  else sprintf "      %s_removed : Set<%s>\n" r.name (getMapKeyType r.type_)) ]
        |> List.fold (+) "type LocalDbDiff =\n"
        |> flip (+) "\n"

    let generate (records : Records) = 
        [ prefix
          mkType records
          mkDiffType records ]
        |> List.fold (+) ""

    module GneratedExample =
        type Post = Post
        type Tag = Tag

        type LocalDb =
            { posts : Map<int, Post>
              userTags : Map<string, Tag>
              topTags : Map<string, Tag> }

        type LocalDbDiff =
            { posts_changed : Map<int, Post>
              posts_removed : Set<int>
              userTags_changed : Map<string, Tag>
              userTags_removed : Set<string>
              topTags_changed : Map<string, Tag>
              topTags_removed : Set<string> }

        let main _ =
            let PostC : Post Serializer.C = never()
            let TagC : Tag Serializer.C = never()

            let LocalDbC : LocalDbDiff Serializer.C =
                Serializer.recordC
                    [ { getSer = fun x -> (Serializer.MapC Serializer.IntC PostC).ser x.posts_changed
                        deserSet = fun x bs -> (Serializer.MapC Serializer.IntC PostC).deser bs 0 |> fun (f, l) -> { x with posts_changed = f }, l }
                      { getSer = fun x -> (Serializer.SetC Serializer.IntC).ser x.posts_removed
                        deserSet = fun x bs -> (Serializer.SetC Serializer.IntC).deser bs 0 |> fun (f, l) -> { x with posts_removed = f }, l } ]
                    { posts_changed = Map.empty
                      posts_removed = Set.empty
                      userTags_changed = Map.empty
                      userTags_removed = Set.empty
                      topTags_changed = Map.empty
                      topTags_removed = Set.empty }

            let serializeDiff (a : LocalDb) (b : LocalDb) : byte [] =
                { posts_changed = Diff.diffMapsAdd a.posts b.posts
                  posts_removed = Diff.diffMapsRemove a.posts b.posts
                  userTags_changed = Diff.diffMapsAdd a.userTags b.userTags
                  userTags_removed = Diff.diffMapsRemove a.userTags b.userTags
                  topTags_changed = Diff.diffMapsAdd a.topTags b.topTags
                  topTags_removed = Diff.diffMapsRemove a.topTags b.topTags }
                |> LocalDbC.ser

            let applyDiff (a : LocalDb) (bytes : byte[]) : LocalDb =
                let (df, _) = LocalDbC.deser { bytes = bytes; offset = 0 } 0
                { a with 
                    posts = Diff.applyDiff a.posts df.posts_changed df.posts_removed
                    userTags = Diff.applyDiff a.userTags df.userTags_changed df.userTags_removed
                    topTags = Diff.applyDiff a.topTags df.topTags_changed df.topTags_removed }
            ()

module Parser =
  open Legivel.Serialization

  let parse file =
      IO.File.ReadAllText file
      |> Deserialize<Records>
      |> function
         | [ Success { Data = x } ] -> x
         | e -> failwithf "illegal state (%O)" e

  let main () =
      parse "example.yaml"
      |> Generator.generate
      |> fun x -> IO.File.WriteAllText("result.gen.fsx", x)

[<EntryPoint>]
let main _ =
    Parser.main()
    // Serializer.Example.main()
    // Diff.Example.main()
    0
