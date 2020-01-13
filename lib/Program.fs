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
            |> Seq.choose (fun (k, v) -> if Map.containsKey k mb && Map.find k mb <> v then Some (k, Map.find k mb) else None)
            |> Map.ofSeq
        Map.fold (fun acc k v -> Map.add k v acc) added changed
    let diffMapsRemove (ma : Map<'k, 'v>) (mb : Map<'k, 'v>) : Set<'k> =
        Map.toSeq ma |> Seq.choose (fun (k, _) -> if Map.containsKey k mb then None else Some k) |> Set.ofSeq
    let diffMaps (ma : Map<'k, 'v>) (mb : Map<'k, 'v>) : Map<'k, 'v> * Set<'k> =
        let added = Map.filter (fun k _ -> Map.containsKey k ma |> not) mb
        let removed = Map.toSeq ma |> Seq.choose (fun (k, _) -> if Map.containsKey k mb then None else Some k) |> Set.ofSeq
        let changed =
            Map.toSeq ma
            |> Seq.choose (fun (k, v) -> if Map.containsKey k mb && Map.find k mb <> v then Some (k, Map.find k mb) else None)
            |> Map.ofSeq
        Map.fold (fun acc k v -> Map.add k v acc) added changed, removed
    let applyDiff m changed deleted =
        let m = deleted |> Set.fold (fun acc k -> Map.remove k acc) m
        Map.fold (fun acc k v -> Map.add k v acc) m changed

module Serializer =
    type Buffer = { bytes : byte []; offset : int }
    type C<'a> = 
        { ser : 'a -> byte []
          deser : Buffer -> 'a * int }
    let UnitC : C<unit> =
        { ser = fun _ -> [||]
          deser = fun _ -> (), 0 }
    let IntC : C<int> =
        { ser = fun x -> BitConverter.GetBytes(x)
          deser = fun b -> BitConverter.ToInt32(b.bytes, b.offset), 4 }
    let StringC : C<string> = 
        { ser = (fun x -> 
            let bs = Text.Encoding.UTF8.GetBytes(x)
            Array.concat [ BitConverter.GetBytes(length bs) ; bs ])
          deser = fun b ->
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
          deser = fun b ->
            let count = BitConverter.ToInt32(b.bytes, b.offset)
            List.init count ignore
            |> List.fold 
                (fun (m, l, i) _ -> 
                    let (k, l1) = keyC.deser { b with offset = b.offset + l }
                    let (v, l2) = valueC.deser { b with offset = b.offset + l + l1 }
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

    type FieldSer<'a> = 
        { getSer : 'a -> byte [] 
          deserSet : 'a -> Buffer -> 'a * int }
    let recordC (fields : FieldSer<_> list) empty : C<_> =
        { ser = fun m ->
            fields
            |> List.map ^ fun f -> f.getSer m
            |> Array.concat
          deser = fun b ->
            fields
            |> List.fold 
                (fun (m, l) ff -> 
                    let deserSet = ff.deserSet
                    let (v, l2) = deserSet m { b with offset = b.offset + l }
                    v, l + l2) 
                (empty, 0) }

    let record3C (a : C<_>) f1 (b : C<_>) f2 (c : C<_>) f3 g : C<'v> =
        { ser = fun m -> 
            Array.concat [f1 m |> a.ser; f2 m |> b.ser; f3 m |> c.ser ]
          deser = fun bs ->
            let (a, l1) = bs |> a.deser
            let (b, l2) = { bs with offset = bs.offset + l1 } |> b.deser
            let (c, l3) = { bs with offset = bs.offset + l2 + l1 } |> c.deser
            g a b c, l3 + l2 + l1 }
    let record5C (c1 : C<_>) f1 (c2 : C<_>) f2 (c3 : C<_>) f3 c4 f4 c5 f5 g : C<'v> =
        { ser = fun m -> 
            Array.concat [f1 m |> c1.ser; f2 m |> c2.ser; f3 m |> c3.ser; f4 m |> c4.ser; f5 m |> c5.ser ]
          deser = fun bs ->
            let (a, l1) = bs |> c1.deser
            let (b, l2) = { bs with offset = bs.offset + l1 } |> c2.deser
            let (c, l3) = { bs with offset = bs.offset + l2 + l1 } |> c3.deser
            let (d, l4) = { bs with offset = bs.offset + l3 + l2 + l1 } |> c4.deser
            let (e, l5) = { bs with offset = bs.offset + l4 + l3 + l2 + l1 } |> c5.deser
            g a b c d e, l5 + l4 + l3 + l2 + l1 }

    let inline mkSer c g s =
        { getSer = fun x -> c.ser ^ g x
          deserSet = fun x bs -> c.deser bs |> fun (f, l) -> s x f, l }

#if !FABLE_COMPILER

module Examples =
    module DiffExample =
        open Diff
        type Post = { id: int; title: string; comments: int }
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
                { posts = [ 999, { id = 999; title = "title-999"; comments = 10 }; 42,  { id = 42; title = "title-42"; comments = 100 } ] |> Map.ofList
                  userTags = [ "comix", Tag ] |> Map.ofList
                  topTags = [ "ecchi", Tag; "it", Tag ] |> Map.ofList }
            let changed =
                { posts = [ 999, { id = 999; title = "title-999"; comments = 10 } ] |> Map.ofList
                  userTags = [ "anime", Tag; "comix", Tag ] |> Map.ofList
                  topTags = [ "ecchi", Tag; "it", Tag ] |> Map.ofList }
            let d = diff origin changed
            printfn "Diff = %O" diff

            let actual = apply origin d
            if actual = changed 
                then printfn "Update success"
                else failwithf "Assert failed\nExpected = %O\nActual = %O" origin changed

            let expected = { origin with posts = origin.posts |> Map.map (fun _ v -> { v with comments = v.comments + 1 }) }
            let d = diff origin expected
            let actual = d |> apply origin
            if actual = expected 
                then printfn "Update success"
                else failwithf "Assert failed\nExpected = %O\nActual = %O\nDiff = %O" expected actual d

    module SerializerExample =
        open Serializer
        type Profile = Profile
        let ProfileC : Profile C = recordC [] Profile
        type PostsWithLevels = PostsWithLevels
        let PostsWithLevelsC : PostsWithLevels C = recordC [] PostsWithLevels
        type Message = Message
        let MessageC : Message C = recordC [] Message
        type Source = Source
        let SourceC : Source C = recordC [] Source
        type PostResponse = PostResponse
        let PostResponseC : PostResponse C = recordC [] PostResponse
        type Post = { id : int; title: string; comments: int }
        type Tag = Tag
        type LocalDb =
            { posts : Map<int, Post>
              userTags : Map<string, Tag>
              topTags : Map<string, Tag>
              number1 : int
              string1 : string }
        let TagC : C<Tag> = { ser = (fun _ -> [||]); deser = (fun _ -> Tag, 0) }

        let PostC : C<Post> =
            record3C
                IntC (fun x -> x.id) 
                StringC (fun x -> x.title) 
                IntC (fun x -> x.comments)
                (fun a b c -> { id = a; title = b; comments = c})

        let LocalDbC : C<LocalDb> =
            record5C
                (MapC IntC PostC) (fun x -> x.posts)
                (MapC StringC TagC) (fun x -> x.userTags)
                (MapC StringC TagC) (fun x -> x.topTags)
                IntC (fun x -> x.number1)
                StringC (fun x -> x.string1)
                (fun a b c d e -> { posts = a; userTags = b; topTags = c; number1 = d; string1 = e })

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
            let (b, len) = LocalDbC.deser { bytes = a; offset = 0 }
            printfn "%O (%i)" b len
            if db <> b then failwith "Deserialized result not equals to origin"

open Legivel.Attributes

type A = 
    { name: string
      [<YamlField("type")>] type': string }
type Records = 
    { [<YamlField("module")>] module': string
      [<YamlField("open")>] open': string; items: A list }

module Generator =
    let mkHeader x = 
        [ "(* GENERATED *)\n"
          sprintf "module %s\n" x.module'
          sprintf "open %s" x.open' 
          "open App"
          "open App.Serializer\n" ]

    let mkType records =
        [ yield "type LocalDb =\n    {"
          yield! records.items |> List.map ^ fun x -> sprintf "      %s : %s" x.name x.type'
          yield "    }\n" ]

    let getMapKeyType = function Match "Map<(.+?), .+?>" [x] -> x | e -> failwithf "%O" e
    let getMapValueType = function Match "Map<.+?, (.+?)>" [x] -> x | e -> failwithf "%O" e
    let typeToCType =
        function 
        | "int" -> "IntC"
        | "string" -> "StringC"
        | "unit" -> "UnitC"
        | name -> sprintf "%sC" name

    let mkDiffType (records : Records) =
        [ yield "type LocalDbDiff =\n    {"
          yield!
            records.items
            |> List.collect ^ fun r -> 
                [ sprintf "      %s_changed : %s" r.name r.type'
                  sprintf "      %s_removed : Set<%s>" r.name (getMapKeyType r.type') ]
          yield "    }\n" ]

    let mkSerializeDiff (records : Records) =
        [ yield "let serializeDiff (a : LocalDb) (b : LocalDb) : byte [] =\n    {"
          yield!
            records.items
            |> List.collect ^ fun r ->
                [ sprintf "      %s_changed = Diff.diffMapsAdd a.%s b.%s" r.name r.name r.name
                  sprintf "      %s_removed = Diff.diffMapsRemove a.%s b.%s" r.name r.name r.name ]
          yield "    } |> LocalDbC.ser\n" ]

    let mkApplyDiff records =
        [ yield "let applyDiff (a : LocalDb) (bytes : byte[]) : LocalDb =\n    let (df, _) = LocalDbC.deser { bytes = bytes; offset = 0 }\n    { a with"
          yield!
            records.items
            |> List.map ^ fun r -> 
                sprintf "        %s = Diff.applyDiff a.%s df.%s_changed df.%s_removed" r.name r.name r.name r.name
          yield "    }\n"]

    let mkLocalDbC records =
        [ yield "let LocalDbC : LocalDbDiff C ="
          yield "    recordC"
          yield "        ["
          yield! 
            records.items
            |> List.collect ^ fun r ->
                let kt = getMapKeyType r.type' |> typeToCType
                [ sprintf "          mkSer (MapC %s %s) (fun x -> x.%s_changed) (fun x f -> { x with %s_changed = f })" kt (getMapValueType r.type' |> typeToCType) r.name r.name
                  sprintf "          mkSer (SetC %s) (fun x -> x.%s_removed) (fun x f -> { x with %s_removed = f })" kt r.name r.name ]
          yield "        ]"
          yield "        {"
          yield! 
            records.items
            |> List.collect ^ fun r ->
                [ sprintf "          %s_changed = Map.empty" r.name
                  sprintf "          %s_removed = Set.empty" r.name ]
          yield "        }\n" ]

    let generate (records : Records) = 
        [ mkHeader
          mkType
          mkDiffType
          mkLocalDbC
          mkSerializeDiff
          mkApplyDiff ]
        |> Seq.collect (fun f -> f records)
        |> Seq.fold (sprintf "%s\n%s") ""

    module GeneratedExample =
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

        open Serializer
        
        let main _ =
            let PostC : Post C = never()
            let TagC : Tag C = never()

            let LocalDbC : LocalDbDiff C =
                recordC
                    [ { getSer = fun x -> (MapC IntC PostC).ser x.posts_changed
                        deserSet = fun x bs -> (MapC IntC PostC).deser bs |> fun (f, l) -> { x with posts_changed = f }, l }
                      { getSer = fun x -> (SetC IntC).ser x.posts_removed
                        deserSet = fun x bs -> (SetC IntC).deser bs |> fun (f, l) -> { x with posts_removed = f }, l } ]
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
                let (df, _) = LocalDbC.deser { bytes = bytes; offset = 0 }
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

  let main path =
      parse path
      |> Generator.generate
      |> fun x -> IO.File.WriteAllText(IO.Path.Combine(IO.Path.GetDirectoryName(path), "Store.gen.fs"), x)

[<EntryPoint>]
let main args =
    Parser.main args.[0]
    // Examples.SerializerExample.main()
    // Examples.DiffExample.main()
    0

#endif
