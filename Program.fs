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
        { ser : 'field -> byte [] option
          deser : Buffer -> 'field * int
          set : 'instance -> 'field -> 'instance
          get : 'instance -> 'field }
    let recordC fields empty : C<_> =
        commonC
            IntC 
            { ser = fun x -> fields |> List.pick (fun f -> f.ser x)
              deser = fun b i -> (fields.[i - 1]).deser b }
            { toList = fun p -> fields |> List.mapi (fun i x -> i + 1, x.get p)
              add = fun i f x -> (fields.[i - 1]).set x f
              empty = empty }
    let recordC' fields empty : C<_> =
        { ser = fun m ->
            let ser x = List.pick (fun f -> f.ser x) fields
            fields 
            |> List.mapi (fun i x -> i + 1, x.get m)
            |> List.collect (fun (i, x) -> [ IntC.ser i; ser x ])
            |> Array.concat
            |> fun xs -> Array.concat [ BitConverter.GetBytes(fields.Length) ; xs ]
          deser = fun b _ ->
            let add i f x = (fields.[i - 1]).set x f
            let deser b i = (fields.[i - 1]).deser b
            let count = BitConverter.ToInt32(b.bytes, b.offset)
            List.init count ignore
            |> List.fold 
                (fun (m, l, i) _ ->
                    let (k, l1) = IntC.deser { b with offset = b.offset + l } 0
                    let (v, l2) = deser { b with offset = b.offset + l + l1 } i
                    add k v m, l + l1 + l2, i + 1)
                (empty, 4, 1) 
            |> fun (a, b, _) -> a, b }
    type SimpleTypeField = IntField of int | StringField of string

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
                [ { ser = function IntField x -> Some <| IntC.ser x | _ -> None
                    deser = fun bs -> IntC.deser bs 0 ||> fun a l -> IntField a, l
                    set = fun x -> function IntField f -> { x with id = f } | e -> failwithf "%O" e
                    get = fun x -> IntField x.id }
                  { ser = function StringField x -> Some <| StringC.ser x | _ -> None
                    deser = fun b -> StringC.deser b 0 ||> fun a l -> StringField a, l
                    set = fun x -> function StringField f -> { x with title = f } | e -> failwithf "%O" e
                    get = fun x -> StringField x.title }
                  { ser = function IntField x -> Some <| IntC.ser x | _ -> None
                    deser = fun b -> IntC.deser b 0 ||> fun a l -> IntField a, l
                    set = fun x -> function IntField f -> { x with comments = f } | e -> failwithf "%O" e
                    get = fun x -> IntField x.comments } ] 
                { id = 0; title = ""; comments = 0 }

        type LocalDbFields = 
            | PostsField of Map<int, Post> 
            | MapStringTagField of Map<string, Tag>
            | SimpleField of SimpleTypeField
        let LocalDbC : C<LocalDb> =
            recordC
                [ { ser = function PostsField x -> (MapC IntC PostC).ser x |> Some | _ -> None
                    deser = fun b -> (MapC IntC PostC).deser b 0 ||> fun a l -> PostsField a, l
                    set = fun x -> function PostsField f -> { x with posts = f } | _ -> failwith "???" 
                    get = fun x -> PostsField x.posts }
                  { ser = function MapStringTagField x -> (MapC StringC TagC).ser x |> Some | _ -> None
                    deser = fun b -> (MapC StringC TagC).deser b 0 ||> fun a l -> MapStringTagField a, l
                    set = fun x -> function MapStringTagField f -> { x with userTags = f } | _ -> failwith "???"
                    get = fun x -> MapStringTagField x.userTags }
                  { ser = function MapStringTagField x -> (MapC StringC TagC).ser x |> Some | _ -> None
                    deser = fun b -> (MapC StringC TagC).deser b 0 ||> fun a l -> MapStringTagField a, l
                    set = fun x -> function MapStringTagField f -> { x with topTags = f } | _ -> failwith "???"
                    get = fun x -> MapStringTagField x.topTags }
                  { ser = function SimpleField (IntField x) -> IntC.ser x |> Some | _ -> None
                    deser = fun b -> IntC.deser b 0 ||> fun a l -> SimpleField (IntField a), l
                    set = fun x -> function SimpleField (IntField f) -> { x with number1 = f } | _ -> failwith "???"
                    get = fun x -> SimpleField (IntField x.number1) }
                  { ser = function SimpleField (StringField x) -> StringC.ser x |> Some | _ -> None
                    deser = fun b -> StringC.deser b 0 ||> fun a l -> SimpleField (StringField a), l
                    set = fun x -> function SimpleField (StringField f) -> { x with string1 = f } | _ -> failwith "???"
                    get = fun x -> SimpleField (StringField x.string1) } ]
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

    let mkFieldType (records : Records) =
        let getMapKeyType = function Match "Map<(.+?), .+?>" [x] -> x | e -> failwithf "%O" e
        records
        |> List.indexed
        |> List.collect ^ fun (i, r) -> 
            [ sprintf "    | Field_%s_changed of %s\n" r.name r.type_
              sprintf "    | Field_%s_removed of Set<%s>\n" r.name (getMapKeyType r.type_) ]
        |> List.fold (+) "type LocalDbDiffField =\n"
        |> flip (+) "\n"

    let generate (records : Records) = 
        [ prefix
          mkType records
          mkDiffType records
          mkFieldType records ]
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

        type LocalDbDiffField =
            | Field_posts_changed of Map<int, Post>
            | Field_posts_removed of Set<int>
            | Field_userTags_changed of Map<string, Tag>
            | Field_userTags_removed of Set<string>
            | Field_topTags_changed of Map<string, Tag>
            | Field_topTags_removed of Set<string>

        let main _ =
            let PostC : Post Serializer.C = never()
            let TagC : Tag Serializer.C = never()

            let LocalDbC : LocalDbDiff Serializer.C =
                Serializer.recordC
                    [ { ser = function Field_posts_changed x -> (Serializer.MapC Serializer.IntC PostC).ser x |> Some | _ -> None
                        deser = fun b -> (Serializer.MapC Serializer.IntC PostC).deser b 0 ||> fun a l -> Field_posts_changed a, l
                        set = fun x -> function Field_posts_changed f -> { x with posts_changed = f } | e -> failwithf "%O" e
                        get = fun x -> Field_posts_changed x.posts_changed }
                      { ser = function Field_posts_removed x -> (Serializer.SetC Serializer.IntC).ser x |> Some | _ -> None
                        deser = fun b -> (Serializer.SetC Serializer.IntC).deser b 0 ||> fun a l -> Field_posts_removed a, l
                        set = fun x -> function Field_posts_removed f -> { x with posts_removed = f } | e -> failwithf "%O" e
                        get = fun x -> Field_posts_removed x.posts_removed } ]
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
