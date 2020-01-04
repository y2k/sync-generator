open System

[<CompilerMessage("Incomplete hole", 130)>]
let TODO() = failwith "not implemented"

module Diff =
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
    type C<'a> = 
        { ser : 'a -> byte []
          deser : byte [] -> int -> int -> 'a * int }

    type KeyValue =
        | OfBase of byte [] * parserId : Guid
        | OfInt of key : int * value : KeyValue * parserId : Guid
        | OfString of key : string * value : KeyValue * parserId : Guid
        | OfCollection of value : KeyValue []
        | OfMap of value : Map<string, KeyValue>
    let UnitC : C<unit> =
        { ser = fun _ -> [||]
          deser = fun _ _ _ -> (), 0 }
    let IntC : C<int> =
        { ser = fun x -> BitConverter.GetBytes(x)
          deser = fun x o _ -> BitConverter.ToInt32(x, o), 4 }
    let StringC : C<string> = 
        { ser = (fun x -> 
            let bs = Text.Encoding.UTF8.GetBytes(x)
            Array.concat [ BitConverter.GetBytes(bs.Length) ; bs ])
          deser = fun bs o _ ->
            let len = BitConverter.ToInt32(bs, o)
            Text.Encoding.UTF8.GetString(bs, o + 4, len), len + 4 }
    type ObjModule<'a, 'b, 'k> =
        { toList : 'a -> ('k * 'b) list
          add : 'k -> 'b -> 'a -> 'a
          empty : 'a }
    let commonC (keyC : C<'k>) (valueC : C<'b>) (objModule : ObjModule<'a, 'b, 'k>) : C<'a> = 
        { ser = fun m ->
            objModule.toList m
            |> List.collect (fun (i, x) -> [ keyC.ser i; valueC.ser x ])
            |> Array.concat
            |> fun xs -> Array.concat [ BitConverter.GetBytes(objModule.toList m |> List.length) ; xs ]
          deser = fun bs o _ ->
            let count = BitConverter.ToInt32(bs, o)
            List.init count ignore
            |> List.fold 
                (fun (m, l, k_) _ -> 
                    let (k, l1) = keyC.deser bs (o + l) 0
                    let (v, l2) = valueC.deser bs (o + l + l1) k_ //k
                    objModule.add k v m, l + l1 + l2, k_ + 1)
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

    module Example =
        type Post = { id : int; title: string; comments: int }
        type Tag = Tag
        type LocalDb =
            { posts : Map<int, Post>
              userTags : Map<string, Tag>
              topTags : Map<string, Tag> }
        let TagC : C<Tag> = { ser = (fun _ -> [||]); deser = (fun _ _ _ -> Tag, 0) }

        type SimpleTypes = IntType of int | StringType of string
        let defSer = function IntType x -> IntC.ser x | StringType x -> StringC.ser x
        let mapTuple deser bs o f = deser bs o 0 |> fun (x, l) -> f x, l

        let PostC : C<Post> =
            let a : C<SimpleTypes> = 
                { ser = defSer
                  deser = fun bs o i -> 
                      match i with
                      | 1 -> mapTuple IntC.deser bs o IntType
                      | 2 -> mapTuple StringC.deser bs o StringType
                      | 3 -> mapTuple IntC.deser bs o IntType
                      | x -> failwithf "illegal tag (%i)" x }
            let b : ObjModule<Post, SimpleTypes, int> = 
                { toList = fun x -> 
                    [ 1, IntType x.id
                      2, StringType x.title
                      3, IntType x.comments ]
                  add = fun k f x -> 
                    match k, f with
                    | 1, IntType id -> { x with id = id }
                    | 2, StringType title -> { x with title = title }
                    | 3, IntType comments -> { x with comments = comments }
                    | x -> failwithf "illegal tag (%O)" x
                  empty = { id = 0; title = ""; comments = 0 } }
            commonC IntC a b

        (* ... *)
        type FieldTag = FieldTag of int
        type Field'<'fieldDesc, 'value> = 
            { f : FieldTag -> byte [] -> int -> 'fieldDesc * int
              g : 'value -> 'fieldDesc
              h : 'fieldDesc -> 'value -> 'value }
        (* ... *)
        type PostField' = 
            | IdType' of int * C<int>
            | TitleType' of string * C<string>
            | CommentsType' of int * C<int>
        let fields_ : Field'<PostField', Post> list = 
            [ { f = fun _ _ _ -> IdType' (TODO()), TODO()
                g = fun _ -> TODO()
                h = fun _ _ -> TODO() } 
              { f = fun _ _ _ -> TitleType' (TODO()), TODO()
                g = fun _ -> TODO()
                h = fun _ _ -> TODO() }
              { f = fun _ _ _ -> CommentsType' (TODO()), TODO()
                g = fun _ -> TODO()
                h = fun _ _ -> TODO() } ]
        let empty_ : Post = { id = 0; title = ""; comments = 0 }
        (* ... *)
        let PostC' : C<Post> =
            let a : C<PostField'> = 
                { ser = 
                    function 
                    | IdType' (x, c) -> c.ser x 
                    | TitleType' (x, c) -> c.ser x
                    | CommentsType' (x, c) -> c.ser x
                  deser = fun bs o i ->
                    let f = fields_.[i]
                    let (r, l) = f.f (FieldTag i) bs o
                    r, l }
            let b : ObjModule<Post, PostField', int> = 
                { toList = fun p -> fields_ |> List.mapi (fun i x -> i + 1, x.g p)
                  add = fun i f x -> 
                    let f' = fields_.[i]
                    f'.h f x
                  empty = empty_ }
            commonC IntC a b

        type LocalDbFields = PostsField of Map<int, Post> | UserTagsField of Map<string, Tag> | TopTagsField of Map<string, Tag>
        let LocalDbC : C<LocalDb> = 
            let a : C<LocalDbFields> =
                { ser = 
                    function
                    | PostsField x -> (MapC IntC PostC).ser x
                    | UserTagsField x -> (MapC StringC TagC).ser x
                    | TopTagsField x -> (MapC StringC TagC).ser x
                  deser = fun bs o i ->
                    match i with
                    | 1 -> (MapC IntC PostC).deser bs o 0 |> fun (x, o) -> PostsField x, o
                    | 2 -> (MapC StringC TagC).deser bs o 0 |> fun (x, o) -> UserTagsField x, o
                    | 3 -> (MapC StringC TagC).deser bs o 0 |> fun (x, o) -> TopTagsField x, o
                    | x -> failwithf "illegal tag (%i)" x }
            let b : ObjModule<LocalDb, LocalDbFields, int> = 
                { toList = fun x -> 
                    [ 1, PostsField x.posts
                      2, UserTagsField x.userTags
                      3, TopTagsField x.topTags ]
                  add = fun _ x db -> 
                    match x with
                    | PostsField x -> { db with posts = x }
                    | UserTagsField x -> { db with userTags = x }
                    | TopTagsField x -> { db with topTags = x }
                  empty = { posts = Map.empty ; userTags = Map.empty; topTags = Map.empty } }
            commonC IntC a b

        let main () =
            let db = 
              { posts = [ 999, { id = 999; title = "hello"; comments = 13 }; 42, { id = 42; title = "world"; comments = -100 } ] |> Map.ofList
                userTags = [ "anime", Tag; "comix", Tag ] |> Map.ofList
                topTags = [ "ecchi", Tag; "it", Tag ] |> Map.ofList }
            let a = LocalDbC.ser db
            printfn "%A" a
            let (b, len) = LocalDbC.deser a 0 0
            printfn "%O (%i)" b len
            if db <> b then failwith "Deserialized result not equals to origin"

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
         | e -> failwithf "illegal state (%O)" e

[<EntryPoint>]
let main _ =
    // Parser.parse "example.yaml"
    // |> Generator.generate
    // |> fun x -> IO.File.WriteAllText("result.gen.fsx", x)
    Serializer.Example.main()
    // Diff.Example.main()
    0
