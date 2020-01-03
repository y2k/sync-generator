open System

[<CompilerMessage("Incomplete hole", 130)>]
let TODO() = failwith "not implemented"

module Diff =

    let mapDiff (ma : Map<'k, 'v>) (mb : Map<'k, 'v>) : Map<'k, 'v> * Set<'k> =
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
            let (pc, pd) = mapDiff a.posts b.posts
            let (uc, ud) = mapDiff a.userTags b.userTags
            let (tc, td) = mapDiff a.topTags b.topTags
            { posts_changed = pc; posts_deleted = pd; 
              userTags_changed = uc; userTags_deleted = ud; 
              topTags_changed = tc; topTags_deleted = td }
        let main _ =
            TODO()

module Serializer =
    type C<'a, 'k> = 
        { ser : 'a -> byte []
          deser : byte [] -> int -> 'k -> 'a * int }

    type KeyValue =
        | OfBase of byte [] * parserId : Guid
        | OfInt of key : int * value : KeyValue * parserId : Guid
        | OfString of key : string * value : KeyValue * parserId : Guid
        | OfCollection of value : KeyValue []
        | OfMap of value : Map<string, KeyValue>
    let IntC : C<int, _> =
        { ser = fun x -> BitConverter.GetBytes(x)
          deser = fun x o _ -> BitConverter.ToInt32(x, o), 4 }
    let StringC : C<string, _> = 
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
    let commonC (keyC : C<'k, _>) (valueC : C<'b, 'k>) (objModule : ObjModule<'a, 'b, 'k>) : C<'a, _> = 
        { ser = fun m ->
            objModule.toList m
            |> List.collect (fun (i, x) -> [ keyC.ser i; valueC.ser x ])
            |> Array.concat
            |> fun xs -> Array.concat [ BitConverter.GetBytes(objModule.toList m |> List.length) ; xs ]
          deser = fun bs o _ ->
            let count = BitConverter.ToInt32(bs, o)
            List.init count ignore
            |> List.fold 
                (fun (m, l) _ -> 
                    let (k, l1) = keyC.deser bs (o + l) ()
                    let (v, l2) = valueC.deser bs (o + l + l1) k
                    objModule.add k v m, l + l1 + l2)
                (objModule.empty, 4) }
    let MapC (keyC : C<'k, _>) (vc : C<'v, _>) : C<Map<'k, 'v>, _> =
        let objModule : ObjModule<Map<'k, 'v>, 'v, 'k> = 
            { toList = fun m -> Map.toList m
              add = fun k v m -> Map.add k v m
              empty = Map.empty }
        commonC keyC vc objModule

module Example =
    open Serializer

    type Post = { id : int; title: string; comments: int }
    type Tag = Tag
    type LocalDb =
        { posts : Map<int, Post>
          userTags : Map<string, Tag>
          topTags : Map<string, Tag> }
    let TagC : C<Tag, _> = { ser = (fun _ -> [||]); deser = (fun _ _ _ -> Tag, 0) }

    type SimpleTypes = IntType of int | StringType of string

    let PostC : C<Post, _> =
        let a : C<SimpleTypes, _> = 
            { ser = function
                    | IntType x -> IntC.ser x
                    | StringType x -> StringC.ser x
              deser = fun bs o k -> 
                  match k with
                  | 1 -> IntC.deser bs o () |> fun (x, l) -> IntType x, l
                  | 2 -> StringC.deser bs o () |> fun (x, l) -> StringType x, l
                  | 3 -> IntC.deser bs o () |> fun (x, l) -> IntType x, l
                  | x -> failwithf "illegal tag (%i)" x }
        let b : ObjModule<Post, SimpleTypes, int> = 
            { toList = fun x -> [ 1, IntType x.id; 2, StringType x.title; 3, IntType x.comments ]
              add = fun k f x -> 
                match k, f with
                | 1, IntType id -> { x with id = id }
                | 2, StringType title -> { x with title = title }
                | 3, IntType comments -> { x with comments = comments }
                | x -> failwithf "illegal tag (%O)" x
              empty = { id = 0; title = ""; comments = 0 } }
        commonC IntC a b

    type LocalDbFields = PostsField of Map<int, Post> | UserTagsField of Map<string, Tag> | TopTagsField of Map<string, Tag>
    let LocalDbC : C<LocalDb, unit> = 
        let a : C<LocalDbFields, _> =
            { ser = 
                function
                | PostsField x -> (MapC IntC PostC).ser x
                | UserTagsField x -> (MapC StringC TagC).ser x
                | TopTagsField x -> (MapC StringC TagC).ser x
              deser = fun bs o key ->
                match key with
                | 1 -> (MapC IntC PostC).deser bs o () |> fun (x, o) -> PostsField x, o
                | 2 -> (MapC StringC TagC).deser bs o () |> fun (x, o) -> UserTagsField x, o
                | 3 -> (MapC StringC TagC).deser bs o () |> fun (x, o) -> TopTagsField x, o
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
        let (b, len) = LocalDbC.deser a 0 ()
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
    Example.main()
    0
