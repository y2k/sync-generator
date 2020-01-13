module SyncGenerator.Lib

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
