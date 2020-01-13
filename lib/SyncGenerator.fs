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
    let DoubleC : double C =
        { ser = fun x -> BitConverter.GetBytes(x)
          deser = fun b -> BitConverter.ToDouble(b.bytes, b.offset), 8 }
    let BoolC : bool C =
        { ser = fun x -> BitConverter.GetBytes(x)
          deser = fun b -> BitConverter.ToBoolean(b.bytes, b.offset), 1 }
    let IntC : C<int> =
        { ser = fun x -> BitConverter.GetBytes(x)
          deser = fun b -> BitConverter.ToInt32(b.bytes, b.offset), 4 }
    let Int64C : C<int64> =
        { ser = fun x -> BitConverter.GetBytes(x)
          deser = fun b -> BitConverter.ToInt64(b.bytes, b.offset), 8 }
    let StringC : C<string> = 
        { ser = (fun x -> 
            let bs = Text.Encoding.UTF8.GetBytes(x)
            Array.concat [ BitConverter.GetBytes(length bs) ; bs ])
          deser = fun b ->
            let len = BitConverter.ToInt32(b.bytes, b.offset)
            Text.Encoding.UTF8.GetString(b.bytes, b.offset + 4, len), len + 4 }
    let DateTimeC : DateTime C =
        { ser = fun x -> Int64C.ser x.Ticks
          deser = fun bs -> Int64C.deser bs |> fun (x, l) -> DateTime(x), l }
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
    let ArrayC (keyC : C<'k>) : C<'k []> =
        { ser = fun m ->
            m
            |> Array.collect ^ fun x -> keyC.ser x
            |> fun xs -> Array.concat [ IntC.ser m.Length; xs ]
          deser = fun bs ->
            let (count, _) = IntC.deser bs
            List.init count ignore
            |> List.fold
                 (fun (m, l) ff ->
                     let (x, l2) = keyC.deser { bs with offset = bs.offset + l }
                     x :: m, l + l2) 
                 ([], 0) 
            |> fun (xs, l) -> List.toArray xs, l }
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

    let inline private read bs c2 l = 
        let (b, l2) = { bs with offset = bs.offset + l } |> c2.deser
        b, l2 + l
    let record2C c0 f0 c1 f1 g : C<'v> =
        { ser = fun m -> Array.concat [ f0 m |> c0.ser; f1 m |> c1.ser ]
          deser = fun bs ->
            let (a0, l) = read bs c0 0
            let (a1, l) = read bs c1 l
            g a0 a1, l }
    let record1C c0 f0 g : C<'v> =
        { ser = fun m -> Array.concat [ f0 m |> c0.ser ]
          deser = fun bs ->
            let (a0, l) = read bs c0 0
            g a0, l }
    let record3C (a : C<_>) f1 (b : C<_>) f2 (c : C<_>) f3 g : C<'v> =
        { ser = fun m -> 
            Array.concat [ f1 m |> a.ser; f2 m |> b.ser; f3 m |> c.ser ]
          deser = fun bs ->
            let (a, l1) = bs |> a.deser
            let (b, l2) = { bs with offset = bs.offset + l1 } |> b.deser
            let (c, l3) = { bs with offset = bs.offset + l2 + l1 } |> c.deser
            g a b c, l3 + l2 + l1 }
    let record4C c0 f0 c1 f1 c2 f2 c3 f3 g : C<'v> =
        { ser = fun m -> Array.concat [ f0 m |> c0.ser; f1 m |> c1.ser; f2 m |> c2.ser; f3 m |> c3.ser ]
          deser = fun bs ->
            let (a0, l) = read bs c0 0
            let (a1, l) = read bs c1 l
            let (a2, l) = read bs c2 l
            let (a3, l) = read bs c3 l
            g a0 a1 a2 a3, l }
    let record5C (c1 : C<_>) f1 (c2 : C<_>) f2 (c3 : C<_>) f3 c4 f4 c5 f5 g : C<'v> =
        { ser = fun m -> 
            Array.concat [ f1 m |> c1.ser; f2 m |> c2.ser; f3 m |> c3.ser; f4 m |> c4.ser; f5 m |> c5.ser ]
          deser = fun bs ->
            let (a0, l) = read bs c1 0
            let (a1, l) = read bs c2 l
            let (a2, l) = read bs c3 l
            let (a3, l) = read bs c4 l
            let (a4, l) = read bs c5 l
            g a0 a1 a2 a3 a4, l }
    let record10C c1 f1 c2 f2 c3 f3 c4 f4 c5 f5 c6 f6 c7 f7 c8 f8 c9 f9 c10 f10 g : C<'v> =
        { ser = fun m -> 
            Array.concat 
                [ f1 m |> c1.ser; f2 m |> c2.ser; f3 m |> c3.ser; f4 m |> c4.ser; f5 m |> c5.ser
                  f6 m |> c6.ser; f7 m |> c7.ser; f8 m |> c8.ser; f9 m |> c9.ser; f10 m |> c10.ser ]
          deser = fun bs ->
            let (a1, l) = read bs c1 0
            let (a2, l) = read bs c2 l
            let (a3, l) = read bs c3 l
            let (a4, l) = read bs c4 l
            let (a5, l) = read bs c5 l
            let (a6, l) = read bs c6 l
            let (a7, l) = read bs c7 l
            let (a8, l) = read bs c8 l
            let (a9, l) = read bs c9 l
            let (a10, l) = read bs c10 l
            g a1 a2 a3 a4 a5 a6 a7 a8 a9 a10, l }

    let inline mkSer c g s =
        { getSer = fun x -> c.ser ^ g x
          deserSet = fun x bs -> c.deser bs |> fun (f, l) -> s x f, l }
