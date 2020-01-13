module Example.Types

open SyncGenerator.Lib.Serializer

type Post = { id : int; title: string; comments: int }

let PostC : C<Post> =
    record3C
        IntC (fun x -> x.id)
        StringC (fun x -> x.title)
        IntC (fun x -> x.comments)
        (fun a b c -> { id = a; title = b; comments = c })

module Utils =
    open Browser
    open Fetch
    open Fable.Core

    let fetchBytes url diff =
      let toBytes (a : Types.Response) =
          promise {
              let! b = a.arrayBuffer()
              let c = Fable.Core.JS.Constructors.DataView.Create(b)
              let d : byte [] = Array.zeroCreate b.byteLength
              for i in [ 0 .. d.Length - 1 ] do
                  d.[i] <- c.getUint8(i)
              return d
          }
      let toBlob (diff : byte []) =
          let a = JS.Constructors.Uint8Array.Create(diff)
          Blob.Create([| a |])
      promise {
          let! a = fetch url [ Method HttpMethod.POST; Body (U3.Case1 <| toBlob diff) ]
          return! toBytes a
      }
