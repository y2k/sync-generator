module Example.Types

open App.Serializer

type Post = { id : int; title: string; comments: int }

let PostC : C<Post> =
    recordC
        [ intC (fun x -> x.id) (fun x f -> { x with id = f })
          stringC (fun x -> x.title) (fun x f -> { x with title = f })
          intC (fun x -> x.comments) (fun x f -> { x with comments = f }) ] 
        { id = 0; title = ""; comments = 0 }

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
