module Example.App

module Domain =
    open Store.Gen

    let onClientEvent (db : LocalDb) =
        let id = System.Random().Next()
        { db with posts = Map.add (id) { id = id; title = "hello"; comments = 0 } db.posts }

    let onServerEvent (db : LocalDb) = 
        { db with posts = Map.map (fun _ v -> { v with comments = v.comments + 1 }) db.posts }

module ClientModule =
    open Browser
    open Store.Gen
    open Example.Types.Utils

    let sharedDb = ref { posts = Map.empty }

    let main _ =
        let button = document.getElementById "button"
        let text1 = document.getElementById "text1"
        let dumpDb () = text1.innerText <- sprintf "%s\n===\n%O" text1.innerText !sharedDb
        button.addEventListener(
            "click",
            (fun _ ->
                text1.innerText <- ""
                promise {
                    dumpDb()
                    let db = !sharedDb
                    let db2 = Domain.onClientEvent db
                    sharedDb := db2
                    dumpDb()
                    let! d = 
                        serializeDiff db db2
                        |> fetchBytes "/localdb"
                    sharedDb := applyDiff !sharedDb d
                    dumpDb()
                } |> Promise.start))

#if FABLE_COMPILER
    main ()
#endif

#if !FABLE_COMPILER

module ServerModule =
    open Store.Gen

    let sharedDb = ref { posts = Map.empty }
    let handle (diff : byte[]) : byte[] =
        sharedDb := applyDiff !sharedDb diff
        let db2 = Domain.onServerEvent !sharedDb
        let diff = serializeDiff !sharedDb db2
        sharedDb := db2
        diff

open Suave
open Suave.Filters
open Suave.Operators

[<EntryPoint>]
let main _ =
    choose [
        GET >=> path "/" >=> Successful.OK """
<html>
    <body>
        <button id="button">Press me</button>
        <span id="text1"></span>
        <script src="bundle.js"></script>
    </body>
</html>"""
        GET >=> path "/bundle.js" >=> request (fun _ -> System.IO.File.ReadAllText("bin/bundle.js") |> Successful.OK)
        POST >=> path "/localdb" >=> request (fun r -> 
            r.rawForm
            |> ServerModule.handle
            |> Successful.ok) ]
    |> startWebServer defaultConfig
    0

#endif
