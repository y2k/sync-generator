# Генератор F# клиент-серверного клиента по контракту

## Yaml описание контракта
```yaml
module: Store.Gen
open: Example.Types
items:
- name: posts
  type: Map<int, Post>
- name: userName
  type: string option
- name: messages
  type: Message Set
```

Для этого контракта гененируется тип
```f#
type LocalDb =
    { posts : Map<int, Post>
      userName : string option
      messages : Message Set }
```
И две функции сериализирующие и десериалиализующие изменения
```f#
let serializeDiff (origin : LocalDb) (change : LocalDb) : byte [] = ...
let applyDiff (origin : LocalDb) (serializedDiff : byte[]) : LocalDb = ...
```
Дальше, через стандартные средства (xhr/websocket) их можно передевать по сети, синхронизирую стейт на клиенте и сервере.

## Пример
[Полный пример](example/Program.fs)

### Общие типы и сериализатор
```f#
type Post = { id : int; title: string; comments: int }

let PostC : C<Post> =
    recordC
        [ intC (fun x -> x.id) (fun x f -> { x with id = f })
          stringC (fun x -> x.title) (fun x f -> { x with title = f })
          intC (fun x -> x.comments) (fun x f -> { x with comments = f }) ] 
        { id = 0; title = ""; comments = 0 }
```

### Общая бизнес-логика
```f#
module Domain =
    open Store.Gen

    let onClientEvent (db : LocalDb) : LocalDb =
        let id = System.Random().Next()
        { db with posts = Map.add (id) { id = id; title = "hello"; comments = 0 } db.posts }

    let onServerEvent (db : LocalDb) : LocalDb = 
        { db with posts = Map.map (fun _ v -> { v with comments = v.comments + 1 }) db.posts }
```

### Клиентский код (FABLE)
```f#
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
```

### Серверный код (.NET Core)
```f#
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
```
