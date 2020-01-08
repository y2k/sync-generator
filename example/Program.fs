module Example

module ClientModule =
    open Fable.Import.Browser
    let main _ =
        window.alert "Hello World"

#if FABLE_COMPILER
    main ()
#endif

#if !FABLE_COMPILER

module ServerModule =
    let handle (_ : byte[]) : byte[] = failwith "???"

open Suave
open Suave.Filters
open Suave.Operators

[<EntryPoint>]
let main _ =
    choose [
        GET >=> path "/" >=> Successful.OK """
<html>
    <body>
        <script src="bundle.js"></script>
    </body>
</html>"""
        GET >=> path "/bundle.js" >=> request (fun _ -> System.IO.File.ReadAllText("bin/bundle.js") |> Successful.OK)
        POST >=> request (fun r -> 
            r.rawForm
            |> ServerModule.handle
            |> Successful.ok) ]
    |> startWebServer defaultConfig
    0

#endif
