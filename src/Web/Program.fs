open Suave

module HelloWorldModule =
  let okRes = Successful.OK "Hello F# World!"
  let bye = Successful.OK "Goodbye World!"

module FilesModule =
  open System
  open System.IO
  open System.Reflection

  // you can copy-and-paste this:
  let rootPath =
    Assembly.GetExecutingAssembly().CodeBase
    |> fun s -> (Uri s).AbsolutePath
    |> Path.GetDirectoryName

  let cats =
    Files.browse (Path.Combine (rootPath, "cats"))

  let app =
    Files.browseFile rootPath "files/hello.txt"


module RoutingModule =
  open System.IO
  open Suave.Filters
  open Suave.Successful
  open Suave.Operators

  let private aCat fileName =
    Path.Combine (FilesModule.rootPath, "cats", fileName)

  let app : WebPart =
    choose [
      path "/hi" >=> HelloWorldModule.okRes
      path "/bye" >=> HelloWorldModule.bye
      // never routing: equivalent as never returning
      path "/never" >=> never >=> OK "DID RETURN?"
      path "/never2" >=> (fun ctx -> fail) >=> OK "DID RETURN?"  // never is still a webpart returning fail
      // fail is nothing else than a fun returning (async) None of type HttpContext option
      path "/never2" >=> (fun ctx -> async.Return(None : HttpContext option)) >=> OK "DID RETURN?"  
      // never routing end
      path "/cat" >=> Files.file (aCat "3.jpg")
      pathScan "/cats/%i/image" (
        sprintf "%i.jpg"
        >> aCat
        >> Files.file)

      pathScan "/hi/%s" (fun name -> OK (sprintf "Hi, %s!" name))
      pathScan "/bye/%s" (fun name -> OK (sprintf "Good bye, %s!" name))

      never >=> request (fun r -> OK (sprintf "You requested %s" r.url.AbsolutePath))

      path "/test-context" >=> context (fun c ->
        let state =
          c.userState
          |> Map.add "message" (sprintf "You requested (ctx) %s" c.request.url.AbsolutePath |> box)
        fun ctx -> async.Return(Some { ctx with userState = state })
      ) >=> warbler (fun ctx -> OK (ctx.userState.["message"] :?> string))

      path "/responses" >=> request (fun r ->
        fun ctx -> async.Return (Some ctx)
      )
      path "/responses2" >=> request (fun r ->
        fun ctx -> 
          async {
            return Some ctx
          }
      )
      path "/responses3" >=> request (fun r ->
        let bs = System.Text.Encoding.UTF8.GetBytes "Hello"
        fun ctx -> 
          async {
            return Some
              { ctx with
                  response =
                    { ctx.response with
                        content = Bytes bs } }
          }
      )

      RequestErrors.NOT_FOUND "Not found"
    ]

module LazyModule =
  open System
  open Suave.Filters
  open Suave.Successful
  open Suave.Operators

  let app =
    choose [
      path "/eager" >=> OK (sprintf "%O" DateTimeOffset.Now)
      path "/lazy" >=> context (fun _ -> OK (sprintf "%O" DateTimeOffset.Now))

      path "/challenge" >=> context (fun _ ->
        let now = DateTimeOffset.Now
        if now.Second % 2 = 0 then OK "hi" else OK "bye"
      )
    ]

module IndexModule =
  open Suave
  open Suave.Successful
  open Suave.RequestErrors
  open Suave.Operators
  open Suave.Filters

  let app =
    choose [
      GET >=> path "/" >=> Files.browseFileHome "index.html"
      GET >=> path "/index.html" >=> Files.browseFileHome "index.html"
      POST >=> request (fun r -> 
        match r.formData "name" with
        | Choice1Of2 name when name.Length <> 0 ->
          OK (sprintf "Got your message, %s" name)

        | Choice1Of2 _ ->
          BAD_REQUEST "Please provide your name"

        | Choice2Of2 error ->
          BAD_REQUEST error)

      Writers.setStatus HTTP_404 >=> Files.browseFileHome "not_found.html"
    ]

module SteoFile = 
  open System
  open System.IO
  open System.Reflection

  let rootPath =
    Assembly.GetExecutingAssembly().CodeBase
    |> fun s -> (Uri s).AbsolutePath
    |> Path.GetDirectoryName

  let app =
    //Files.browse (Path.Combine(rootPath, "files"))
    //Files.browseFileHome "files/hello.txt"
    Files.browseFile rootPath "files/hello.txt"

  let showPic = 
    //Files.browseFileHome "cats/1.jpg"
    Files.browse (Path.Combine(rootPath, "cats")) // and then http://localhost:8083/2.jpg to browse the file(s)

  let showAPic picName = 
    Files.file (Path.Combine(rootPath, "cats", picName)) // and then http://localhost:8083/2.jpg to browse the file(s)

module SteoRouteModule = 
  open System.IO
  open Suave.Filters
  open Suave.Successful
  open Suave.Operators  // for the >=> (fsh? ) operator

  let getImagePath imgName = 
    Path.Combine(SteoFile.rootPath, "cats", imgName)

  let app: WebPart = 
    choose [
      path "/hi_old" >=> HelloWorldModule.okRes
      path "/bye_old" >=> HelloWorldModule.bye
      path "/img" >=> SteoFile.showAPic "1.jpg"
      path "/never" >=> never >=> OK "I don't think I ever got here..."
      // the following are equivalent...
      path "/never2" >=> (fun ctx -> 
          printfn "never"   // just to log and verify that the route is actually called
          fail) >=> OK "no return again... " // never is a Webpart that means... fail
      // or we can see fail as a ... 
      path "/never3" >=> (fun ctx -> async.Return(None: HttpContext option)) >=> OK "whatever ... " 
      //pathScan "/img/%i/image" (fun x -> getImage (sprintf "%i.jpg"  x))
      // to not repeat
      //pathScan "/img/%i/image" (fun x -> SteoFile.showAPic (sprintf "%i.jpg"  x))
      // point free notation
      // pathScan "/img/%i/image" (fun x -> x
      //   >> sprintf "%i.jpg"
      //   >> SteoFile.showAPic)
      // or even shorter
      pathScan "/img/%i/image" (
        sprintf "%i.jpg"
        >> getImagePath
        >> Files.file
      )

      // routing challenge
      pathScan "/hi/%s" (fun x -> Successful.OK (sprintf "Hi, %s" x))
      pathScan "/bye/%s" (fun y -> OK (sprintf "Bye, %s" y)) // or I can use the shorten version of only OK

      never >=> request (fun r -> OK (sprintf "you requested %s" r.url.AbsolutePath))  // never call/use this
      // we want to change the context ...
      // context (fun c -> OK (sprintf "you requested %s" c.request.url.AbsolutePath))
      path "/context" >=> context (fun c -> 
        let state =
          c.userState 
          // ERROR - userState is a Map<string, obj> so we need to box the 2nd string to obj
          //|> Map.add "message" (sprintf "you requested %s" c.request.url.AbsolutePath) 
          |> Map.add "message" (sprintf "you requested (ctx) %s" c.request.url.AbsolutePath :> obj)  // or ... |> obj (:> = cast operator)
        // NOT working = we need to return a HttpContext Async
        //async.Return(Some { c with userState = state })  // we want to change the state 
        // we need to box cause we need to return a HttpContext -> Asynx<HttpContext option> instead we hane Asynx<'a>
        fun ctx -> async.Return(Some { ctx with userState = state }) // so we need to wrap it up (using a function) .. c and ctx are the same context
      ) >=> warbler (fun ctx -> OK (ctx.userState.["message"] :?> string)) // using warbler, a generic function (request and context are specialized warbler(s)),
      // that defer the execution of the webpart passed to (webpart is the base block of Suave)
      //eg: this minimal webserver print always the same datetime (when the server is started):
      // let app = GET >=> path "/" >=> OK (string DateTime.Now)
      // with warbler allow the webserver to actual call the fucntion everytime it needs the result
      // let app = GET >=> path "/" >=> warbler (fun ctx -> OK (string DateTime.Now))

      // *** WEBPART (in Suave) -> it's a function from Context to Async HttpContext option ***

      path "/responses" >=> request (fun r ->
          fun ctx -> async.Return(Some ctx)
        )
      // or... with Async Workflow
      path "/responses2" >=> request (fun r ->
          fun ctx -> async {
            return Some ctx   // we need to add return in this case
          }
        )

      path "/responses3" >=> request (fun r ->   // trying to return some data, so return the context and returns
        let bs = "Hello" |> System.Text.Encoding.UTF8.GetBytes
        fun ctx -> 
          async {
            return Some
              {
              // here we change the Context (ctx) to use the data we generated (the bytes array, bs)
                ctx with response = { ctx.response with content = Bytes bs }
              }
          }
      )

      // Second routing challenge - Default 404 page
      // -- request (fun r -> Files.browseFileHome "notfound.html")

      RequestErrors.NOT_FOUND "Not found"
    ]


module SteoLazyModule =
  open System
  open Suave.Filters
  open Suave.Successful
  open Suave.Operators

  // calling this module will evaluate all the code inside... it means that the eager part will be evaluate
  // whilst the lazy one, thanks to the context function will be evaluated only when called
  let app =
    choose [
      path "/eager" >=> OK (sprintf "%O" DateTimeOffset.Now) 
      path "/lazy" >=> context (fun _ -> OK (sprintf "%O" DateTimeOffset.Now))
      // timely greeting challenge - hi when even seconds, bye otherwise (uneven seconds)
      path "/hibye" >=> context (fun _ -> 
        let greet = if (DateTime.Now.Second % 2 = 0) then "Hi" else "Bye"
        OK greet
      )
    ]

module SteoIndexModule =
  open System
  open Suave
  open Suave.Successful
  open Suave.RequestErrors
  open Suave.Operators
  open Suave.Filters

  let app = 
    choose [
      // path "/" >=> Files.browseFileHome "index.html"
      // or lazy evaluate it (read when it changes
      GET >=> path "/" >=> context(fun _ -> Files.browseFileHome "index.html")
      POST >=> request (fun r -> 
        match r.formData "name" with   // using the Choice Monad   (use name2 or a wrong string to checkl the error..
        | Choice1Of2 name when name.Length > 0 -> OK (sprintf "OK messagge got it!! Thanks, %s" name)
        | Choice1Of2 _ -> BAD_REQUEST "Please provide your name"
        | Choice2Of2 error -> BAD_REQUEST error   // provided by Suave
      )
      // challenge 
      GET >=> path "/index.html" >=> context(fun _ -> Files.browseFileHome "index.html")
      // NOT_FOUND take a string but we can override it by telling the file applicative to write its own context 
      // NOT_FOUND "" >=> Files.browseFileHome "not_found.html"
      // using this way, instead so we don't have to use an empty string
      Writers.setStatus HTTP_404 >=> Files.browseFileHome "not_found.html"
    ]

[<EntryPoint>]
let main argv =
  // startWebServer defaultConfig IndexModule.app
  //startWebServer defaultConfig (Successful.OK "Hello F# World from Suave!!!")
  // serving file...
  //startWebServer defaultConfig SteoFile.app
  //startWebServer defaultConfig SteoFile.showPic
  //startWebServer defaultConfig SteoRouteModule.app
  //startWebServer defaultConfig SteoLazyModule.app
  startWebServer defaultConfig SteoIndexModule.app
  0
