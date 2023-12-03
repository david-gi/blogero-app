module blogero_app.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/post/{id}">] Link of id: string  
    | [<EndPoint "/create">] Create

type Model =
    {
        page: Page
        posts: Post[]
        current: Post option
        error: exn option
    }
    
and Post =
    {
        id: string
        title: string
        content: string
        image: string
        created: DateTime
    }

let initModel =
    {
        page = Home
        posts = [|
            {
                id = "123-321";
                title = "F is fun";
                content = "F# is peace. It's like an antiphlogistic for the mind. Love your code again.";
                image = "";
                created = DateTime.Now;
            }
            {
                id = "123-321";
                title = "A pleasant dev experience";
                content = "I enjoyed every part of it, as did I Websharper. Never used Elm but love the Elmish flow. And then there are these magic Templates <B";
                image = "";
                created = DateTime.Now;
            }
            {
                id = "123-321";
                title = "Holy Bolero!";
                content = "Develop SPAs with the full power of F# and .NET.";
                image = "";
                created = DateTime.Now;
            }
        |]
        current = None
        error = None
    }

type Message =
    | SetPage of Page
    | GetPosts
    | Save of Post
    | Error of exn
    | ClearError

type Color =
    | Primary
    | Gray
    | Blue
    | Purple
    | Red
    | Orange
    | Yellow
    | Green
    
let parseColor = function
    | Primary -> "blue"
    | Gray -> "gray"
    | Blue -> "blue"
    | Purple -> "violet"
    | Red -> "red"
    | Orange -> "orange"
    | Yellow -> "yellow"
    | Green -> "green"

let lookupPost model (id: string) =
    try Some <| Array.find (fun x -> x.id = id) model.posts
    with | err -> None

let getRelativePostId model offset = 
    let getPost x = model.posts[x].id
    let currentIndex = Array.findIndex (fun x -> x.id = model.current.Value.id) <| model.posts
    let len = model.posts.Length
    match (offset + currentIndex) with
    | x when x < 0 -> getPost (x + len)
    | x when x >= len -> getPost (x - len)
    | x -> getPost x

let update message model =
    match message with
    | SetPage page -> 
        match page with
        | Home -> { model with page = page; current = None }, Cmd.ofMsg GetPosts
        | Create ->
            { model with 
                page = page; 
                current = Some { 
                        id = (Guid.NewGuid ()).ToString(); 
                        title = String.Empty; 
                        content = String.Empty; 
                        image = String.Empty; 
                        created = DateTime.UtcNow;
                    }
            }, Cmd.none
        | Link _ when model.page = Create -> model, Cmd.none
        | Link x -> { model with page = page; current = lookupPost model x }, Cmd.none

    | GetPosts -> model, Cmd.none
    | Save _ -> model, Cmd.none

    | Error err -> { model with error = Some err }, Cmd.none
    | ClearError -> { model with error = None }, Cmd.none

let router = Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

let postTemplate post condensed dispatch =
    Main.PostContainer()
        .Title(post.title)
        .Date(post.created.ToShortDateString())
        .ImageSrc("icon.png")
        .Content(cond condensed <| function 
                | true -> empty() 
                | false -> text post.content)
        .Open(fun _ -> dispatch <| SetPage (Link <| post.id))
        .Elt()

let buttonTemplate (text: string) color dispatch message =
    Main.Button()
        .Text(text)
        .Color(parseColor color)
        .Click(fun _ -> dispatch message)
        .Elt()

let errorTemplate model dispatch = 
    cond model.error <| function
    | None -> empty()
    | Some e ->
        Main.NotificationContainer()
            .Text(e.Message)
            .Color(parseColor Red)
            .Hide(fun _ -> dispatch ClearError)
            .Elt()         

let view model dispatch =
    Main()
        .Side(concat {
                let makeNode = fun x -> postTemplate x true dispatch
                match model.posts with
                | xs when xs.Length > 0 -> forEach xs <| makeNode
                | _ -> text "no results"
            })
        .Panel(
            cond model.page <| function
            | Home -> empty()
            | Create ->
                Main.PanelContainer()
                    .Tray(concat {
                            buttonTemplate "Discard" Gray dispatch (SetPage Home)
                            buttonTemplate "Publish" Primary dispatch (Save model.current.Value)
                        })
                    .Content(text "")
                    .Elt()
            | Link _ ->
                match model.current with
                | Some post ->
                    Main.PanelContainer()
                        .Tray(concat {
                                buttonTemplate "⤫" Gray dispatch (SetPage Home)
                                buttonTemplate "►" Primary dispatch (SetPage (Link <| getRelativePostId model 1))
                                buttonTemplate "◄" Primary dispatch (SetPage (Link <| getRelativePostId model -1))
                            })
                        .Content(postTemplate post false dispatch)
                        .Elt()
                | None -> text "404")
        .Corner(
            cond model.current <| function
            | Some _ -> empty()
            | None -> buttonTemplate "Write" Primary dispatch (SetPage Create))
        .Footer(errorTemplate model dispatch)
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()
    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.ofMsg GetPosts) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
