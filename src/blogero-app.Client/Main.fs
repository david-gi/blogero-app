module blogero_app.Client.Main

open System
open System.IO
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client
open Microsoft.AspNetCore.Components.Forms

type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/search/{q}">] Query of q: string
    | [<EndPoint "/post/{id}">] Link of id: string
    | [<EndPoint "/create">] Create

type Model =
    { page: Page
      posts: Post[]
      current: Post option
      searchCriteria: (Post -> bool) option
      error: exn option }

and Post =
    { id: string
      title: string
      content: string
      image: string
      created: DateTime }

let initModel =
    { page = Home
      posts = [||]
      searchCriteria = None
      current = None
      error = None }

type PostService =
    {
        /// Get all demo post from json
        getPostsDemo: unit -> Async<Post[]>

        /// Commit a demo post to json
        addPostDemo: Post -> Async<unit>

        /// Get all post from Storj
        getPosts: unit -> Async<Post[]>

        /// Commit a post to Storj
        addPost: Post -> Async<unit>
    }

    interface IRemoteService with
        member this.BasePath = "/posts"

type Message =
    | SetPage of Page
    | GetPosts
    | GotPosts of Post[]
    | Search of string
    | ClearSearch
    | SetProp of PostProp
    | Save of Post
    | Saved of Post
    | Error of exn
    | ClearError

and PostProp =
    | Title of string
    | Image of string
    | Content of string

type Color =
    | Primary
    | Gray
    | Blue
    | Purple
    | Red
    | Orange
    | Yellow
    | Green

let parseColor =
    function
    | Primary -> "blue"
    | Gray -> "gray"
    | Blue -> "blue"
    | Purple -> "violet"
    | Red -> "red"
    | Orange -> "orange"
    | Yellow -> "yellow"
    | Green -> "green"

let getSelectedPosts model =
    match model.searchCriteria with
    | Some x -> (model.posts |> Array.filter x)
    | None -> model.posts

let lookupPost model (id: string) =
    try
        Some <| Array.find (fun x -> x.id = id) model.posts
    with err ->
        None

let getRelativePostId model offset =
    let getPost x = model.posts[x].id
    let selectedPosts = getSelectedPosts model

    let currentIndex =
        Array.findIndex (fun x -> x.id = model.current.Value.id) <| selectedPosts

    let len = selectedPosts.Length

    match (offset + currentIndex) with
    | x when x < 0 -> getPost (x + len)
    | x when x >= len -> getPost (x - len)
    | x -> getPost x

let handleFileUpload (dispatch: Dispatch<Message>) (e: InputFileChangeEventArgs) =
    async {
        let mbCount = 3
        let maxImageSize = 1024 * mbCount

        if e.File.Size > maxImageSize then
            dispatch <| Error(Exception $"File over size limit ({mbCount} MBs)")

        else if not (e.File.ContentType = "image/jpg" || e.File.ContentType = "image/jpeg") then
            dispatch <| Error(Exception "Only JPEG file format accepted")

        else
            let buffer: byte array = Array.zeroCreate 1024
            let ms = new MemoryStream(buffer)
            let fStream = e.File.OpenReadStream(maxImageSize)
            fStream.CopyToAsync(ms) |> Async.AwaitTask |> Async.RunSynchronously

            let res = Convert.ToBase64String <| ms.ToArray()
            dispatch <| SetProp(Image res)
    }

let guidString () = Guid.NewGuid().ToString()

let initPost () =
    { id = guidString ()
      title = String.Empty
      content = String.Empty
      image = String.Empty
      created = DateTime.UtcNow }

let update remote message model =
    match message with
    | SetPage page ->
        match page with
        | Home ->
            { model with
                page = page
                current = None },
            Cmd.ofMsg GetPosts
        | Create ->
            { model with
                page = page
                current = Some <| initPost () },
            Cmd.none
        | Link _ when model.page = Create -> model, Cmd.none
        | Link x ->
            { model with
                page = page
                current = lookupPost model x },
            Cmd.none
        | Query x ->
            { model with
                current = None
                page = if model.page = Create then Create else page },
            Cmd.ofMsg (Search x)

    | GetPosts -> model, Cmd.OfAsync.either remote.getPostsDemo () GotPosts Error
    | GotPosts posts -> { model with posts = posts }, Cmd.none

    | Search crit ->
        { model with
            searchCriteria = Some(fun x -> (x.title + x.content).ToLower().Contains(crit.ToLower())) },
        Cmd.none
    | ClearSearch -> { model with searchCriteria = None }, Cmd.none

    | SetProp prop ->
        match prop with
        | _ when model.current.IsNone -> model, Cmd.none
        | Title x ->
            { model with
                current = Some { model.current.Value with title = x } },
            Cmd.none
        | Image x ->
            { model with
                current = Some { model.current.Value with image = x } },
            Cmd.none
        | Content x ->
            { model with
                current = Some { model.current.Value with content = x } },
            Cmd.none
    | Save post -> model, Cmd.OfAsync.either remote.addPostDemo post (fun () -> Saved post) Error
    | Saved post ->
        { model with
            current = Some post
            page = Link post.id
            posts =
                let postList = post :: (model.posts |> Array.toList)
                postList |> List.toArray },
        Cmd.none

    | Error err -> { model with error = Some err }, Cmd.none
    | ClearError -> { model with error = None }, Cmd.none

let router = Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

let searchTemplate dispatch =
    Main
        .Textbox()
        .Placeholder("Filter...")
        .Text(String.Empty, (fun x -> dispatch <| SetPage(Query x)))
        .Elt()

let buttonTemplate (text: string) color dispatch message =
    Main
        .Button()
        .Text(text)
        .Color(parseColor color)
        .Click(fun _ -> dispatch message)
        .Elt()

let safetyButtonTemplate (text: string) color dispatch message =
    Main
        .SafetyButton()
        .Text(text)
        .Uid("Button-" + guidString ())
        .Color(parseColor color)
        .Click(fun _ -> dispatch message)
        .Elt()

let imgOrDefault post =
    if (post.image.Length > 0) then
        "data:image/jpeg;base64," + post.image
    else
        "icon.png"

let postTemplate post condensed dispatch =
    Main
        .PostContainer()
        .Title(post.title)
        .Date(post.created.ToShortDateString())
        .ImageSrc(imgOrDefault post)
        .Content(
            cond condensed
            <| function
                | true -> empty ()
                | false -> text post.content
        )
        .Open(fun _ -> dispatch <| SetPage(Link <| post.id))
        .Elt()

let formTemplate post dispatch =
    Main
        .FormContainer()
        .Image(
            concat {
                let elId = "ImgInput-" + guidString ()
                Main.ImgUploadLabel().For(elId).Elt()

                comp<InputFile> {
                    attr.id elId
                    attr.style "display: none"
                    attr.async.callback "OnChange" <| (handleFileUpload dispatch)
                }
            }
        )
        .ImagePreview(imgOrDefault post)
        .Title(
            Main
                .Textbox()
                .Placeholder("Enter title...")
                .Text(post.title, (fun x -> SetProp(Title x) |> dispatch))
                .Elt()
        )
        .Content(
            Main
                .Textarea()
                .Placeholder("Write content...")
                .Text(post.content, (fun x -> SetProp(Content x) |> dispatch))
                .Elt()
        )
        .Date(post.created.ToShortDateString())
        .Elt()

let errorTemplate model dispatch =
    cond model.error
    <| function
        | None -> empty ()
        | Some e ->
            Main
                .NotificationContainer()
                .Text(e.Message)
                .Color(parseColor Red)
                .Hide(fun _ -> dispatch ClearError)
                .Elt()

let view model dispatch =
    cond (model.posts.Length = 0)
    <| function
        | true -> Main().Side(Main.LoadingContainer().Elt()).Elt()
        | false ->
            Main()
                .Search(searchTemplate dispatch)
                .Side(
                    match getSelectedPosts model with
                    | xs when xs.Length > 0 -> forEach xs <| fun x -> postTemplate x true dispatch
                    | _ -> Main.NoMatchesContainer().Elt()
                )
                .Panel(
                    cond model.page
                    <| function
                        | Home
                        | Query _ -> empty ()
                        | Create ->
                            Main
                                .PanelContainer()
                                .Tray(
                                    concat {
                                        safetyButtonTemplate "Discard" Gray dispatch (SetPage Home)
                                        safetyButtonTemplate "Publish" Primary dispatch (Save model.current.Value)
                                    }
                                )
                                .Content(formTemplate model.current.Value dispatch)
                                .Elt()
                        | Link _ ->
                            match model.current with
                            | Some post ->
                                Main
                                    .PanelContainer()
                                    .Tray(
                                        concat {
                                            buttonTemplate "⤫" Gray dispatch (SetPage Home)

                                            buttonTemplate
                                                "►"
                                                Primary
                                                dispatch
                                                (SetPage(Link <| getRelativePostId model 1))

                                            buttonTemplate
                                                "◄"
                                                Primary
                                                dispatch
                                                (SetPage(Link <| getRelativePostId model -1))
                                        }
                                    )
                                    .Content(postTemplate post false dispatch)
                                    .Elt()
                            | None ->
                                Main
                                    .PanelContainer()
                                    .Tray(buttonTemplate "Close" Gray dispatch (SetPage Home))
                                    .Content(Main.NotFoundContainer().Elt())
                                    .Elt()
                )
                .Corner(
                    cond model.current
                    <| function
                        | Some _ -> empty ()
                        | None -> buttonTemplate "Write" Primary dispatch (SetPage Create)
                )
                .Footer(errorTemplate model dispatch)
                .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let postService = this.Remote<PostService>()
        let update = update postService

        Program.mkProgram (fun _ -> initModel, Cmd.ofMsg GetPosts) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
