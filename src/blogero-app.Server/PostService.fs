namespace blogero_app.Server

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open Microsoft.AspNetCore.Hosting
open Bolero
open Bolero.Remoting
open Bolero.Remoting.Server
open blogero_app

type PostService(ctx: IRemoteContext, env: IWebHostEnvironment) =
    inherit RemoteHandler<Client.Main.PostService>()

    let filepath = Path.Combine(env.ContentRootPath, "data/posts.json")
    let posts =
        let json = File.ReadAllText filepath
        JsonSerializer.Deserialize<Client.Main.Post[]>(json) |> ResizeArray

    override this.Handler =
        {
            getPosts = fun () -> async {
                return posts.ToArray() |> Array.rev
            }

            addPost = fun post -> async {
              posts.Add(post)
              let json = JsonSerializer.Serialize(posts) 
              File.WriteAllText (filepath, json)
            }
        }
