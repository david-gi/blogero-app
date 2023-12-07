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
open uplink.NET.Models
open uplink.NET.Services

type PostService(ctx: IRemoteContext, env: IWebHostEnvironment) =
    inherit RemoteHandler<Client.Main.PostService>()

    //Demo only
    let demoFilepath = Path.Combine(env.ContentRootPath, "data/posts.json")

    let demoPosts =
        let json = File.ReadAllText demoFilepath
        JsonSerializer.Deserialize<Client.Main.Post[]>(json) |> ResizeArray

    //Setting up Storj [https://github.com/TopperDEL/uplink.net/wiki/Documentation]
    let setupStorj () =
        let bucketName = "myblogerobucket"
        let access = new Access("YOUR_ACCESS_GRANT")
        let bucketService = new BucketService(access)

        let awaitTaskAndRun x =
            x |> Async.AwaitTask |> Async.RunSynchronously
        let bucket = match bucketService.GetBucketAsync(bucketName) |> awaitTaskAndRun with
                        | x when x = null -> bucketService.CreateBucketAsync(bucketName) |> awaitTaskAndRun
                        | x -> x
        let objectService = new ObjectService(access)
        (bucket, objectService)

    override this.Handler =
        { getPostsDemo = fun () -> async { return demoPosts.ToArray() |> Array.rev }

          addPostDemo =
            fun post ->
                async {
                    let json = JsonSerializer.Serialize(demoPosts)
                    File.WriteAllText(demoFilepath, json)
                }

          getPosts =
            fun () ->
                async {
                    let bucket, objectService = setupStorj ()
                    let listObjectsOptions = new ListObjectsOptions()
                    listObjectsOptions.Custom <- true

                    let objectList =
                        objectService.ListObjectsAsync(bucket, new ListObjectsOptions())
                        |> Async.AwaitTask
                        |> Async.RunSynchronously

                    let dowloadAndDeserialize =
                        fun (obj: uplink.NET.Models.Object) ->
                            let downloadOperation =
                                objectService.DownloadObjectAsync(bucket, obj.Key, new DownloadOptions(), false)
                                |> Async.AwaitTask
                                |> Async.RunSynchronously

                            let json = Text.Encoding.UTF8.GetString(downloadOperation.DownloadedBytes)
                            JsonSerializer.Deserialize<Client.Main.Post>(json)

                    return objectList.Items.ToArray() |> Array.map dowloadAndDeserialize
                }

          addPost =
            fun post ->
                async {
                    let bucket, objectService = setupStorj ()
                    let json = JsonSerializer.Serialize(post)
                    let bytesToUpload = Text.Encoding.UTF8.GetBytes json

                    let customMetadata = new CustomMetadata()
                    let customDataEntry = new CustomMetadataEntry()
                    customDataEntry.Key <- "post-id"
                    customDataEntry.Value <- post.id
                    customMetadata.Entries.Add customDataEntry

                    let uploadOperation =
                        objectService.UploadObjectAsync(
                            bucket,
                            post.id,
                            new UploadOptions(),
                            bytesToUpload,
                            customMetadata,
                            true
                        )
                        |> Async.AwaitTask
                        |> Async.RunSynchronously

                    uploadOperation.StartUploadAsync() |> Async.AwaitTask |> Async.RunSynchronously
                } }
