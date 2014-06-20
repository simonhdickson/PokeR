namespace PokerSignalR

open FunScript
open FunScript.Owin
open FunScript.SignalR
open FunScript.SignalR.Observable
open FunScript.SignalR.Connection
open FunScript.SignalR.Transports
open FunScript.TypeScript

[<JS>]
module App =
    type j = Api<"https://github.com/c9/typescript/raw/master/typings/jquery.d.ts">
    type ts = Api<"https://github.com/c9/typescript/raw/master/typings/lib.d.ts">

    type
        Card = Suit * Value
    and
        Suit = Heart | Diamond | Club | Spade
    and
        Value = Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two | One

    type Message =
        | Hand  of Card * Card
        | Flop  of Card * Card * Card
        | Turn  of Card
        | River of Card
    
    [<JSEmit("return console.log({0});")>]
    let log(str:obj):unit = failwith "never"

    [<Export("app")>]
    let entrypoint() =
        Log.Config.setConfig [ 
            Log.Config.Entry("transports.*", Log.All);
            Log.Config.Entry("connection.channel", Log.All)
        ]
        let rec start () =
            async {
                let! connection = Connection.connect("http://localhost:34298/signalr/poker")
                match connection with
                | Succeeded(chan) ->
                    chan
                    |> Connection.messages<Message>
                    |> Observable.add log
                    |> ignore
                    chan
                    |> Connection.disconnect (fun () -> log("Disconnected"))
                    do! deal chan
                | _ -> ()
            }
        and deal chan =
            async {
                chan.Send("ping")
        
                do! chan 
                    |> Connection.messages
                    |> Observable.first
                    |> Async.ignore
            }
                
        start() |> Async.StartImmediate
