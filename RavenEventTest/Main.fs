module RavenEventTest.Main

open Raven.Client.Embedded
open NEventStore
open NEventStore.Persistence.RavenPersistence

let staticid = System.Guid("AF02CF7B-21AD-475D-B79F-635BF0D4A08E")


let openEventStore () = 
  NEventStore.Wireup.Init()
    .UsingInMemoryPersistence()
    //.UsingRavenPersistence("RavenLocal")
    //.ConsistentQueries()
    .InitializeStorageEngine()
    .UsingSynchronousDispatchScheduler()
    .Build()
  

[<EntryPoint>]
let main argv = 
  let store = openEventStore()

  let games = Agg.NEventAggregate(Bowling.BowlingAggregate, store)


  let newGameId = System.Guid.NewGuid()
  let commit, event, state = Bowling.Command.RegisterPlayer "joe" |> games.Exec newGameId 
  
  printfn "%A -> %A" event state
  for x in [1..20] do
    let commit, event, state = Bowling.Command.RollBall 0.75 |> games.Exec newGameId 
    
    printfn "%A -> %A" event state

  System.Console.ReadKey() |> ignore
  0 // return an integer exit code
