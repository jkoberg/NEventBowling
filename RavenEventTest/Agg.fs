module RavenEventTest.Agg


open System

// Foundational ideas:

// A stream contains all the messages needed to restore the state of an aggregeate
// The streamId can be considered the aggregate root identity
// the stream events can be 

// Commands come from ???
// Commands are checked for validity against the current state
// Events are created to represent the commands
// The stream of events is applied to mutate the model
// 
 
// from http://gorodinski.com/blog/2013/02/17/domain-driven-design-with-fsharp-and-eventstore/

type Id = Guid

/// A bundle of functions that implement the aggregate incremental state transform
type Aggregate<'TState, 'TCommand, 'TEvent> = {
  /// the "empty" initial state that further events will mutate
  zero  : 'TState
  /// mutate the state as the result of an event
  apply : 'TState -> 'TEvent -> 'TState
  /// given a state and command, validate the command and return the event that it would result in
  exec  : 'TState -> 'TCommand -> 'TEvent
  }


/// An interface to NEventStore that emits and consumes Events
type NEventStoreLoader<'TEvent>(store:NEventStore.IStoreEvents) = 

  /// returns the current revision number and entire stream of events for a given streamId
  member this.load id = 
    use stream = store.OpenStream(id, 0, System.Int32.MaxValue)
    stream.StreamRevision, seq {
      for eventmsg in stream.CommittedEvents do
        match eventmsg.Body with
        | :? 'TEvent as e ->
          yield e
        | _ as e ->
          failwith (sprintf "Didn't get expected event type %s.  Instead got %s. Value:\n%A" (typeof<'TEvent>.ToString()) (e.GetType().ToString()) e)
      }

  /// attempts to commit an event to a given streamId at a certain revision
  member this.commit streamId revision (event:'TEvent) = 
    use stream = store.OpenStream(streamId, 0, revision)
    stream.Add(NEventStore.EventMessage(Body=event))
    let commitid = System.Guid.NewGuid()
    stream.CommitChanges(commitid)
    commitid
    

/// Adapts an Aggregate to store and load itself from an NEventStore store
type NEventAggregate<'TState, 'TCommand, 'TEvent>(agg:Aggregate<'TState,'TCommand,'TEvent>, store) =

  let loader = NEventStoreLoader<'TEvent>(store)

  /// Return a revision and object hydrated from the stream, or an empty (zero) object for nonexistant streamids
  member this.Load streamid =
    let revision, events = loader.load streamid
    let state = events |> Seq.fold agg.apply agg.zero
    (revision, state)

  /// Return the commitid, resulting event, and new state after attempting to execute a command against an aggregate.
  /// The aggregate will be loaded to validate the command. 
  member this.Exec id command =
    let oldrevision, oldstate = this.Load id
    let resultingEvent = agg.exec oldstate command // exception if conflict??
    let commitid =  loader.commit id oldrevision resultingEvent
    let newstate = agg.apply oldstate resultingEvent
    (commitid, resultingEvent, newstate)
    