module RavenEventTest.Bowling

let private Rnd = System.Random()

let dropSomeWithProb p = 
  Set.filter (fun i -> Rnd.NextDouble() <= p)


type GamePhase = 
  | GameFinished
  | WaitingForPlayer
  | WaitingForStaff

type ScoreStatus = 
  | Incomplete of FrameScores:int list * UnscoredRolls: int list
  | Complete of FrameScores:int list

type State = {
  phase: GamePhase
  playerName: string
  pinsUp: Set<int>
  rollRecord: int list
  lastGameScores: int list
  } with
  static member Zero = {
    phase = GameFinished
    playerName = null
    pinsUp = Set [1..10]
    rollRecord = []
    lastGameScores = []
    }





type Event =
  | PlayerRegistered of Name:string
  | PinsDropped of Pins:Set<int>
  | FrameEnded of Pins:Set<int>
  | BallStranded
  | BallRecovered
  | GameOver of FrameScores:int list


let Apply state event = 
  match event with
  | PlayerRegistered name -> {state with playerName = name; phase = WaitingForPlayer}
  | PinsDropped dropped   -> {state with pinsUp = state.pinsUp - dropped; phase = WaitingForPlayer; rollRecord = dropped.Count::state.rollRecord}
  | FrameEnded dropped    -> {state with pinsUp = Set [1..10]; phase = WaitingForPlayer; rollRecord = dropped.Count::state.rollRecord}
  | BallStranded          -> {state with phase = WaitingForStaff}
  | BallRecovered         -> {state with phase = WaitingForPlayer}
  | GameOver scores       -> {State.Zero with lastGameScores=scores}





module Rules = 
  let rec scoreRolls' accumulated rolls = 
    match rolls with 
    | r1::(b1::b2::_ as more) when r1 = 10      -> r1 + b1 + b2 :: accumulated |> scoreRolls' more
    | r1::r2::(b1::_ as more) when r1 + r2 = 10 -> r1 + r2 + b1 :: accumulated |> scoreRolls' more
    | r1::r2::more            when r1 + r2 < 10 -> r1 + r2 :: accumulated |> scoreRolls' more
    | []                                        -> Complete accumulated
    | _                                         -> Incomplete (accumulated, rolls)

  let scoreRolls = scoreRolls' []




type Command = 
  | RegisterPlayer of Name:string
  | RollBall of Probability:float
  | RecoverBall


let Exec state command =
  match command with
  | RegisterPlayer n -> if state.phase = GameFinished then PlayerRegistered n
                        else failwith "to register, game must not be started and no player registered."

  | RollBall p ->       match state.phase with
                        | GameFinished -> BallStranded
                        | WaitingForStaff -> failwith "You've already stranded a ball.  Get Out!"
                        | WaitingForPlayer ->
                          let pinsHit = state.pinsUp |> dropSomeWithProb p
                          match pinsHit.Count :: state.rollRecord |> List.rev |> Rules.scoreRolls with
                          | Incomplete (frames,rolls) -> PinsDropped pinsHit
                          | Complete framelist -> if framelist.Length = 10
                                                    then GameOver framelist
                                                    else FrameEnded pinsHit

  | RecoverBall ->     match state.phase with
                        | WaitingForStaff -> BallRecovered
                        | GameFinished -> failwith "What ball????"
                        | WaitingForPlayer -> failwith "What ball???"
  
      
open Agg
let BowlingAggregate = {apply = Apply; exec=Exec; zero=State.Zero}

