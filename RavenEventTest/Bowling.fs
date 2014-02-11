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


let Apply state = function
  | PlayerRegistered name -> { state with playerName = name
                                          phase = WaitingForPlayer }

  | PinsDropped dropped   -> { state with pinsUp = state.pinsUp - dropped
                                          phase = WaitingForPlayer
                                          rollRecord = dropped.Count::state.rollRecord }

  | FrameEnded dropped    -> { state with pinsUp = Set [1..10]
                                          phase = WaitingForPlayer
                                          rollRecord = dropped.Count::state.rollRecord }

  | BallStranded          -> { state with phase = WaitingForStaff }

  | BallRecovered         -> { state with phase = WaitingForPlayer }

  | GameOver scores       -> { State.Zero with lastGameScores = scores }





module Rules = 
  let rec scoreRolls' scored  unscored =
    match unscored with
    | r1::(b1::b2::_ as more) when r1 = 10       ->  r1 + b1 + b2 :: scored |> scoreRolls' more
    | r1::r2::(b1::_ as more) when r1 + r2 = 10  ->  r1 + r2 + b1 :: scored |> scoreRolls' more
    | r1::r2::more            when r1 + r2 < 10  ->  r1 + r2 :: scored      |> scoreRolls' more
    | [] -> Complete scored
    | _  -> Incomplete (scored, unscored)

  let scoreRolls = scoreRolls' []



type Command = 
  | RegisterPlayer of Name:string
  | RollBall of Probability:float
  | PreciseRoll of Pins:Set<int>
  | RecoverBall


let rec Exec state = function
  | RegisterPlayer n -> match state.phase with
                        | GameFinished -> PlayerRegistered n
                        | _            -> failwith "to register, game must not be started and no player registered."

  | RollBall p ->       state.pinsUp |> dropSomeWithProb p |> PreciseRoll |> Exec state

  | PreciseRoll ps ->   match state.phase with
                        | GameFinished     -> BallStranded
                        | WaitingForStaff  -> failwith "You've already stranded a ball.  Get Out!"
                        | WaitingForPlayer -> let pinsLeft = state.pinsUp - ps
                                              let newScore = ps.Count :: state.rollRecord |> List.rev |> Rules.scoreRolls
                                              match pinsLeft.Count, newScore with
                                              | _, Complete scored when scored.Length >= 10 -> GameOver scored 
                                              | 0, Complete scored                          -> FrameEnded ps
                                              | n, Complete scored                          -> FrameEnded ps
                                              | 0, Incomplete (scored,unscored)             -> FrameEnded ps
                                              | n, Incomplete (scored,unscored)             -> PinsDropped ps

  | RecoverBall ->     match state.phase with
                       | WaitingForStaff -> BallRecovered
                       | _               -> failwith "What ball????"
  
      
open Agg
let BowlingAggregate = {apply = Apply; exec=Exec; zero=State.Zero}

