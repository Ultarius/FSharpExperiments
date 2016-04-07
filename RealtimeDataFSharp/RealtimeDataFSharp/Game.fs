module Game

open System.Collections.Generic
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

open Stations


let easeInOutQuad2 (currentTime : float32) (startPos : float32) (endPos : float32) (duration : float32) =   let newTime = currentTime / (duration / 2.0f)
                                                                                                            if ( newTime < 1.0f ) then
                                                                                                                startPos + newTime * newTime * endPos / 2.0f
                                                                                                                //let b = startPos + (endPos / 2.0f * newTime * newTime)
                                                                                                                //b
                                                                                                            else 
                                                                                                                let newTime' = newTime - 1.0f
                                                                                                                //startPos + (newTime' * (newTime' - 2.0f) - 1.0f) * endPos / 2.0f
                                                                                                                startPos + (-endPos / 2.0f * (newTime' * (newTime' - 2.0f) - 1.0f))
                                                                                                                //-endPos / 2.0f * ( newTime' * ( newTime' - 2.0f) - 1.0f ) + startPos
//
//  t /= d/2;
//	if (t < 1) return c/2*t*t + b;
//	t--;
//	return -c/2 * (t*(t-2) - 1) + b;

//QuadEaseInOut( double t, double b, double c, double d )
//        {
//            if ( ( t /= d / 2 ) < 1 )
//                return c / 2 * t * t + b;
//
//            return -c / 2 * ( ( --t ) * ( t - 2 ) - 1 ) + b;

type Line = A | B | C | D | E

type Station = {
    Name : string
    Arrival : float32
    Next : Option<Station>
    Position : Vector2
}

type TrainStatus = 
    | Waiting of float32
    | Moving of float32
    | Arrived

type Train = {
    Line : Line
    Station : Station
    Position : Vector2
    Status : TrainStatus
} with member x.Update (dt : GameTime) =
        match x.Status with
        | Moving time ->    match x.Station.Next with
                            | Some nextStation ->   let duration = nextStation.Arrival - x.Station.Arrival
                                                    if (time < duration) then
                                                        let newTime = (time + ((float32)dt.ElapsedGameTime.Milliseconds / 1000.0f))
                                                        let disX = (nextStation.Position.X - x.Station.Position.X)
                                                        let disY = (nextStation.Position.Y - x.Station.Position.Y)
                                                        let newPosX = if x.Station.Position.X <> nextStation.Position.X then easeInOutQuad2 time x.Station.Position.X disX duration else nextStation.Position.X
                                                        let newPosY = if x.Station.Position.Y <> nextStation.Position.Y then easeInOutQuad2 time x.Station.Position.Y disY duration else nextStation.Position.Y
                                                        {x with Position = new Vector2(newPosX, newPosY); Status = Moving newTime}
                                                    else
                                                        {x with Station = nextStation; Status = Waiting 4.0f }
                            | None -> x
        | Waiting wait ->   if (wait < 0.0f) then
                                match x.Station.Next with
                                | Some nextStation ->   {x with Status = Moving (0.0f)}

                                | None -> {x with Status = Arrived}
                            else
                                {x with Status = Waiting (wait - ((float32)dt.ElapsedGameTime.Milliseconds / 1000.0f)) }
        | Arrived -> x



type Coroutine<'a, 's> = 's -> CoroutineStep<'a, 's>
    and CoroutineStep<'a, 's> = 
    |   Done of 'a * 's
    |   Wait of Coroutine<'a, 's> * 's

let rec (>>) p k =
    fun s ->
        match p s with
        | Done(a, s') -> k a s'
        | Wait(leftOver, s') -> Wait((leftOver >> k), s')


type CoroutineBuilder() = 
    member this.Return(x) = (fun s -> Done(x, s))
    member this.Bind(p, k) = p >> k


let co = CoroutineBuilder()



let getState = fun s -> Done(s, s)

type Metro = {
    Line : Line
    Station : Station
    Position : Vector2
    Status : TrainStatus
    Behaviour : GameTime -> Coroutine<Unit, Metro> 
} with static member Update (dt : GameTime) =
        co{ 
            let! metro = getState
            do! metro.Behaviour dt
            //let! newMetro = fun (s : Metro) -> Done((), {s with Position = new Vector2(s.Position.X + 1.0f, 0.0f)})
            //return newMetro
        }


(*
let! metro = getstate
do! metro.update
*)

let MetroProgram() (dt : GameTime) : Coroutine<Unit, Metro> = 
    fun x ->
        match x.Status with
        | Moving time ->    match x.Station.Next with
                            | Some nextStation ->   let duration = nextStation.Arrival - x.Station.Arrival
                                                    if (time < duration) then
                                                        let newTime = (time + ((float32)dt.ElapsedGameTime.Milliseconds / 1000.0f))
                                                        let disX = (nextStation.Position.X - x.Station.Position.X)
                                                        let disY = (nextStation.Position.Y - x.Station.Position.Y)
                                                        let newPosX = if x.Station.Position.X <> nextStation.Position.X then easeInOutQuad2 time x.Station.Position.X disX duration else nextStation.Position.X
                                                        let newPosY = if x.Station.Position.Y <> nextStation.Position.Y then easeInOutQuad2 time x.Station.Position.Y disY duration else nextStation.Position.Y
                                                        Done((), {x with Position = new Vector2(newPosX, newPosY); Status = Moving newTime})
                                                    else
                                                        Done((), {x with Station = nextStation; Status = Waiting 4.0f })
                            | None -> Done((), x)
        | Waiting wait ->   if (wait < 0.0f) then
                                match x.Station.Next with
                                | Some nextStation ->   Done((), {x with Status = Moving (0.0f)})

                                | None -> Done((), {x with Status = Arrived})
                            else
                                Done((), {x with Status = Waiting (wait - ((float32)dt.ElapsedGameTime.Milliseconds / 1000.0f)) })
        | Arrived -> Done((), x)

type GameState = {
    Metro : Metro
}

//let connectStation (s1 : Station) (s2 : Station) =
//    {s2 with Next = Some(s1)}
//
//let (<<>>) = connectStation

let oostplein =  {Name = "Oostplein"; Next = None; Arrival = 25.0f; Position = new Vector2(50.0f, 300.0f)}
let blaak = {Name = "Blaak"; Next = None; Arrival = 20.0f; Position = new Vector2(350.0f, 200.0f)}
let beurs = {Name = "Beurs"; Next = None; Arrival = 12.0f; Position = new Vector2(350.0f, 0.0f)}
let eendrachtsplein = {Name = "Eendrachtsplein"; Next = None; Arrival = 9.0f; Position = new Vector2(250.0f, 0.0f)}
let dijkzigt = {Name = "Dijkzigt"; Next = None; Arrival = 0.0f; Position = new Vector2(300.0f, 300.0f) }

//let track = station <<>> station0 <<>> station1 <<>> station2 <<>> station3


let combinedTrack = [blaak; beurs; eendrachtsplein; dijkzigt] |> List.fold (fun next start -> {start with Next = Some(next)}) oostplein

//let easeInOutQuad (time : float32) = if time < 0.5f then 2.0f * time * time else -1.0f + ( 4.0f - 2.0f * time) * time 

//let stops = Stations.GetAllStationStopRecords ()
//let ritten = Ritten.ritten ()

//let metroStops = Stations.FilterMetroStops stops ritten
//let metroStations = Stations.ConnvertStopsToStations metroStops



printfn "%A" combinedTrack


let rec costep coroutine state =
  match coroutine state with
  | Done(_, newState) ->  (fun s -> Done((), s)), newState
  | Wait(c', s') -> costep c' s'


type TrainSimulation() as this =
    inherit Game()
 
    do this.Content.RootDirectory <- "Content"

    let graphics = new GraphicsDeviceManager(this)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let mutable texture = Unchecked.defaultof<Texture2D>

    let mutable GameState = {   
        Metro = Unchecked.defaultof<Metro>
    }


    override x.Initialize() =
        do base.Initialize()

    override this.LoadContent() =
        do spriteBatch <- new SpriteBatch(this.GraphicsDevice)

        //let parkImage = spriteLoader "parking.png" this.GraphicsDevice

        //movableThingsImages <- [carImage; boatImage];

        texture <- new Texture2D(this.GraphicsDevice, 1, 1)
        texture.SetData([| Color.White |])


        //let newTrain = {Line = A; Station = combinedTrack; Position = combinedTrack.Position; Status = TrainStatus.Waiting 0.0f}

        let newMetro = {Line = A; Station = combinedTrack; Position = combinedTrack.Position; Status = TrainStatus.Waiting 0.0f; Behaviour = MetroProgram()}

        GameState <- {GameState with Metro = newMetro}
        
        //let a = {ID = 0; Components = [Position(10.0f, 10.0f)];}

        

        (*GameState <-{ GameState with 
                        Entities = [a; bouncyBall]
                        Systems = [OtherSystem; LogicSystem]
                    }
                    *)
        //State <- MainUpdate State


        ()
 
    override this.Update (gameTime) =

        (* let rec worldLoop (systems : ISystem list) world = // Find out a better solution to this. =D
            match systems with
            | [] -> world
            | x::xs -> worldLoop xs (x.Update world); *)

(*
        let newWorld = GameState.Systems |> List.fold (fun s system -> system.Update s) GameState

        GameState <- newWorld

        //let newWorld = GameState.Systems |> List.map(fun s -> s.Update s GameState) |> Seq.head

        //GameState <- newWorld
        
        *)

        let leftCo, newTrain = costep (Metro.Update gameTime) GameState.Metro


        GameState <- {GameState with Metro = newTrain}
        ()
        
    override this.Draw (gameTime) =
        do this.GraphicsDevice.Clear Color.CornflowerBlue

        //spriteBatch.Begin(SpriteSortMode.Deferred, BlendState.NonPremultiplied)
        
        spriteBatch.Begin(SpriteSortMode.Immediate, BlendState.AlphaBlend);

        spriteBatch.Draw(texture, new Rectangle((int)GameState.Metro.Position.X, (int)GameState.Metro.Position.Y, 15, 15), Color.Red); 

        spriteBatch.End()      
