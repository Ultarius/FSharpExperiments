module Entity

open SFML.Graphics
open SFML.System

type Coroutine<'w, 'so, 'si, 'a> = 'w -> 'so -> 'si -> Result<'w, 'so, 'si, 'a>
and Result<'w, 'so, 'si, 'a> = 
    | Done of 'so * 'si * 'a
    | Busy of 'so * 'si * Coroutine<'w, 'so, 'si, 'a>

let ret (x:'a) : Coroutine<'w, 'so, 'si, 'a> =
    fun w so si -> Done(so,si,x)

let rec (>>=) (p:Coroutine<'w, 'so, 'si, 'a>) (k:'a->Coroutine<'w, 'so, 'si, 'b>) : Coroutine<'w, 'so, 'si, 'b> =
    fun w so si ->
        match p w so si with
        | Done (so', si', a)    -> k a w so' si'
        | Busy (so', si', p')   -> Busy (so', si', p' >>= k)

type CoroutineBuilder() =
    member this.Return x = ret x
    member this.Bind(p,k) = p >>= k

let co = CoroutineBuilder()


let co_step result =
    match result with
    | Done(so, si, res) -> so, si, co{return res}
    | Busy(so, si, c') -> so, si, c'

type Behaviour<'d> = Behaviour of ('d -> 'd * Behaviour<'d>)



type Message =
    {
        To : int
        From : int
        Message : string
    }

type FactoryData =
    {
        Position : Vector2f
        RefreshRate : int
        Storage : int
        MaxStorage : int
    }

type TruckData =
    {
        Position : Vector2f
        Capacity : int
        MaxCapacity : int
        Speed : float32
        Arrived : bool
        Loaded : bool
        FactoryData : FactoryData
    }
    

type RoadData =
    {
        Position : Vector2f
    }

type EntityData = 
    | Factory of FactoryData
    | Truck of TruckData
    | Road of RoadData


type Entity<'d> = 
    {
        Data        : 'd
        Render      : 'd -> RectangleShape
    }

type GameState = 
    {
        Entities  : Entity<EntityData> list
        Messages  : Message list
        Routines  : Coroutine<GameState, Message list, Entity<EntityData> list, Unit> list
    }

let rec repeatgg c x =
    match c with
    | Behaviour(cfunc) ->   let x', c' = cfunc x 
                            x'


let rec repeat c (x : EntityData) (m : Message list) =
    match c with
    | Done(so',si',x) ->   let x', c' = so' x m
                           x' 
    | Busy (so, si, p') -> si


let rec AIBehaviour = Behaviour(fun (x : EntityData ) -> match x with
                                                                | Truck x -> (if x.Position.X < -50.0f && x.Loaded then {x with Capacity = 0; Loaded = false; Arrived = false;} |> Truck elif ( x.Loaded) then {x with Position = new Vector2f(x.Position.X - (float32)x.Speed, x.Position.Y)} |> Truck else (if x.Position.X < x.FactoryData.Position.X && x.Arrived = false then {x with Position = new Vector2f(x.Position.X + (float32)x.Speed, x.Position.Y)} |> Truck else {x with Arrived = true;} |> Truck)), AIBehaviour
                                                                | _ -> (x, AIBehaviour))
                                                                    
let rec PickupBehaviour = Behaviour(fun (x : EntityData ) -> match x with
                                                                | Truck x -> (if (x.Arrived) then (if(x.Capacity < x.MaxCapacity) then {x with Capacity = x.Capacity + 1} |> Truck else {x with Loaded = true} |> Truck ) else x |> Truck ), PickupBehaviour
                                                                | _ -> (x, PickupBehaviour))  
                                                                    
let rec Drawable = Behaviour(fun (x : EntityData ) -> match x with
                                                                | Truck x ->  (x |> Truck, Drawable)
                                                                | Factory x ->  (x |> Factory, Drawable)
                                                                | _ -> (x, Drawable))

let TruckArrived (t : TruckData) (m : Message list) : (EntityData * Message list) =
    if t.Arrived then 
        let newTruck = {t with Arrived = false}
        let message = {To = 0; From = 1; Message = "Arrived"}
        (newTruck |> Truck, (@) [message] m)
    else
        (t |> Truck, m)
    
let TruckAI (t : TruckData) (m : Message list) : (EntityData * Message list) =
    if t.Arrived && (t.Capacity < t.MaxCapacity) then 
        let rec messageReader list =
            match list with
                | [] when t.Capacity < t.MaxCapacity -> ({t with Capacity = t.Capacity + 1} |> Truck, (@) [{To = 0; From = 1; Message = "Request"}] m)
                | [] -> (t |> Truck, m)
                | x::xs when x.Message = "Supply" -> ({t with Capacity = t.Capacity + 1} |> Truck, m)
                | x::xs -> messageReader xs
        messageReader m
    elif t.Capacity >= t.MaxCapacity && t.Position.X > -75.0f then
        ({t with Position = new Vector2f(t.Position.X - t.Speed, t.Position.Y)} |> Truck, m)
    elif t.Capacity = 0 && t.FactoryData.Position.X > t.Position.X then
        ({t with Position = new Vector2f(t.Position.X + t.Speed, t.Position.Y)} |> Truck, m)
    elif t.Arrived = false && t.Position.X >= t.FactoryData.Position.X then
        ({t with Arrived = true} |> Truck, m)
    else
        ({t with Capacity = 0; Arrived = false} |> Truck, m)
    
 
(*
                                           
let rec PickupEvent = Queue(fun (x : EntityData ) (messages : Message list) -> match x with
                                                                                | Truck x -> (TruckArrived x messages), PickupEvent
                                                                                | Factory x ->  ((x |> Factory), messages), PickupEvent
                                                                                | _ -> (x, messages), PickupEvent)    
                                           
let rec AI = Queue(fun (x : EntityData ) (messages : Message list) -> match x with
                                                                                | Truck x -> (TruckAI x messages), AI
                                                                                | Factory x ->  ((x |> Factory), messages), AI
                                                                                | _ -> (x, messages), AI)

*)

//let coAI : Coroutine<GameState, Message list,'d, Unit> = fun x b c -> match c b x with | Done(so', d', k') -> let x', c' = so' x b 


let MainLoop : Coroutine<GameState, Message list, Entity<EntityData> list, Unit> =
                                                         fun x b c ->   
                                                                        //let newEntities, newMessages = c |> List.fold(fun (entities, messages) entity -> match entity.Data with | _ -> (entity::, b)) (c, b)
                                                                        //let flod = c |> List.fold(fun (newMessage, newEntities, messages) entity -> List.fold (fun (newMessages, newEntities, entity) msg -> (newMessages, newEntities, entity)) (newMessages, newEntities, entity) messages) ([], [], b) c
                                                                        let newE = c |> List.map(fun e -> match e.Data with
                                                                                                                    | Factory x -> e
                                                                                                                    | Truck x -> let truckData, messages = TruckAI x b
                                                                                                                                 {e with Data = truckData }
                                                                                                                    | _ -> e)
                                                                        Done(b, newE, ())
                                                                                             

let addRoad amount (y : float32) : Entity<EntityData> list =
    [
        for i = 0 to amount do
          yield
            {
                Data = {Position = new Vector2f(float32(i * 50), y);} |> Road;
                Render = fun (Road x) -> 
                    new RectangleShape(new Vector2f(50.0f, 25.0f), Position = new Vector2f(x.Position.X, x.Position.Y), FillColor=Color.Black)
            }
     ]