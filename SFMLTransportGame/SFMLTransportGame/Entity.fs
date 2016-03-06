module Entity

open SFML.Graphics
open SFML.System


type Behaviour<'d> = Behaviour of ('d -> 'd * Behaviour<'d>)

type Entity<'d> = 
    {
        Data        : 'd
        Behaviours  : Behaviour<'d> list 
        Render      : 'd -> RectangleShape
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

let rec repeat c x =
    match c with
    | Behaviour(cfunc) ->   let x', c' = cfunc x 
                            //printfn "%A" x'
                            x'


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

let addRoad amount (y : float32) : Entity<EntityData> list =
    [
        for i = 0 to amount do
          yield
            {
                Data = {Position = new Vector2f(float32(i * 50), y);} |> Road;
                Behaviours = [];
                Render = fun (Road x) -> 
                    new RectangleShape(new Vector2f(50.0f, 25.0f), Position = new Vector2f(x.Position.X, x.Position.Y), FillColor=Color.Black)
            }
     ]