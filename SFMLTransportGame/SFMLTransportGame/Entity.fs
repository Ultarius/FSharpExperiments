module Entity

open SFML.Graphics
open SFML.Window
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


type Message =
    {
        To      : uint64
        From    : uint64
        Message : string
    }

type FactoryData =
    {
        ID          : uint64
        Position    : Vector2f
        RefreshRate : int
        Storage     : int
        MaxStorage  : int
    }

type TruckData =
    {
        ID          : uint64
        Position    : Vector2f
        Capacity    : int
        MaxCapacity : int
        Speed       : float32
        Arrived     : bool
        Loaded      : bool
        Loading     : bool
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
    }

type GameState = 
    {
        Entities    : Entity<EntityData> list
        Messages    : Message list
        Routines    : Coroutine<GameState, Entity<EntityData> list, Message list, Unit> list
        Draw        : Coroutine<GameState, Entity<EntityData> list, RenderWindow, Unit>
    }

    
let FactoryAI (f : FactoryData) (m : Message list) : (EntityData * Message list) =
    let rec messageReader data list prev =
        match list with
            | x::xs when x.To = f.ID && f.Storage > 0 && x.Message = "Request" -> ({data with Storage = data.Storage - 1} |> Factory, (@) [{To = x.From; From = x.To; Message = "Supply"}]  (prev @ xs))
            | x::xs -> messageReader data xs (prev @ [x])
            | [] -> (data |> Factory, m)

    if (f.RefreshRate > 0) then
        messageReader {f with RefreshRate = f.RefreshRate - 1} m []
    else if (f.Storage < f.MaxStorage) then
        messageReader {f with RefreshRate = 150; Storage = f.Storage + 1} m []
    else
        messageReader {f with RefreshRate = 150;} m []

    

let TruckAI (t : TruckData) (m : Message list) : (EntityData * Message list) =
    if t.Arrived && (t.Capacity < t.MaxCapacity) then 
        let rec messageReader list prev =
            match list with
                | x::xs when x.To = t.ID && x.Message = "Supply" -> ({t with Capacity = t.Capacity + 1; Loading = false} |> Truck, (@) prev xs)
                | x::xs -> messageReader xs (prev @ [x])
                | [] when t.Loading = false && t.Capacity < t.MaxCapacity -> ({t with Loading = true} |> Truck, (@) [{To = t.FactoryData.ID; From = t.ID; Message = "Request"}] m)
                | [] -> (t |> Truck, m)
        messageReader m []
    elif t.Capacity >= t.MaxCapacity && t.Position.X > -75.0f then
        ({t with Position = new Vector2f(t.Position.X - t.Speed, t.Position.Y)} |> Truck, m)
    elif t.Capacity = 0 && t.FactoryData.Position.X > t.Position.X then
        ({t with Position = new Vector2f(t.Position.X + t.Speed, t.Position.Y)} |> Truck, m)
    elif t.Arrived = false && t.Position.X >= t.FactoryData.Position.X then
        ({t with Arrived = true} |> Truck, m)
    else
        ({t with Capacity = 0; Arrived = false} |> Truck, m)
    
 



let MainLoop : Coroutine<GameState, Entity<EntityData> list, Message list, Unit> =
                                                         fun gameState entities messages ->   
                                                                        //let newEntities, newMessages = c |> List.fold(fun (entities, messages) entity -> match entity.Data with | _ -> (entity::, b)) (c, b)
                                                                        //let flod = c |> List.fold(fun (newMessage, newEntities, messages) entity -> List.fold (fun (newMessages, newEntities, entity) msg -> (newMessages, newEntities, entity)) (newMessages, newEntities, entity) messages) ([], [], b) c
                                                                        
                                                                        let entities', mailbox' = List.fold (fun (_entities, _mailbox) (entity : Entity<EntityData>) -> match entity.Data with
                                                                                                                                                                        | Truck x -> let truckData, truckMessages = TruckAI x _mailbox
                                                                                                                                                                                     ({entity with Data = truckData} :: _entities), truckMessages
                                                                                                                                                                        | Factory x -> let factoryData, factoryMessages = FactoryAI x _mailbox
                                                                                                                                                                                       ({entity with Data = factoryData} :: _entities), factoryMessages
                                                                                                                                                                        | Road x -> ({entity with Data = (x |> Road)} :: _entities), _mailbox ) ([], messages) entities

                                                                        Done(List.rev entities', mailbox', ())

let ikeaTexture : Texture = new Texture(@"resources\ikea.png");
let productBoxTexture : Texture = new Texture(@"resources\product_box.png");
let truck : Texture = new Texture(@"resources\volvo.png");

let Renderer : Coroutine<GameState, Entity<EntityData> list, RenderWindow, Unit> = 
               fun x b (c : RenderWindow) ->  let newE = b |> List.iter(fun e -> match e.Data with
                                                                                    | Factory x -> c.Draw(new RectangleShape(new Vector2f(95.0f, 80.0f), Position = new Vector2f(x.Position.X, x.Position.Y), FillColor=Color.White, Texture = ikeaTexture))
                                                                                                   for i in 1 .. x.Storage do
                                                                                                        c.Draw(new RectangleShape(new Vector2f(25.0f, 25.0f), Position = new Vector2f(x.Position.X + 100.f, x.Position.Y - float32(-90 + i * 30)), FillColor = Color.White, Texture = productBoxTexture))
                                                                                    | Truck x -> c.Draw(new RectangleShape(new Vector2f(80.0f, 40.0f), Position = new Vector2f(x.Position.X, x.Position.Y), FillColor=Color.White, Texture = truck))
                                                                                    | Road x -> c.Draw(new RectangleShape(new Vector2f(50.0f, 25.0f), Position = new Vector2f(x.Position.X, x.Position.Y), FillColor=Color.Black))
                                                                                    | _ -> ())
                                              Done(b, c, ())
                                                                                             

let addRoad amount (y : float32) : Entity<EntityData> list =
    [
        for i = 0 to amount do
          yield
            {
                Data = {Position = new Vector2f(float32(i * 50), y);} |> Road;
            }
     ]