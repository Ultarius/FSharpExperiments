module Game

open SFML.Graphics
open SFML.Window
open SFML.System

open Entity


type Game = {
    Init : RenderWindow -> unit
    Run : GameState -> RenderWindow -> unit
}



let rec TransportGame : Game = 
    {   
        Init = fun (window : RenderWindow ) ->
            let factoryData : FactoryData = {FactoryData.Position = new Vector2f(600.0f, 50.0f); RefreshRate = 10; Storage = 0; MaxStorage = 10;}
            let factory = {Data = factoryData |> EntityData.Factory; Render = fun (Factory x) -> new RectangleShape(new Vector2f(95.0f, 80.0f), Position = new Vector2f(x.Position.X, x.Position.Y), FillColor=Color.Red)}

            let factoryData2 : FactoryData = {FactoryData.Position = new Vector2f(600.0f, 300.0f); RefreshRate = 10; Storage = 0; MaxStorage = 10;}
            let factory2 = {Data = factoryData2 |> EntityData.Factory; Render = fun (Factory x) -> new RectangleShape(new Vector2f(95.0f, 80.0f), Position = new Vector2f(x.Position.X, x.Position.Y), FillColor=Color.Red)}

            let truck = {Data = {Position = new Vector2f(0.0f, 107.0f); Speed = 2.5f; Capacity = 0; MaxCapacity = 300; Arrived = false; Loaded = false; FactoryData = factoryData;} |> EntityData.Truck; Render = fun (Truck x) -> new RectangleShape(new Vector2f(40.0f, 20.0f), Position = new Vector2f(x.Position.X, x.Position.Y), FillColor=Color.White)}
        
            let truck2 = {Data = {Position = new Vector2f(0.0f, 357.0f); Speed = 4.5f; Capacity = 0; MaxCapacity = 300; Arrived = false; Loaded = false; FactoryData = factoryData2;} |> EntityData.Truck; Render = fun (Truck x) -> new RectangleShape(new Vector2f(40.0f, 20.0f), Position = new Vector2f(x.Position.X, x.Position.Y), FillColor=Color.White)}
       
            let roads = (@) (addRoad 11 355.0f) (addRoad 11 105.0f)

            do TransportGame.Run {Entities = (@) roads [truck; truck2; factory; factory2]; Messages = []; Routines = [MainLoop]} window

        Run = fun (state : GameState) (window : RenderWindow) ->
            window.DispatchEvents()
            window.Clear()
            window.Draw(new RectangleShape(new Vector2f(1280.0f, 720.0f), FillColor=Color.Green))

            state.Entities |> List.iter(fun e -> window.Draw(e.Render e.Data))
            
            let a, b, c = co_step (state.Routines.Head state state.Messages state.Entities)

            //let n, m = repeat state.Entities.[0].Behaviours.[0] state.Entities.[0].Data state.Messages
            
            //let newEntities, newMessages = state.Entities |> List.map(fun entiteit -> {entiteit with Data = List.map (fun s comp messages -> repeat comp s messages) entiteit.Data entiteit.Behaviours state.Messages })

            let newState = {state with Entities = b
                                       Messages = a
                           }

            //let newState = {state with Entities = state.Entities 
            //                        |> List.map(fun entiteit -> {entiteit with Data = List.fold (fun (s : EntityData) comp messages -> repeat comp s messages) entiteit.Data entiteit.Behaviours state.Messages})}

            window.Display()
            do TransportGame.Run newState window

    }