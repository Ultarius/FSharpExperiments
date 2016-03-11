//open Game

open System
open SFML.Window
open SFML.Graphics

open SFML.System
open Entity

[<EntryPoint>]
let main argv = 

    let videoMode = VideoMode(1280u, 720u)
    let titleText = "Transport Game" 

    use window = new RenderWindow(videoMode, titleText)
    
    //window.SetFramerateLimit(60ul);
    
    window.SetVerticalSyncEnabled(true)

    window.Closed.AddHandler( fun s a -> Environment.Exit 0 ) 

    let factoryData : FactoryData = {FactoryData.ID = 1UL; Position = new Vector2f(600.0f, 150.0f); RefreshRate = 150; Storage = 0; MaxStorage = 5;}
    let factory = {Data = factoryData |> EntityData.Factory;}

    let factoryData2 : FactoryData = {FactoryData.ID = 2UL; Position = new Vector2f(600.0f, 400.0f); RefreshRate = 150; Storage = 0; MaxStorage = 5;}
    let factory2 = {Data = factoryData2 |> EntityData.Factory;}

    let truck = {Data = {ID = 3UL; Position = new Vector2f(0.0f, 188.0f); Speed = 2.5f; Capacity = 0; MaxCapacity = 2; Arrived = false; Loaded = false; Loading = false; FactoryData = factoryData;} |> EntityData.Truck;}
        
    let truck2 = {Data = {ID = 4UL; Position = new Vector2f(0.0f, 438.0f); Speed = 4.5f; Capacity = 0; MaxCapacity = 1; Arrived = false; Loaded = false; Loading = false; FactoryData = factoryData2;} |> EntityData.Truck;}
       
    let roads = (@) (addRoad 11 455.0f) (addRoad 11 205.0f)

    let rec GameLoop state = 
        window.DispatchEvents()
        window.Clear()
        window.Draw(new RectangleShape(new Vector2f(1280.0f, 720.0f), FillColor=Color.Green))

        let newEntities, newMessages, c = co_step (state.Routines.Head state state.Entities state.Messages )

        let _, _, corout = co_step (state.Draw state newEntities window)

        let newState = {state with Entities = newEntities
                                   Messages = newMessages
                        }

        window.Display()
        GameLoop newState
            
    do GameLoop {Entities = (@) roads [factory; factory2; truck; truck2 ]; Messages = []; Routines = [MainLoop]; Draw = Renderer}


    //do TransportGame.Init window

    0
