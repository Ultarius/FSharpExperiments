open Game

open System
open SFML.Window
open SFML.Graphics

[<EntryPoint>]
let main argv = 

    let videoMode = VideoMode(1280u, 720u)
    let titleText = "Transport Game" 

    use window = new RenderWindow(videoMode, titleText)
    
    window.SetFramerateLimit(60ul);
    
    window.Closed.AddHandler( fun s a -> Environment.Exit 0 ) 

    do TransportGame.Init window

    0
