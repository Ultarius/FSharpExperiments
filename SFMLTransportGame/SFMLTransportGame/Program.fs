open Game

open SFML.Window
open SFML.Graphics

[<EntryPoint>]
let main argv = 

    let videoMode = VideoMode(1280u, 720u)
    let titleText = "Transport Game" 

    use window = new RenderWindow(videoMode, titleText)


    do TransportGame.Init window

    0
