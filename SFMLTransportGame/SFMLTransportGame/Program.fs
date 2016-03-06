#if INTERACTIVE
#r "/path/to/AZROLESLib.dll"
#endif

open Game

[<EntryPoint>]
let main argv = 

    do TransportGame.Init

    0
