namespace Bla

module Bla =
    
    [<ReflectedDefinition>]
    let sum (a : int) = a + 1

module Main =

    [<EntryPoint>]
    let main argv = 
        printfn "%A" (Bla.sum 1)
        0 // return an integer exit code
