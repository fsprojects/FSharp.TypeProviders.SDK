module Program 
#if !NETSTANDARD
let [<EntryPoint>] main _ = 0
#else
let main _ = 0
#endif
