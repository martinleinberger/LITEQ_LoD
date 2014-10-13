module Utils

open System.Collections.Generic

type ICache<'T> =
    abstract TryRetrieve : string -> 'T option
    abstract Set : string * 'T -> unit

let createInMemoryCache () =
    let dict = new Dictionary<_, _>()
    { new ICache<_> with
        member __.Set(key, value) =
            lock dict <| fun () -> dict.[key] <- value
        member __.TryRetrieve(key) =
            lock dict <| fun () ->
                match dict.TryGetValue(key) with
                    | true, value -> Some value 
                    | _ -> None }

let typeCache = 
    let s = new HashSet<string>()
    if System.IO.File.Exists("./cache.txt")
        then System.IO.File.ReadAllLines "./cache.txt" |> Seq.iter (fun t -> s.Add t |> ignore)
    s
             
let addType (line:string) = 
    if typeCache.Contains line |> not then
        typeCache.Add line |> ignore
        use wr = new System.IO.StreamWriter("./cache.txt",true)
        wr.WriteLine(line)

let memoize f = 
    let dict = new System.Collections.Generic.Dictionary<_,_>()
    fun n -> 
        match dict.TryGetValue n with
        | (true,v) -> v
        | _ -> 
            let temp = f n
            dict.Add (n,temp)
            temp
