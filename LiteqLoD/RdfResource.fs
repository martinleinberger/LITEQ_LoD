module RdfResource

open VDS.RDF
open VDS.RDF.Parsing
open Schema
open Utils
open Microsoft.FSharp.Collections


let loadResource (uri : System.Uri) = 
    let graph = new Graph()
    UriLoader.Load(graph, uri)
    graph

let schemaCache : ICache<ISchemaProvider> = createInMemoryCache()

[<StructuredFormatDisplay("{Uri}")>]
type RdfResource(uri : System.Uri, graph: IGraph) = 
    class
       
        member __.Uri = uri
        member __.Graph = graph
        member __.Item 
            with get (property : System.Uri) =
                    (__.Graph.GetTriplesWithPredicate property)
                    |> Seq.map (fun x -> x.Object)
    end


type CList<'a> = System.Collections.Concurrent.ConcurrentBag<'a>

let createRdfResourceDownloadTask (success : CList<RdfResource>) (failure:CList<string*exn>) (uri : string) = 
    async { 
        try 
            let graph = new Graph()
            let uri' = System.Uri uri
            UriLoader.Load(graph,  uri')
            let resource = RdfResource(uri', graph)
            success.Add resource
        with ex -> failure.Add (uri,ex)
    }
        
type LodList<'a>(sources : string seq, createTask: CList<'a> -> CList<string*exn> -> string -> Async<unit>) = 
    class
        let successfullResources = CList<'a>()
        let failedResources = CList<string*exn>()
        
        let downloadTask = 
            sources
            |> Seq.map (createTask successfullResources failedResources)
            |> Async.Parallel
            |> Async.Ignore
            |> Async.StartAsTask
        
        member __.Results = successfullResources :> seq<'a>
        member __.Failures = failedResources :> seq<string*exn>
        
        member __.Finished = downloadTask.IsCompleted
        member __.IsEmpty = successfullResources.IsEmpty
        member __.Task = downloadTask
    end

let filterPropertyType (resource : RdfResource) (propertyName:System.Uri, typeName:System.Uri) = 
    resource.Graph.GetTriplesWithPredicate propertyName
    |> Seq.map (fun triple -> 
           match triple.Object with
           | :? UriNode -> (loadResource (triple.Object :?> UriNode).Uri).GetTriplesWithPredicate(System.Uri "rdf:type")
           | :? LiteralNode -> failwith "Everythings fucked..."
           | _ -> failwith "Even more fucked")
    |> Seq.concat
    |> Seq.exists (fun triple -> (triple.Object :?> UriNode).Uri.Equals(typeName))

let gatherInstances (inIndex:string) (withType:string) (furtherRestrictions:(string*string*string) list) =
    //printfn "%A" furtherRestrictions
    let furtherRestrictions' = furtherRestrictions |> Seq.pairwise |> Seq.map(fun ((_,p,_),(_,_,t)) ->  p, t)
    //printfn "%A" furtherRestrictions'
    
    let schema =  
        match schemaCache.TryRetrieve inIndex with
        | Some schema -> schema
        | None -> 
            let s = new HTTPSchema() :> ISchemaProvider
            schemaCache.Set (inIndex, s)
            s
    
    let lineTofile filename (x: 'a seq) =
        let line = string (Seq.length x)
        use writer = System.IO.File.CreateText filename
        writer.WriteLine line
        x
   

    let help x = 
        printfn "%A" x
        x

    let r =
        // Hol dir eine Liste von TypeClustern die einen bestimmten Typ besitzen
        schema.GetTypeClustersFor withType 
        // Hol dir die Äquivalenzcluster die zu diesen TypeClustern gehören und mach eine große Liste draus
        |> PSeq.map (schema.GetAllEQCIn)
        |> PSeq.concat
        // Erzeug eine Map die Äquivalenzcluster auf Property/TypeCluster abbildet ( also EQ1 => [ ([foafName, foafGivenName], [TC1, TC2]), ... ]
        |> PSeq.fold(fun (accumulator:Map<string, (string list*string list) list>) equivalenceCluster -> 
            accumulator.Add(equivalenceCluster, schema.GetPropertiesAndTypeClusterIn equivalenceCluster)
        ) Map.empty<string, (string list*string list) list>
        |> Map.fold(fun state equivalenceCluster propertiesAndTypes ->
            // Erzeugt eine Liste von Tupeln bei dem jeweils EIN Property auf EINEN TypeCluster zeigt
            // Also [(foafName, TC1); (foafGivenName, TC1); ...]
            let x = 
                propertiesAndTypes
                |> List.collect(fun (properties,typeClusters) -> 
                    properties
                    |> List.collect(fun property -> 
                        typeClusters 
                        |> List.map(fun typeCluster -> schema.GetAllTypesIn typeCluster)
                        |> List.concat
                        |> List.map(fun ``type`` -> property, ``type``))
                )
            // (carries,WirelessDevice) => EC1
            // (hatHund, Hund) => EC2
            x |> List.fold(fun (state':Map<string*string,string list>) key ->
                if state'.ContainsKey key 
                    then state'.Add (key,  equivalenceCluster:: state'.[key])
                    else state'.Add (key, [equivalenceCluster])
            ) state
        ) Map.empty<string*string,string list>
        |> fun x ->
            if (Seq.length furtherRestrictions') = 0
                then x
                else x |> Map.filter(fun key _ -> furtherRestrictions' |> Seq.exists (fun x' -> x' = key) )
        |> Map.toSeq
        |> Seq.map snd
        |> (lineTofile @"C:\Users\Martin\Downloads\debug.txt")
        |> Seq.concat
        |> Seq.distinct
        |> Seq.map (schema.GetAllInstancesIn)
        |> Seq.concat
        |> Seq.toList
//        |> PSeq.map Set.ofList
//        |> Set.intersectMany    // <<< this is the bad call
//        |> Set.toList

//        |> Map.fold(fun (accumulator:Map<string*string, string list>) equivalenceCluster propertiesAndTypes ->
//            propertiesAndTypes 
//            |> List.fold(fun  (s1:Map<string*string, string list>) (properties,types) ->
//                properties 
//                |> List.fold(fun (s2:Map<string*string, string list>) property ->
//                        types
//                        |> List.fold(fun s3 ``type`` ->
//                            let key = property, ``type``
//                            if s3.ContainsKey key
//                                then s3.Add (key, equivalenceCluster :: s3.[key])
//                                else s3.Add (key, [equivalenceCluster])
//                            ) s2
//                    ) s1
//                ) accumulator
//            ) Map.empty<string*string, string list>


//        |> Map.fold(fun accumulator equivalenceCluster propertiesAndTypes ->
//
//        ) Map.empty<string*string, string>
//       
       
//        |> PSeq.map (schema.GetAllInstancesIn)
//        |> Seq.concat
//        |> PSeq.distinct

//        |> PSeq.map (fun uri -> RdfResource (System.Uri uri))
//        |> PSeq.fold(fun accumulator resource ->
//            try
//                match (furtherRestrictions' |> Seq.forall (filterPropertyType resource)) with
//                | true -> resource :: accumulator
//                | false -> accumulator
//            with
//            | _ -> accumulator) List.empty<RdfResource>
    new LodList<RdfResource>(r, createRdfResourceDownloadTask)