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
type RdfResource(uri : System.Uri) = 
    class
        
        let mutable graph : Graph option = None

        member __.Uri = uri
        member __.Graph = 
            match graph with 
            | Some g -> g
            | None -> 
                graph <- Some (loadResource __.Uri)
                graph.Value
        member __.Item 
            with get (property : System.Uri) =
                    (__.Graph.GetTriplesWithPredicate property)
                    |> Seq.map (fun x -> x.Object)
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
    printfn "%A" furtherRestrictions
    let furtherRestrictions' = furtherRestrictions |> Seq.pairwise |> Seq.map(fun ((_,p,_),(_,_,t)) -> System.Uri p, System.Uri t)
    let schema =  
        match schemaCache.TryRetrieve inIndex with
        | Some schema -> schema
        | None -> 
            let s = new HTTPSchema() :> ISchemaProvider
            schemaCache.Set (inIndex, s)
            s

    schema.GetTypeClustersFor withType 
    |> PSeq.map (schema.GetAllEQCIn)
    |> Seq.concat
    |> PSeq.map (schema.GetAllInstancesIn)
    |> Seq.concat
    |> PSeq.distinct
    |> PSeq.map (fun uri -> RdfResource (System.Uri uri))
    |> PSeq.fold(fun accumulator resource ->
        try
            match (furtherRestrictions' |> Seq.forall (filterPropertyType resource)) with
            | true -> resource :: accumulator
            | false -> accumulator
        with
        | _ -> accumulator) List.empty<RdfResource>