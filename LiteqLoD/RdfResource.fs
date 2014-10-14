module RdfResource

open VDS.RDF
open VDS.RDF.Parsing
open Schema
open Utils


let loadResource (uri : System.Uri) = 
    let graph = new Graph()
    UriLoader.Load(graph, uri)
    graph

let schemaCache : ICache<ISchemaProvider> = createInMemoryCache()

[<StructuredFormatDisplay("{Uri}")>]
type RdfResource(uri : System.Uri) = 
    class
        let graph = loadResource uri

        member __.Uri = uri
        member __.Graph = graph
        member __.Item 
            with get (uri : System.Uri) = graph.GetTriplesWithPredicate uri |> Seq.map (fun x -> x.Object)
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
    //let furtherRestrictions' = furtherRestrictions |> Seq.pairwise |> Seq.map(fun ((_,p,_),(_,_,t)) -> System.Uri p, System.Uri t)
    let schema =  
        match schemaCache.TryRetrieve inIndex with
        | Some schema -> schema
        | None -> 
            let s = new HTTPSchema() :> ISchemaProvider
            schemaCache.Set (inIndex, s)
            s

    schema.GetTypeClustersFor withType 
    |> List.map (schema.GetAllEQCIn)
    |> List.concat
    |> List.map (schema.GetAllInstancesIn)
    |> List.concat
    |> List.map (fun uri -> RdfResource (System.Uri uri))
    //|> List.filter(fun r -> furtherRestrictions' |> Seq.forall (filterPropertyType r))





//let dummyInstancesFromSchemex : RdfResource list = []
//let dummyRestrictions = ["?yy", "a", "Katze"; "?x", "hatHaustier", "?yy"; "?y", "a", "Hund" ; "?x", "hatAllergie", "?y"; "?x", "a", "Person"]
//
//let gatherInstances (patterns : Pattern list) =
//    let patterns' = List.rev patterns
//    let initial_type = ``object`` patterns'.[0]
//    let schema = (new DummySchema()) :> SchemaProvider
//    let instances =
//        schema.GetTypeClustersFor initial_type
//        |> List.map(fun typeCluster -> schema.GetAllEQCIn typeCluster)
//        |> List.concat
//        |> List.map(fun equivalenceCluster -> schema.GetAllInstancesIn equivalenceCluster)
//        |> List.concat
//        |> List.map(fun uri -> RdfResource (loadResource <| System.Uri uri))
//    let restrictions = 
//        patterns'
//        |> List.tail
//        |> Seq.pairwise
//        |> Seq.map(fun ((_,propertyName,_),(_,_,typeName)) -> System.Uri propertyName, System.Uri typeName)
//    instances
//    |> Seq.filter (fun instance -> restrictions |> Seq.forall (filterPropertyType instance))

