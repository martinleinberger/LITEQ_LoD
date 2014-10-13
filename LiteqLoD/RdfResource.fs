module RdfResource

open System
open VDS.RDF
open VDS.RDF.Parsing
open Schema

type Pattern = string*string*string

let loadResource (uri : System.Uri) = 
    let graph = new Graph()
    UriLoader.Load(graph, uri)
    graph


type RdfResource(graph : Graph) = 
    class
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

let dummyInstancesFromSchemex : RdfResource list = []
let dummyRestrictions = ["?yy", "a", "Katze"; "?x", "hatHaustier", "?yy"; "?y", "a", "Hund" ; "?x", "hatAllergie", "?y"; "?x", "a", "Person"]

let gatherInstances (patterns : Pattern list) =
    let patterns' = List.rev patterns
    let initial_type = ``object`` patterns'.[0]
    let schema = (new DummySchema()) :> SchemaProvider
    let instances =
        schema.GetTypeClustersFor initial_type
        |> List.map(fun typeCluster -> schema.GetAllEQCIn typeCluster)
        |> List.concat
        |> List.map(fun equivalenceCluster -> schema.GetAllInstancesIn equivalenceCluster)
        |> List.concat
        |> List.map(fun uri -> RdfResource (loadResource <| System.Uri uri))
    let restrictions = 
        patterns'
        |> List.tail
        |> Seq.pairwise
        |> Seq.map(fun ((_,propertyName,_),(_,_,typeName)) -> System.Uri propertyName, System.Uri typeName)
    instances
    |> Seq.filter (fun instance -> restrictions |> Seq.forall (filterPropertyType instance))

