module RdfResource

open System
open VDS.RDF
open VDS.RDF.Parsing

let loadResource (uri : Uri) = 
    let graph = new Graph()
    UriLoader.Load(graph, uri)
    graph


type RdfResource(graph : Graph) = 
    class
        member __.Graph = graph
        member __.Item 
            with get (uri : Uri) = graph.GetTriplesWithPredicate uri |> Seq.map (fun x -> x.Object)
    end

let filterPropertyType (resource : RdfResource) (propertyName:Uri,typeName:Uri) = 
    resource.Graph.GetTriplesWithPredicate propertyName
    |> Seq.map (fun triple -> 
           match triple.Object with
           | :? UriNode -> (loadResource (triple.Object :?> UriNode).Uri).GetTriplesWithPredicate(Uri "rdf:type")
           | :? LiteralNode -> failwith "Everythings fucked..."
           | _ -> failwith "Even more fucked")
    |> Seq.concat
    |> Seq.exists (fun triple -> (triple.Object :?> UriNode).Uri.Equals(typeName))

let dummyInstancesFromSchemex : RdfResource list = []
let dummyRestrictions = ["?yy", "a", "Katze"; "?x", "hatHaustier", "?yy"; "?y", "a", "Hund" ; "?x", "hatAllergie", "?y"; "?x", "a", "Person"]


let test = 
    let propsAndTypes =
        dummyRestrictions
        |> List.rev
        |> List.tail
        |> Seq.pairwise
        |> Seq.map(fun ((_,propertyName,_),(_,_,typeName)) -> Uri propertyName, Uri typeName)
    dummyInstancesFromSchemex
    |> Seq.filter (fun instance -> propsAndTypes |> Seq.forall (filterPropertyType instance) )


