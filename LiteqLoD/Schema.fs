

module Schema

open FSharp.Data

type Uri = string
type Property = Uri
type Type = Uri
type Triple = Uri*Uri*Uri
type TypeCluster = Uri
type EquivalenceCluster = Uri

exception HTTPSchemaException of string 

let db = [
    "tc1", "containsClass", "Person";
    "tc1", "hasEqc", "eq1";
    "tc1", "hasEqc", "eq2"

    "tc2", "containsClass", "Hund";
    "tc2", "containsClass", "Katze";

    "tc3", "containsClass", "Hund";
    "tc3", "containsClass", "Katze";
    "tc3", "containsClass", "Hundehaarallergie";    

    "tc4", "containsClass", "Hund";

    "eq1", "hasPC", "pc1";
    "eq1", "hasPC", "pc2";
    "eq1", "hasDS", "ds1"

    "eq2", "hasPC", "pc3"

    "pc1", "hasRange", "tc2";
    "pc1", "hasProp", "hatHaustier";
    
    "pc2", "hasRange", "tc3";
    "pc2", "hasProp", "hatHaustier";
    "pc2", "hasProp", "hatAllergie";

    "pc3", "hasRange", "tc4";
    "pc3", "hasProp", "hatHaustier";
    
    "ds1", "context", "www.drunkenbasketballplayers.com";
    "ds1", "hasInstance", "player1";
    "ds1", "hasInstance", "player2"
]

let subject (s,_,_)    = s
let predicate (_,p,_)  = p
let ``object`` (_,_,o) = o

let trimUri (x:string) :string = x.Remove(0,1).Remove(x.LastIndexOf(">")-1 ,1)
let trimUris (x:seq<string>) :seq<string> = x |> Seq.map (fun x -> trimUri x)
let escapeUri (x:string) :string = System.Uri.EscapeDataString x
let escapeUris (x:seq<string>) :seq<string> = x |> Seq.map (fun x -> escapeUri x)

type SchemaProvider = 
    abstract member AllRdfTypes : Type list
    abstract member GetAllTypesIn : TypeCluster -> Type list
    abstract member GetTypeClustersFor : Type -> TypeCluster list
    abstract member GetAllEQCIn : TypeCluster -> EquivalenceCluster list
    //abstract member GetAllPropertiesIn: EquivalenceCluster -> Property list
    abstract member GetPropertiesAndTypeClusterIn : EquivalenceCluster -> (Property list * TypeCluster list) list
    
    // Instance related stuff
    abstract member GetAllInstancesIn : EquivalenceCluster -> Uri list


type StartingPointProvider = 
    abstract member Get : unit -> Type list

type HTTPSchema() =
    interface SchemaProvider with
        member __.AllRdfTypes = 
            db
            |> List.filter(fun (s,p,o) -> p = "containsClass")
            |> List.map ``object``
            
        member __.GetAllTypesIn typecluster =
            try
                let uri = "http://webschemex2.west.uni-koblenz.de/lookup?get=types&uri=" + escapeUri typecluster
                let response = JsonValue.Load(uri).["response"]
                if response.AsString()= "fail" then
                    raise (HTTPSchemaException("GetAllTypesIn " +  typecluster + " failed with response " + response.AsString()))
                else
                    [for item in JsonValue.Load(uri).["response"] -> trimUri(item.AsString())]
            with
            | _ -> raise (HTTPSchemaException("GetAllTypesIn" + typecluster + " failed"))

        member __.GetTypeClustersFor ``type`` =
            try
                let uri = "http://webschemex2.west.uni-koblenz.de/lookup?get=tc&uri=" + escapeUri ``type``
                let response = JsonValue.Load(uri).["response"]
                if response.AsString() = "fail" then
                    raise (HTTPSchemaException("GetTypeClustersFor " +  ``type`` + " failed with response " + response.AsString()))
                else
                    [for item in response -> trimUri(item.AsString())]
            with
            | _ -> raise (HTTPSchemaException("GetTypeClustersFor " +  ``type`` + " failed"))


        member __.GetAllEQCIn typecluster =
            try  
                let uri = "http://webschemex2.west.uni-koblenz.de/lookup?get=eqc&uri=" + escapeUri typecluster
                let response = JsonValue.Load(uri).["response"]
                if response.AsString() = "fail" then
                    raise (HTTPSchemaException("GetAllEQCIn " + typecluster + " failed with response " + response.AsString()))
                else
                    [for item in response -> trimUri(item.AsString())]
            with
            | _ ->  raise (HTTPSchemaException("GetAllEQCIn " + typecluster + " failed"))

        member __.GetPropertiesAndTypeClusterIn equivalenceClass =
            try
                let uri = "http://webschemex2.west.uni-koblenz.de/lookup?get=mappings&uri=" + escapeUri equivalenceClass
                let response = JsonValue.Load(uri).["response"]
                if response.AsString() = "fail" then
                    raise (HTTPSchemaException("GetPropertiesAndTypeClusterIn " + equivalenceClass + " failed with response " + response.AsString()))
                else
                    Array.toList (response.AsArray()) |> List.map (fun x -> 
                        Array.toList(x.[1].AsArray()) |> List.map (fun x -> trimUri(x.AsString())),
                        Array.toList(x.[0].AsArray())|> List.map (fun x -> trimUri(x.AsString())))
            with
            | _ -> raise (HTTPSchemaException("GetPropertiesAndTypeClusterIn " + equivalenceClass + " failed"))
             
        member __.GetAllInstancesIn equivalenceClass =
            try
                let uri = "http://webschemex2.west.uni-koblenz.de/lookup?get=entities&uri=" + escapeUri equivalenceClass
                let response =  JsonValue.Load(uri)
                if response.AsString() = "fail" then
                    raise (HTTPSchemaException("GetAllInstancesIn " + equivalenceClass + " failed with response " + response.AsString()))
                else
                    [for item in response -> trimUri(item.AsString())]
            with
            | _ -> raise (HTTPSchemaException("GetAllInstancesIn " + equivalenceClass + " failed"))

    interface StartingPointProvider with
        member __.Get () = ["http://xmlns.com/foaf/0.1/Person"]

type DummySchema() = 
    interface SchemaProvider with
            member __.AllRdfTypes =
                db
                |> List.filter(fun (s,p,o) -> p = "containsClass")
                |> List.map ``object``

            member __.GetAllTypesIn typeCluster = 
                db
                |> List.filter(fun (s,p,_) -> s = typeCluster && p = "containsClass")
                |> List.map ``object``

            member __.GetTypeClustersFor ``type`` = 
                db
                |> List.filter(fun (s,p,o) -> p = "containsClass" && o = ``type``)
                |> List.map subject
                
            member __.GetAllEQCIn typeCluster = 
                db
                |> List.filter(fun (s,p,_) -> s = typeCluster && p = "hasEqc")
                |> List.map ``object``

            (*
            member __.GetAllPropertiesIn equivalenceCluster = 
                db
                |> List.filter(fun (s,p,o) -> s = equivalenceCluster && p = "hasProp")
                |> List.map third
            *)

            member __.GetPropertiesAndTypeClusterIn equivalenceCluster = 
                let propertyCluster =
                    db
                    |> List.filter(fun (s,p,_) -> s = equivalenceCluster && p = "hasPC")
                    |> List.map ``object``
                
                db
                |> List.filter(fun (s,p,o) -> List.exists ((=) s) propertyCluster)
                |> Seq.groupBy(fun (s,_,_) -> s)
                |> Seq.map(fun (_,l) ->
                    l
                    |> Seq.fold(fun (listOfProps,listOfTCs) (_,p,o) ->
                        match p with
                        | "hasRange" -> listOfProps, o :: listOfTCs
                        | _ -> o :: listOfProps, listOfTCs
                    ) (List.empty<string>, List.empty<string>)
                )
                |> List.ofSeq

            // Instance related stuff
            member __.GetAllInstancesIn equivalenceCluster =
                let dataSets = 
                    db
                    |> List.filter(fun (s,p,o) -> s = equivalenceCluster && p = "hasDS")
                    |> List.map ``object``

                db
                |> List.filter(fun (s,p,o) -> List.exists ((=) s) dataSets && p = "hasInstance")
                |> List.map ``object``
 

    interface StartingPointProvider with
        member __.Get () = ["Person"]