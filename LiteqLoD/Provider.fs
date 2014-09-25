namespace LiteqLoD

open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open System.Reflection

open Schema




[<TypeProvider>]
type RDFTypeProvider(config : TypeProviderConfig) as this = 
    class
        inherit TypeProviderForNamespaces()
        let ns = "Uniko.West"
        let asm = Assembly.GetExecutingAssembly()
        let provTy = ProvidedTypeDefinition(asm, ns, "LoD", Some typeof<obj>)
        
        [<DefaultValue>] val mutable private schema : SchemaProvider
        [<DefaultValue>] val mutable private startingPoint : StartingPointProvider 
        [<DefaultValue>] val mutable private niceName : string -> string

        // This stuff is needed as we want to add things as possible starting points once they have been explored
        let container = new ProvidedTypeDefinition("NPQL", None)

        let findPropertiesForType (``type``:Type) : (Property * TypeCluster list) list = 
            // Access those type clusters that contain the type
            this.schema.GetTypeClustersFor ``type``
            // Map these type clusters to equivalence clusters and flatten the list
            |> List.map(fun typeCluster ->
                this.schema.GetAllEQCIn typeCluster
            )
            |> List.concat
            // Get the properties and their ranges for those equivalence clusters (make the list flat again)
            |> List.map(fun equivalenceCluster ->
                this.schema.GetPropertiesAndTypeClusterIn equivalenceCluster
            )
            |> List.concat
            // Transform the [ [p1;p2] * [tc1;tc2]; [p2;p3] * [tc3] ] list into [ p1 * [tc1;tc2] ; p2 * [tc1;tc2;tc3] ; ... ]
            |> List.fold(fun (accumulator:Map<Property, TypeCluster list>) (properties, typeClusters) ->
                properties |> List.fold(fun (accumulator:Map<Property, TypeCluster list>) property ->
                    if accumulator.ContainsKey property 
                        then accumulator.Add (property, typeClusters @ accumulator.[property])
                        else accumulator.Add (property, typeClusters)
                ) accumulator
            ) Map.empty<Property, TypeCluster list>
            |> Map.toList
            // Remove the duplicate type clusters in the type cluster lists
            // HACK: Pretty sure this could be included into the previous step
            |> List.map(fun (property,typeClusters) ->
                property, (typeClusters |> Set.ofList |> Set.toList)
            )
        
        // Building the intension is not yet fully implemented right now
        let rec makeIntension = fun _ ->
            ProvidedTypeDefinition(className="Intension", baseType=Some typeof<obj>)

        and makePropertyNavigation = fun ``type`` ->
            findPropertiesForType ``type``
            |> List.map(fun (property,typeClusters) -> 
                let t = ProvidedTypeDefinition("-"+(this.niceName property)+"->", None)
                t.AddMembersDelayed (fun _ ->
                    typeClusters
                    |> List.map makeTypesOutOfCluster
                    |> List.concat
                )
                t
            )

        and makePropertyRestriction = fun ``type`` ->
            findPropertiesForType ``type``
            |> List.map(fun (property, typeClusters) ->
                let t = ProvidedTypeDefinition("<-"+(this.niceName property)+"-", None)
                t.AddMembersDelayed (fun _ ->
                    typeClusters
                    |> List.map makeTypesOutOfCluster
                    |> List.concat
                )
                t
            )

        and makeTypesOutOfCluster = fun typeCluster ->
            let types = this.schema.GetAllTypesIn typeCluster
            types |> Seq.iter addToContainer
            types |> List.map makeType

        and makeType = fun typeName ->
            let t = ProvidedTypeDefinition(className = (this.niceName typeName), baseType=None)
            // Add Intension and Extension
            t.AddMembersDelayed (fun _ ->
                let intension = makeIntension ()
                let extension = ProvidedProperty("Extension", typedefof<seq<_>>.MakeGenericType(intension),
                                    IsStatic=true, GetterCode = fun _ -> <@@ [] @@>)
                [intension :> MemberInfo; extension :> MemberInfo]
            )
            // Add property navigation and restriction
            t.AddMembersDelayed (fun _ -> makePropertyNavigation typeName)
            t.AddMembersDelayed (fun _ -> makePropertyRestriction typeName)
            t

        and addToContainer = fun typeName ->
            if Utils.typeCache.Contains typeName |> not then
                container.AddMember (makeType typeName)
                Utils.addType typeName
            
        let buildTypes (typeName : string) (startingStuff :string) = 

            let t = ProvidedTypeDefinition(className = (this.niceName typeName), baseType = None)
            t.AddMemberDelayed (fun _ ->
                container.AddMembersDelayed (fun _ -> this.startingPoint.Get() |> List.map makeType)
                container.AddMembersDelayed (fun _ ->
                    Utils.typeCache |> Seq.map makeType |> Seq.toList
                    )
                container)
            provTy.AddMember t
            t
        
        let parameters = [ ProvidedStaticParameter("endpointUrl", typeof<string>) ]
        do
            // Set source of schematic data
            let dummy = DummySchema()
            this.schema <- dummy :> SchemaProvider
            this.startingPoint <- dummy :> StartingPointProvider
            this.niceName <- id

        do provTy.DefineStaticParameters(parameters, fun typeName args -> buildTypes typeName (args.[0] :?> string))
        do this.AddNamespace(ns, [ provTy ])
    end

[<TypeProviderAssembly>]
do ()