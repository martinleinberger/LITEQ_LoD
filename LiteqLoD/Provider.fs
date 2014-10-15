namespace LiteqLoD

open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open System.Reflection

open Schema
open Utils


[<TypeProvider>]
type RDFTypeProvider(config : TypeProviderConfig) as this = 
    class
        inherit TypeProviderForNamespaces()
        let ns = "Uniko.West"
        let asm = Assembly.GetExecutingAssembly()
        let provTy = ProvidedTypeDefinition(asm, ns, "LoD", Some typeof<obj>)
        
        [<DefaultValue>] val mutable private schema : ISchemaProvider
        [<DefaultValue>] val mutable private startingPoint : IStartingPointProvider 
        let mutable niceName : Uri -> string = id

        // Will be used in future updates
        let mutable prefixOf : Uri -> string = fun _ -> "unknownPrefix"
        let intension_cache : ICache<ProvidedTypeDefinition> = createInMemoryCache()

        // This stuff is needed as we want to add things as possible starting points once they have been explored
        let container = new ProvidedTypeDefinition("NPQL", None)
        
        let findPropertiesForType : Type -> (Property * TypeCluster list) list = memoize(fun ``type`` ->
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
        )

        // Building the intension is not yet convincingly implemented
        let rec makeIntension = fun restrictions typeName ->
            let predefinedProperties =
                restrictions
                |> Seq.toList
                |> List.rev
                |> List.tail
                |> Seq.pairwise
                |> Seq.toList
                |> List.map(fun ((_,propertyName,_),(_,_,typeName)) ->
                    let t = ProvidedTypeDefinition(className=typeName, baseType=Some typeof<obj>)
                    t.AddMembersDelayed (fun _ ->
                        findPropertiesForType typeName
                        |> Seq.map(fun (p,tc) -> ProvidedProperty(propertyName=p, propertyType=typeof<string>,
                                                    GetterCode = fun args -> <@@ "" @@>))
                        |> Seq.toList
                    )
                    let p = ProvidedProperty(propertyName=propertyName, propertyType=t, GetterCode=fun args -> <@@ new obj() @@>)
                    (t,p)
                )
            let allProperties =
                findPropertiesForType typeName 
                |> Seq.filter(fun (p,tc) -> restrictions |> Seq.exists(fun (s, p',o) -> p' = p) |> not) 
                |> Seq.map(fun (p,tc) -> ProvidedProperty(propertyName=p, propertyType=typeof<string>, GetterCode = fun args -> <@@ "" @@>))
                |> Seq.toList
            let t = ProvidedTypeDefinition(className="Intension", baseType=Some typeof<RdfResource.RdfResource>)
            t.AddMembers allProperties
            predefinedProperties
            |> Seq.iter(fun (t',p) ->
                t.AddMember t'
                t.AddMember p
            )
            t.AddMember (ProvidedConstructor(parameters=[], InvokeCode = fun _ -> <@@ new obj() @@>))

            t

        // A property navigation basically erases all previously selected restrictions...
        and makePropertyNavigation = fun freeVariable restrictions ``type`` ->
            findPropertiesForType ``type``
            |> List.map(fun (property, typeClusters) -> 
                let t = ProvidedTypeDefinition("-"+(niceName property)+"->", None)
                t.AddMembersDelayed (fun _ ->
                    typeClusters
                    |> List.map (this.schema.GetAllTypesIn)
                    |> List.concat
                    |> Set.ofList
                    |> Set.toList
                    |> List.map (fun typeName -> makeType "?y" ["?x", "a", typeName] typeName typeName)
                )
                t
            )

        and makePropertyRestriction = fun freeVariable restrictions ``type`` ->
            findPropertiesForType ``type``
            |> List.map(fun (property, typeClusters) ->
                let t = ProvidedTypeDefinition("<-"+(niceName property)+"-", None)

                t.AddMembersDelayed (fun _ ->
                    let restrictions' = ("?x", property, freeVariable) :: restrictions

                    typeClusters
                    |> List.map (this.schema.GetAllTypesIn)
                    |> List.concat
                    |> Set.ofList
                    |> Set.toList
                    |> List.map (fun typeName -> makeType (freeVariable+"y") ((freeVariable, "a", typeName) :: restrictions') ``type`` typeName)

                )
                t
            )

        and makeType = fun freeVariable restrictions typeForPropertyRestriction typeName ->
            let t = ProvidedTypeDefinition(className = (niceName typeName), baseType=None)
            // Add Intension and Extension
            t.AddMembersDelayed (fun _ ->
                let tmp = restrictions |> List.map(fun (s, p, o) -> s+", "+p+", "+o) |> String.concat " . \n"
                let intension = makeIntension restrictions typeForPropertyRestriction 
                let extension = ProvidedProperty("Extension", typedefof<seq<_>>.MakeGenericType(intension),
                                    IsStatic=true, GetterCode = fun _ ->
                                    <@@
                                        let patterns =
                                            tmp.Split([|" . \n"|], System.StringSplitOptions.None)
                                            |> List.ofArray
                                            |> List.rev
                                        let typeName = patterns.[0].Split([|", "|], System.StringSplitOptions.None).[2]
                                        let remainingRestrictions = patterns |> List.tail |> List.map(fun s ->
                                            let x = s.Split([|", "|], System.StringSplitOptions.None)
                                            x.[0], x.[1], x.[2]
                                        )
                                        RdfResource.gatherInstances "schemex" typeName remainingRestrictions
                                    @@>)
                [intension :> MemberInfo; extension :> MemberInfo]
            )
            // Add property navigation and restriction
            t.AddMembersDelayed (fun _ -> makePropertyNavigation freeVariable restrictions typeName)
            t.AddMembersDelayed (fun _ -> makePropertyRestriction freeVariable restrictions typeForPropertyRestriction)
            t

        and addToContainer = fun typeName ->
            if Utils.typeCache.Contains typeName |> not then
                container.AddMember (makeType "?y" ["?x", "a", typeName] typeName typeName)
                Utils.addType typeName
            
        let buildTypes (typeName : string) (startingStuff : string) = 
            let t = ProvidedTypeDefinition(className = (niceName typeName), baseType = None)
            t.AddMemberDelayed (fun _ ->
                container.AddMembersDelayed (fun _ -> this.startingPoint.Get() |> List.map (fun typeName -> makeType "?y" ["?x", "a", typeName] typeName typeName))
                container.AddMembersDelayed (fun _ ->
                    Utils.typeCache |> Seq.map (fun typeName -> makeType "?y" ["?x", "a", typeName] typeName typeName) |> Seq.toList
                )
                container)
            provTy.AddMember t
            t
        
        let parameters = [ ProvidedStaticParameter("endpointUrl", typeof<string>) ]
        do
            // Set source of schematic data
            let s = HTTPSchema()
            this.schema <- s :> ISchemaProvider
            this.startingPoint <- s :> IStartingPointProvider

        do provTy.DefineStaticParameters(parameters, fun typeName args -> buildTypes typeName (args.[0] :?> string))
        do this.AddNamespace(ns, [ provTy ])
    end

[<TypeProviderAssembly>]
do ()