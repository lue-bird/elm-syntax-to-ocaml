module ElmSyntaxToOcaml exposing
    ( modules, ocamlDeclarationsToModuleString
    , OcamlLetDeclaration(..), OcamlExpression(..), OcamlPattern(..), OcamlType(..)
    )

{-| Transpiling [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/)
declarations to ocaml.

@docs modules, ocamlDeclarationsToModuleString
@docs OcamlLetDeclaration, OcamlExpression, OcamlPattern, OcamlType

If you need more fine-grained helpers,
[open an issue](https://github.com/lue-bird/elm-syntax-format/issues/new)

-}

import Data.Graph
import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Import
import Elm.Syntax.Module
import Elm.Syntax.ModuleName
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation
import FastDict
import FastSet
import Print exposing (Print)
import Unicode


{-| The sub-set of F# type syntax used in generated code
-}
type OcamlType
    = OcamlTypeConstruct
        { moduleOrigin : Maybe String
        , name : String
        , arguments : List OcamlType
        }
    | OcamlTypeTuple
        { part0 : OcamlType
        , part1 : OcamlType
        , part2Up : List OcamlType
        }
    | OcamlTypeRecord (FastDict.Dict String OcamlType)
    | OcamlTypeVariable String
    | OcamlTypeFunction
        { input : OcamlType
        , output : OcamlType
        }


{-| The sub-set of F# pattern syntax used in generated code
-}
type OcamlPattern
    = OcamlPatternIgnore
    | OcamlPatternFloat Float
    | OcamlPatternChar Char
    | OcamlPatternString String
    | OcamlPatternVariable String
    | OcamlPatternAs
        { variable : String
        , pattern : OcamlPattern
        }
    | OcamlPatternListCons
        { head : OcamlPattern
        , tail : OcamlPattern
        }
    | OcamlPatternListExact (List OcamlPattern)
    | OcamlPatternRecordInexhaustive (FastSet.Set String)
    | OcamlPatternVariant
        { moduleOrigin : Maybe String
        , name : String
        , value : Maybe OcamlPattern
        }
    | OcamlPatternTuple
        { part0 : OcamlPattern
        , part1 : OcamlPattern
        , part2Up : List OcamlPattern
        }


{-| The sub-set of F# expression syntax used in generated code
-}
type OcamlExpression
    = OcamlExpressionUnit
    | OcamlExpressionFloat Float
    | OcamlExpressionChar Char
    | OcamlExpressionString String
    | OcamlExpressionReference
        { moduleOrigin : Maybe String
        , name : String
        }
    | OcamlExpressionRecordAccess
        { record : OcamlExpression
        , field : String
        }
    | OcamlExpressionTuple
        { part0 : OcamlExpression
        , part1 : OcamlExpression
        , part2Up : List OcamlExpression
        }
    | OcamlExpressionIfThenElse
        { condition : OcamlExpression
        , onTrue : OcamlExpression
        , onFalse : OcamlExpression
        }
    | OcamlExpressionList (List OcamlExpression)
    | OcamlExpressionRecord (FastDict.Dict String OcamlExpression)
    | OcamlExpressionRecordUpdate
        { originalRecordVariable : String
        , fields : FastDict.Dict String OcamlExpression
        }
    | OcamlExpressionCall
        { called : OcamlExpression
        , argument0 : OcamlExpression
        , argument1Up : List OcamlExpression
        }
    | OcamlExpressionLambda
        { parameter0 : OcamlPattern
        , parameter1Up : List OcamlPattern
        , result : OcamlExpression
        }
    | OcamlExpressionMatchWith
        { matched : OcamlExpression
        , case0 :
            { pattern : OcamlPattern
            , result : OcamlExpression
            }
        , case1Up :
            List
                { pattern : OcamlPattern
                , result : OcamlExpression
                }
        }
    | OcamlExpressionLetIn
        { declaration0 : OcamlLetDeclaration
        , declaration1Up : List OcamlLetDeclaration
        , result : OcamlExpression
        }


{-| The sub-set of ocaml local declaration syntax used in generated ocaml code
-}
type OcamlLetDeclaration
    = OcamlLetDestructuring
        { pattern : OcamlPattern
        , expression : OcamlExpression
        }
    | OcamlLetDeclarationValueOrFunction
        { name : String
        , parameters : List OcamlPattern
        , result : OcamlExpression
        , type_ : Maybe OcamlType
        }


{-| How do references used in a module map to their origin module?

Contains variants, variant function and value declaration names.

-}
type alias ModuleContext =
    { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    }


{-| Calculate valid mappings of qualifications + name
to origin module based on a module's imports.

Requires all exposed names
so we can resolve `exposing (..)` and `ChoiceType(..)`.

-}
importsToModuleContext :
    FastDict.Dict
        Elm.Syntax.ModuleName.ModuleName
        { valueOrFunctionOrTypeAliasNames : FastSet.Set String
        , choiceTypesExposingVariants :
            FastDict.Dict String (FastDict.Dict String { valueCount : Int })
        }
    -> List (Elm.Syntax.Node.Node Elm.Syntax.Import.Import)
    -> ModuleContext
importsToModuleContext moduleExposes imports =
    let
        importsNormal :
            List
                { moduleName : Elm.Syntax.ModuleName.ModuleName
                , alias : Maybe String
                , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
                    FastSet.Set String
                , exposedVariants :
                    FastDict.Dict String { valueCount : Int }
                }
        importsNormal =
            implicitImports
                ++ (imports
                        |> List.map
                            (\(Elm.Syntax.Node.Node _ syntaxImport) ->
                                let
                                    importModuleName : Elm.Syntax.ModuleName.ModuleName
                                    importModuleName =
                                        syntaxImport.moduleName |> Elm.Syntax.Node.value

                                    exposes :
                                        { valuesAndFunctionsAndTypeAliasesAndChoiceTypes :
                                            FastSet.Set String
                                        , variants :
                                            FastDict.Dict String { valueCount : Int }
                                        }
                                    exposes =
                                        case syntaxImport.exposingList of
                                            Nothing ->
                                                { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                    FastSet.empty
                                                , variants = FastDict.empty
                                                }

                                            Just (Elm.Syntax.Node.Node _ syntaxExposing) ->
                                                case moduleExposes |> FastDict.get importModuleName of
                                                    Nothing ->
                                                        { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                            FastSet.empty
                                                        , variants = FastDict.empty
                                                        }

                                                    Just moduleExposedNames ->
                                                        case syntaxExposing of
                                                            Elm.Syntax.Exposing.All _ ->
                                                                { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                                    moduleExposedNames.choiceTypesExposingVariants
                                                                        |> FastDict.foldl
                                                                            (\choiceTypeName _ soFar ->
                                                                                soFar |> FastSet.insert choiceTypeName
                                                                            )
                                                                            moduleExposedNames.valueOrFunctionOrTypeAliasNames
                                                                , variants =
                                                                    moduleExposedNames.choiceTypesExposingVariants
                                                                        |> FastDict.foldl
                                                                            (\_ variantNames soFar -> FastDict.union variantNames soFar)
                                                                            FastDict.empty
                                                                }

                                                            Elm.Syntax.Exposing.Explicit explicitEposes ->
                                                                explicitEposes
                                                                    |> List.foldl
                                                                        (\(Elm.Syntax.Node.Node _ expose) soFar ->
                                                                            case expose of
                                                                                Elm.Syntax.Exposing.InfixExpose _ ->
                                                                                    soFar

                                                                                Elm.Syntax.Exposing.TypeOrAliasExpose name ->
                                                                                    { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                                                        soFar.valuesAndFunctionsAndTypeAliasesAndChoiceTypes
                                                                                            |> FastSet.insert name
                                                                                    , variants = soFar.variants
                                                                                    }

                                                                                Elm.Syntax.Exposing.FunctionExpose name ->
                                                                                    { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                                                        soFar.valuesAndFunctionsAndTypeAliasesAndChoiceTypes
                                                                                            |> FastSet.insert name
                                                                                    , variants = soFar.variants
                                                                                    }

                                                                                Elm.Syntax.Exposing.TypeExpose choiceTypeExpose ->
                                                                                    { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                                                        soFar.valuesAndFunctionsAndTypeAliasesAndChoiceTypes
                                                                                            |> FastSet.insert choiceTypeExpose.name
                                                                                    , variants =
                                                                                        case choiceTypeExpose.open of
                                                                                            Nothing ->
                                                                                                soFar.variants

                                                                                            Just _ ->
                                                                                                case
                                                                                                    moduleExposedNames.choiceTypesExposingVariants
                                                                                                        |> FastDict.get choiceTypeExpose.name
                                                                                                of
                                                                                                    Nothing ->
                                                                                                        soFar.variants

                                                                                                    Just choiceTypeDeclared ->
                                                                                                        FastDict.union
                                                                                                            soFar.variants
                                                                                                            choiceTypeDeclared
                                                                                    }
                                                                        )
                                                                        { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                                                            FastSet.empty
                                                                        , variants = FastDict.empty
                                                                        }
                                in
                                { moduleName = importModuleName
                                , alias =
                                    syntaxImport.moduleAlias
                                        |> Maybe.map
                                            (\(Elm.Syntax.Node.Node _ syntaxAlias) ->
                                                syntaxAlias |> String.join "."
                                            )
                                , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                    exposes.valuesAndFunctionsAndTypeAliasesAndChoiceTypes
                                , exposedVariants =
                                    exposes.variants
                                }
                            )
                   )
                |> importsCombine
    in
    importsNormal
        |> List.foldl
            (\syntaxImport soFar ->
                let
                    importedModuleMembers :
                        { valuesAndFunctionsAndTypeAliasesAndChoiceTypes :
                            FastSet.Set String
                        , variants : FastDict.Dict String { valueCount : Int }
                        }
                    importedModuleMembers =
                        case moduleExposes |> FastDict.get syntaxImport.moduleName of
                            Nothing ->
                                { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                    FastSet.empty
                                , variants = FastDict.empty
                                }

                            Just moduleExposedNames ->
                                { valuesAndFunctionsAndTypeAliasesAndChoiceTypes =
                                    moduleExposedNames.choiceTypesExposingVariants
                                        |> FastDict.foldl
                                            (\choiceTypeName _ namesSoFar ->
                                                namesSoFar
                                                    |> FastSet.insert choiceTypeName
                                            )
                                            moduleExposedNames.valueOrFunctionOrTypeAliasNames
                                , variants =
                                    moduleExposedNames.choiceTypesExposingVariants
                                        |> FastDict.foldl
                                            (\_ variantNames variantsSoFar ->
                                                FastDict.union variantNames variantsSoFar
                                            )
                                            FastDict.empty
                                }
                in
                moduleContextMerge
                    (moduleContextMerge
                        { variantLookup =
                            syntaxImport.exposedVariants
                                |> FastDict.foldl
                                    (\variantName variantInfo dictSoFar ->
                                        dictSoFar
                                            |> FastDict.insert ( [], variantName )
                                                { moduleOrigin = syntaxImport.moduleName
                                                , valueCount = variantInfo.valueCount
                                                }
                                    )
                                    FastDict.empty
                        , valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                            syntaxImport.exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes
                                |> FastSet.foldl
                                    (\expose dictSoFar ->
                                        dictSoFar
                                            |> FastDict.insert ( [], expose )
                                                syntaxImport.moduleName
                                    )
                                    FastDict.empty
                        }
                        (case syntaxImport.alias of
                            Nothing ->
                                { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                                    importedModuleMembers.valuesAndFunctionsAndTypeAliasesAndChoiceTypes
                                        |> FastSet.foldl
                                            (\exposeFromImportedModule dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( syntaxImport.moduleName, exposeFromImportedModule )
                                                        syntaxImport.moduleName
                                            )
                                            FastDict.empty
                                , variantLookup =
                                    importedModuleMembers.variants
                                        |> FastDict.foldl
                                            (\exposeFromImportedModule variantInfo dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( syntaxImport.moduleName, exposeFromImportedModule )
                                                        { moduleOrigin = syntaxImport.moduleName
                                                        , valueCount = variantInfo.valueCount
                                                        }
                                            )
                                            FastDict.empty
                                }

                            Just importAlias ->
                                { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                                    importedModuleMembers.valuesAndFunctionsAndTypeAliasesAndChoiceTypes
                                        |> FastSet.foldl
                                            (\exposeFromImportedModule dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( [ importAlias ], exposeFromImportedModule )
                                                        syntaxImport.moduleName
                                            )
                                            FastDict.empty
                                , variantLookup =
                                    importedModuleMembers.variants
                                        |> FastDict.foldl
                                            (\exposeFromImportedModule variantInfo dictSoFar ->
                                                dictSoFar
                                                    |> FastDict.insert
                                                        ( [ importAlias ], exposeFromImportedModule )
                                                        { moduleOrigin = syntaxImport.moduleName
                                                        , valueCount = variantInfo.valueCount
                                                        }
                                            )
                                            FastDict.empty
                                }
                        )
                    )
                    soFar
            )
            { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                FastDict.empty
            , variantLookup =
                FastDict.empty
            }


moduleContextMerge : ModuleContext -> ModuleContext -> ModuleContext
moduleContextMerge a b =
    { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
        FastDict.union
            a.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
            b.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
    , variantLookup =
        FastDict.union
            a.variantLookup
            b.variantLookup
    }


implicitImports :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
implicitImports =
    [ { moduleName = [ "Basics" ]
      , alias = Nothing
      , exposedVariants =
            FastDict.fromList
                [ ( "EQ", { valueCount = 0 } )
                , ( "LT", { valueCount = 0 } )
                , ( "GT", { valueCount = 0 } )
                , ( "True", { valueCount = 0 } )
                , ( "False", { valueCount = 0 } )
                ]
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList
                [ "Int"
                , "Float"
                , "toFloat"
                , "round"
                , "floor"
                , "ceiling"
                , "truncate"
                , "max"
                , "min"
                , "compare"
                , "Order"
                , "Bool"
                , "not"
                , "xor"
                , "modBy"
                , "remainderBy"
                , "negate"
                , "abs"
                , "clamp"
                , "sqrt"
                , "logBase"
                , "e"
                , "pi"
                , "cos"
                , "sin"
                , "tan"
                , "acos"
                , "asin"
                , "atan"
                , "atan2"
                , "degrees"
                , "radians"
                , "turns"
                , "toPolar"
                , "fromPolar"
                , "isNaN"
                , "isInfinite"
                , "identity"
                , "always"
                , "Never"
                , "never"
                ]
      }
    , { moduleName = [ "List" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "List" ]
      }
    , { moduleName = [ "Maybe" ]
      , alias = Nothing
      , exposedVariants =
            FastDict.fromList
                [ ( "Just", { valueCount = 1 } )
                , ( "Nothing", { valueCount = 0 } )
                ]
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "Maybe" ]
      }
    , { moduleName = [ "Result" ]
      , alias = Nothing
      , exposedVariants =
            FastDict.fromList
                [ ( "Ok", { valueCount = 1 } )
                , ( "Err", { valueCount = 1 } )
                ]
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "Result" ]
      }
    , { moduleName = [ "String" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "String" ]
      }
    , { moduleName = [ "Char" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "Char" ]
      }
    , { moduleName = [ "Tuple" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.empty
      }
    , { moduleName = [ "Debug" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.empty
      }
    , { moduleName = [ "Platform" ]
      , alias = Nothing
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "Program" ]
      }
    , { moduleName = [ "Platform", "Cmd" ]
      , alias = Just "Cmd"
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "Cmd" ]
      }
    , { moduleName = [ "Platform", "Sub" ]
      , alias = Just "Sub"
      , exposedVariants = FastDict.empty
      , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
            FastSet.fromList [ "Sub" ]
      }
    ]


importsCombine :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict String { valueCount : Int }
            }
importsCombine syntaxImports =
    importsCombineFrom [] syntaxImports


importsCombineFrom :
    List
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict String { valueCount : Int }
            }
    ->
        List
            { moduleName : Elm.Syntax.ModuleName.ModuleName
            , alias : Maybe String
            , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
                FastSet.Set String
            , exposedVariants :
                FastDict.Dict String { valueCount : Int }
            }
importsCombineFrom soFar syntaxImports =
    case syntaxImports of
        [] ->
            soFar

        [ onlyImport ] ->
            onlyImport :: soFar

        import0 :: import1 :: import2Up ->
            if import0.moduleName == import1.moduleName then
                importsCombineFrom soFar
                    (importsMerge import0 import1
                        :: import2Up
                    )

            else
                importsCombineFrom
                    (import0 :: soFar)
                    (import1 :: import2Up)


importsMerge :
    { moduleName : Elm.Syntax.ModuleName.ModuleName
    , alias : Maybe String
    , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
        FastSet.Set String
    , exposedVariants :
        FastDict.Dict String { valueCount : Int }
    }
    ->
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
    ->
        { moduleName : Elm.Syntax.ModuleName.ModuleName
        , alias : Maybe String
        , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes :
            FastSet.Set String
        , exposedVariants :
            FastDict.Dict String { valueCount : Int }
        }
importsMerge earlier later =
    { moduleName = earlier.moduleName
    , alias =
        case earlier.alias of
            Just alias ->
                alias |> Just

            Nothing ->
                later.alias
    , exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes =
        FastSet.union
            earlier.exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes
            later.exposedValuesAndFunctionsAndTypeAliasesAndChoiceTypes
    , exposedVariants =
        FastDict.union
            earlier.exposedVariants
            later.exposedVariants
    }


ocamlTypeContainedRecords :
    OcamlType
    ->
        FastSet.Set
            -- sorted field names
            (List String)
ocamlTypeContainedRecords ocamlType =
    -- IGNORE TCO
    case ocamlType of
        OcamlTypeRecord fields ->
            FastSet.insert
                (fields |> FastDict.keys)
                (fields
                    |> fastDictMapToFastSetAndUnify
                        ocamlTypeContainedRecords
                )

        OcamlTypeVariable _ ->
            FastSet.empty

        OcamlTypeFunction typeFunction ->
            FastSet.union
                (typeFunction.input |> ocamlTypeContainedRecords)
                (typeFunction.output |> ocamlTypeContainedRecords)

        OcamlTypeTuple typeTuple ->
            (typeTuple.part0 :: typeTuple.part1 :: typeTuple.part2Up)
                |> listMapToFastSetsAndUnify
                    ocamlTypeContainedRecords

        OcamlTypeConstruct typeConstruct ->
            typeConstruct.arguments
                |> listMapToFastSetsAndUnify
                    ocamlTypeContainedRecords


ocamlExpressionContainedConstructedRecords :
    OcamlExpression
    ->
        FastSet.Set
            -- sorted field names
            (List String)
ocamlExpressionContainedConstructedRecords syntaxExpression =
    -- IGNORE TCO
    case syntaxExpression of
        OcamlExpressionRecord fields ->
            FastSet.singleton
                (fields |> FastDict.keys)

        expressionNotRecord ->
            expressionNotRecord
                |> ocamlExpressionSubs
                |> listMapToFastSetsAndUnify
                    ocamlExpressionContainedConstructedRecords


ocamlExpressionContainedLocalReferences :
    OcamlExpression
    -> FastSet.Set String
ocamlExpressionContainedLocalReferences syntaxExpression =
    -- IGNORE TCO
    case syntaxExpression of
        OcamlExpressionReference reference ->
            case reference.moduleOrigin of
                Just _ ->
                    FastSet.empty

                Nothing ->
                    FastSet.singleton reference.name

        expressionNotRecord ->
            expressionNotRecord
                |> ocamlExpressionSubs
                |> listMapToFastSetsAndUnify
                    ocamlExpressionContainedLocalReferences


{-| All surface-level child [expression](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Expression)s.
-}
ocamlExpressionSubs : OcamlExpression -> List OcamlExpression
ocamlExpressionSubs ocamlExpression =
    case ocamlExpression of
        OcamlExpressionCall call ->
            call.called
                :: call.argument0
                :: call.argument1Up

        OcamlExpressionList elements ->
            elements

        OcamlExpressionRecord fields ->
            fields |> FastDict.values

        OcamlExpressionRecordUpdate recordUpdate ->
            recordUpdate.fields |> FastDict.values

        OcamlExpressionIfThenElse ifThenElse ->
            [ ifThenElse.condition
            , ifThenElse.onTrue
            , ifThenElse.onFalse
            ]

        OcamlExpressionLetIn letIn ->
            List.foldr
                (\declaration soFar ->
                    case declaration of
                        OcamlLetDeclarationValueOrFunction letValueOrFunction ->
                            letValueOrFunction.result :: soFar

                        OcamlLetDestructuring letDestructuring ->
                            letDestructuring.expression :: soFar
                )
                [ letIn.result ]
                (letIn.declaration0 :: letIn.declaration1Up)

        OcamlExpressionMatchWith matchWith ->
            matchWith.matched
                :: matchWith.case0.result
                :: (matchWith.case1Up |> List.map .result)

        OcamlExpressionLambda lambda ->
            [ lambda.result ]

        OcamlExpressionTuple parts ->
            parts.part0
                :: parts.part1
                :: parts.part2Up

        OcamlExpressionRecordAccess recordAccess ->
            [ recordAccess.record ]

        OcamlExpressionFloat _ ->
            []

        OcamlExpressionString _ ->
            []

        OcamlExpressionChar _ ->
            []

        OcamlExpressionUnit ->
            []

        OcamlExpressionReference _ ->
            []


choiceTypeDeclaration :
    ModuleContext
    -> Elm.Syntax.Type.Type
    ->
        Result
            String
            { name : String
            , parameters : List String
            , variants : FastDict.Dict String (Maybe OcamlType)
            }
choiceTypeDeclaration moduleOriginLookup syntaxChoiceType =
    Result.map
        (\variants ->
            { name =
                syntaxChoiceType.name
                    |> Elm.Syntax.Node.value
            , parameters =
                syntaxChoiceType.generics
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ parameter) ->
                            parameter |> variableNameDisambiguateFromOcamlKeywords
                        )
            , variants = variants |> FastDict.fromList
            }
        )
        (syntaxChoiceType.constructors
            |> listMapAndCombineOk
                (\(Elm.Syntax.Node.Node _ syntaxVariant) ->
                    Result.map
                        (\values ->
                            ( syntaxVariant.name |> Elm.Syntax.Node.value
                            , case values of
                                [] ->
                                    Nothing

                                [ singleValue ] ->
                                    Just singleValue

                                value0 :: value1 :: value2Up ->
                                    Just
                                        (OcamlTypeTuple
                                            { part0 = value0
                                            , part1 = value1
                                            , part2Up = value2Up
                                            }
                                        )
                            )
                        )
                        (syntaxVariant.arguments
                            |> listMapAndCombineOk
                                (\value ->
                                    value |> type_ moduleOriginLookup
                                )
                        )
                )
        )


ocamlTypeParametersToString : List String -> String
ocamlTypeParametersToString ocamlTypeParameters =
    case ocamlTypeParameters of
        [] ->
            ""

        [ onlyParameter ] ->
            "'" ++ onlyParameter

        parameter0 :: parameter1 :: parameter2Up ->
            "("
                ++ ((parameter0 :: parameter1 :: parameter2Up)
                        |> List.map (\parameter -> "'" ++ parameter)
                        |> String.join ", "
                   )
                ++ ")"


printOcamlChoiceTypeDeclaration :
    { name : String
    , parameters : List String
    , variants : FastDict.Dict String (Maybe OcamlType)
    }
    -> Print
printOcamlChoiceTypeDeclaration ocamlChoiceType =
    Print.exactly
        ((case ocamlChoiceType.parameters of
            [] ->
                ""

            parameter0 :: parameter1Up ->
                ((parameter0 :: parameter1Up)
                    |> ocamlTypeParametersToString
                )
                    ++ " "
         )
            ++ ocamlChoiceType.name
            ++ " ="
        )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (ocamlChoiceType.variants
                            |> FastDict.toList
                            |> Print.listMapAndIntersperseAndFlatten
                                (\( name, maybeValue ) ->
                                    printOcamlVariant
                                        { name = name
                                        , maybeValue = maybeValue
                                        }
                                )
                                Print.linebreakIndented
                        )
                )
            )


printOcamlVariant : { name : String, maybeValue : Maybe OcamlType } -> Print
printOcamlVariant ocamlVariant =
    Print.exactly ("| " ++ ocamlVariant.name)
        |> Print.followedBy
            (case ocamlVariant.maybeValue of
                Nothing ->
                    Print.empty

                Just value ->
                    let
                        valuePrint : Print
                        valuePrint =
                            value |> printOcamlTypeParenthesizedIfSpaceSeparated
                    in
                    Print.exactly " of"
                        |> Print.followedBy
                            (Print.withIndentAtNextMultipleOf4
                                (Print.spaceOrLinebreakIndented
                                    (valuePrint |> Print.lineSpread)
                                    |> Print.followedBy valuePrint
                                )
                            )
            )


typeAliasDeclaration :
    ModuleContext
    -> Elm.Syntax.TypeAlias.TypeAlias
    ->
        Result
            String
            { name : String
            , parameters : List String
            , type_ : OcamlType
            }
typeAliasDeclaration moduleOriginLookup syntaxTypeAlias =
    Result.map
        (\aliasedType ->
            { name =
                syntaxTypeAlias.name
                    |> Elm.Syntax.Node.value
            , parameters =
                syntaxTypeAlias.generics
                    |> List.map
                        (\(Elm.Syntax.Node.Node _ parameter) ->
                            parameter |> variableNameDisambiguateFromOcamlKeywords
                        )
            , type_ = aliasedType
            }
        )
        (syntaxTypeAlias.typeAnnotation
            |> type_ moduleOriginLookup
        )


ocamlTypeReplaceRecordsByGeneratedAliases : OcamlType -> OcamlType
ocamlTypeReplaceRecordsByGeneratedAliases ocamlType =
    -- IGNORE TCO
    case ocamlType of
        OcamlTypeRecord typeRecordFields ->
            OcamlTypeConstruct
                { moduleOrigin = Nothing
                , name =
                    generatedOcamlRecordTypeAliasName
                        (typeRecordFields |> FastDict.keys)
                , arguments =
                    typeRecordFields
                        |> FastDict.values
                        |> List.map ocamlTypeReplaceRecordsByGeneratedAliases
                }

        OcamlTypeVariable name ->
            OcamlTypeVariable name

        OcamlTypeFunction typeFunction ->
            OcamlTypeFunction
                { input =
                    typeFunction.input |> ocamlTypeReplaceRecordsByGeneratedAliases
                , output =
                    typeFunction.output |> ocamlTypeReplaceRecordsByGeneratedAliases
                }

        OcamlTypeConstruct typeConstruct ->
            OcamlTypeConstruct
                { moduleOrigin = typeConstruct.moduleOrigin
                , name = typeConstruct.name
                , arguments =
                    typeConstruct.arguments
                        |> List.map ocamlTypeReplaceRecordsByGeneratedAliases
                }

        OcamlTypeTuple typeTuple ->
            OcamlTypeTuple
                { part0 = typeTuple.part0 |> ocamlTypeReplaceRecordsByGeneratedAliases
                , part1 = typeTuple.part1 |> ocamlTypeReplaceRecordsByGeneratedAliases
                , part2Up =
                    typeTuple.part2Up
                        |> List.map ocamlTypeReplaceRecordsByGeneratedAliases
                }


printOcamlTypeAliasDeclaration :
    { name : String
    , parameters : List String
    , type_ : OcamlType
    }
    -> Print
printOcamlTypeAliasDeclaration ocamlTypeAliasDeclaration =
    Print.exactly
        ((case ocamlTypeAliasDeclaration.parameters of
            [] ->
                ""

            parameter0 :: parameter1Up ->
                ((parameter0 :: parameter1Up)
                    |> ocamlTypeParametersToString
                )
                    ++ " "
         )
            ++ ocamlTypeAliasDeclaration.name
            ++ " ="
        )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (ocamlTypeAliasDeclaration.type_
                            |> printOcamlTypeNotParenthesized
                        )
                )
            )


type_ :
    ModuleContext
    -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
    -> Result String OcamlType
type_ moduleOriginLookup (Elm.Syntax.Node.Node _ syntaxType) =
    -- IGNORE TCO
    case syntaxType of
        Elm.Syntax.TypeAnnotation.Unit ->
            Ok ocamlTypeUnit

        Elm.Syntax.TypeAnnotation.GenericType variable ->
            Ok (OcamlTypeVariable (variable |> variableNameDisambiguateFromOcamlKeywords))

        Elm.Syntax.TypeAnnotation.Typed (Elm.Syntax.Node.Node _ reference) typedArguments ->
            let
                ( qualification, name ) =
                    reference
            in
            case moduleOriginLookup.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup |> FastDict.get reference of
                Nothing ->
                    Err
                        ("could not find module origin of the type reference "
                            ++ qualifiedToString
                                { qualification = qualification
                                , name = name
                                }
                        )

                Just moduleOrigin ->
                    Result.map
                        (\arguments ->
                            let
                                ocamlReference : { moduleOrigin : Maybe String, name : String }
                                ocamlReference =
                                    case
                                        { moduleOrigin = moduleOrigin
                                        , name = name
                                        }
                                            |> referenceToCoreOcaml
                                    of
                                        Just coreOcaml ->
                                            coreOcaml

                                        Nothing ->
                                            { moduleOrigin = Nothing
                                            , name =
                                                { moduleOrigin = moduleOrigin
                                                , name = name
                                                }
                                                    |> referenceToOcamlName
                                            }
                            in
                            OcamlTypeConstruct
                                { moduleOrigin = ocamlReference.moduleOrigin
                                , name = ocamlReference.name
                                , arguments = arguments
                                }
                        )
                        (typedArguments
                            |> listMapAndCombineOk
                                (\argument -> argument |> type_ moduleOriginLookup)
                        )

        Elm.Syntax.TypeAnnotation.Tupled parts ->
            case parts of
                [] ->
                    -- should be handled by Unit
                    Ok ocamlTypeUnit

                [ inParens ] ->
                    type_ moduleOriginLookup inParens

                [ tuplePart0, tuplePart1 ] ->
                    Result.map2
                        (\part0 part1 ->
                            OcamlTypeTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = []
                                }
                        )
                        (tuplePart0 |> type_ moduleOriginLookup)
                        (tuplePart1 |> type_ moduleOriginLookup)

                [ tuplePart0, tuplePart1, tuplePart2 ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            OcamlTypeTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = [ part2 ]
                                }
                        )
                        (tuplePart0 |> type_ moduleOriginLookup)
                        (tuplePart1 |> type_ moduleOriginLookup)
                        (tuplePart2 |> type_ moduleOriginLookup)

                _ :: _ :: _ :: _ :: _ ->
                    Err "too many tuple parts"

        Elm.Syntax.TypeAnnotation.Record recordFields ->
            Result.map (\fields -> OcamlTypeRecord (FastDict.fromList fields))
                (recordFields
                    |> listMapAndCombineOk
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ name, valueNode )) ->
                            Result.map
                                (\value ->
                                    ( name |> variableNameDisambiguateFromOcamlKeywords
                                    , value
                                    )
                                )
                                (valueNode |> type_ moduleOriginLookup)
                        )
                )

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation inputNode outputNode ->
            Result.map2
                (\input output ->
                    OcamlTypeFunction
                        { input = input
                        , output = output
                        }
                )
                (inputNode |> type_ moduleOriginLookup)
                (outputNode |> type_ moduleOriginLookup)

        Elm.Syntax.TypeAnnotation.GenericRecord _ _ ->
            Err "extensible record types are not supported"


ocamlTypeExpandFunctionOutput : OcamlType -> List OcamlType
ocamlTypeExpandFunctionOutput ocamlType =
    ocamlTypeExpandFunctionOutputInto [] ocamlType
        |> List.reverse


ocamlTypeExpandFunctionOutputInto : List OcamlType -> OcamlType -> List OcamlType
ocamlTypeExpandFunctionOutputInto soFar ocamlType =
    case ocamlType of
        OcamlTypeFunction function ->
            ocamlTypeExpandFunctionOutputInto
                (function.input :: soFar)
                function.output

        OcamlTypeConstruct construct ->
            OcamlTypeConstruct construct :: soFar

        OcamlTypeTuple parts ->
            OcamlTypeTuple parts :: soFar

        OcamlTypeRecord record ->
            OcamlTypeRecord record :: soFar

        OcamlTypeVariable variable ->
            OcamlTypeVariable variable :: soFar


ocamlTypeUnit : OcamlType
ocamlTypeUnit =
    OcamlTypeConstruct
        { moduleOrigin = Nothing
        , name = "unit"
        , arguments = []
        }


printOcamlTypeNotParenthesized : OcamlType -> Print
printOcamlTypeNotParenthesized ocamlType =
    -- IGNORE TCO
    case ocamlType of
        OcamlTypeVariable variable ->
            Print.exactly ("'" ++ variable)

        OcamlTypeConstruct typeConstruct ->
            case typeConstruct.arguments of
                [] ->
                    Print.exactly
                        (ocamlReferenceToString
                            { moduleOrigin = typeConstruct.moduleOrigin
                            , name = typeConstruct.name
                            }
                        )

                [ onlyArgument ] ->
                    let
                        argumentPrint : Print
                        argumentPrint =
                            onlyArgument |> printOcamlTypeParenthesizedIfSpaceSeparated
                    in
                    argumentPrint
                        |> Print.followedBy
                            (Print.withIndentAtNextMultipleOf4
                                (Print.spaceOrLinebreakIndented
                                    (argumentPrint |> Print.lineSpread)
                                    |> Print.followedBy
                                        (Print.exactly
                                            (ocamlReferenceToString
                                                { moduleOrigin = typeConstruct.moduleOrigin
                                                , name = typeConstruct.name
                                                }
                                            )
                                        )
                                )
                            )

                argument0 :: argument1 :: argument2Up ->
                    let
                        argumentPrints : List Print
                        argumentPrints =
                            (argument0 :: argument1 :: argument2Up)
                                |> List.map printOcamlTypeNotParenthesized

                        fullLineSpread : Print.LineSpread
                        fullLineSpread =
                            argumentPrints
                                |> Print.lineSpreadListMapAndCombine
                                    Print.lineSpread
                    in
                    Print.exactly "("
                        |> Print.followedBy
                            (case fullLineSpread of
                                Print.MultipleLines ->
                                    Print.exactly " "

                                Print.SingleLine ->
                                    Print.empty
                            )
                        |> Print.followedBy
                            (argumentPrints
                                |> Print.listMapAndIntersperseAndFlatten
                                    (\argumentPrint ->
                                        Print.withIndentIncreasedBy 2
                                            argumentPrint
                                    )
                                    (Print.emptyOrLinebreakIndented fullLineSpread
                                        |> Print.followedBy
                                            (Print.exactly ", ")
                                    )
                            )
                        |> Print.followedBy
                            (Print.emptyOrLinebreakIndented fullLineSpread)
                        |> Print.followedBy
                            (Print.exactly ")")
                        |> Print.followedBy
                            (Print.withIndentAtNextMultipleOf4
                                (Print.spaceOrLinebreakIndented fullLineSpread
                                    |> Print.followedBy
                                        (Print.exactly
                                            (ocamlReferenceToString
                                                { moduleOrigin = typeConstruct.moduleOrigin
                                                , name = typeConstruct.name
                                                }
                                            )
                                        )
                                )
                            )

        OcamlTypeTuple parts ->
            let
                part0Print : Print
                part0Print =
                    parts.part0 |> printOcamlTypeNotParenthesized

                part1Print : Print
                part1Print =
                    parts.part1 |> printOcamlTypeNotParenthesized

                part2UpPrints : List Print
                part2UpPrints =
                    parts.part2Up
                        |> List.map printOcamlTypeNotParenthesized
            in
            Print.exactly "( "
                |> Print.followedBy
                    ((part0Print :: part1Print :: part2UpPrints)
                        |> Print.listMapAndIntersperseAndFlatten
                            (\partPrint ->
                                Print.withIndentIncreasedBy 2
                                    partPrint
                            )
                            (Print.linebreakIndented
                                |> Print.followedBy
                                    (Print.exactly "* ")
                            )
                    )
                |> Print.followedBy
                    Print.linebreakIndented
                |> Print.followedBy (Print.exactly ")")

        OcamlTypeRecord typeRecord ->
            printOcamlTypeRecord typeRecord

        OcamlTypeFunction typeFunction ->
            let
                inputPrint : Print
                inputPrint =
                    printOcamlTypeParenthesizedIfSpaceSeparated
                        typeFunction.input

                outputExpanded : List OcamlType
                outputExpanded =
                    ocamlTypeExpandFunctionOutput typeFunction.output

                outputPrints : List Print
                outputPrints =
                    outputExpanded
                        |> List.map
                            printOcamlTypeParenthesizedIfSpaceSeparated

                fullLineSpread : Print.LineSpread
                fullLineSpread =
                    inputPrint
                        |> Print.lineSpread
                        |> Print.lineSpreadMergeWith
                            (\() ->
                                outputPrints
                                    |> Print.lineSpreadListMapAndCombine
                                        Print.lineSpread
                            )
            in
            inputPrint
                :: (outputPrints
                        |> List.map
                            (\outputPartPrint ->
                                Print.withIndentIncreasedBy 3
                                    outputPartPrint
                            )
                   )
                |> Print.listIntersperseAndFlatten
                    (Print.spaceOrLinebreakIndented fullLineSpread
                        |> Print.followedBy
                            (Print.exactly "-> ")
                    )


typeIsSpaceSeparated : OcamlType -> Bool
typeIsSpaceSeparated ocamlType =
    case ocamlType of
        OcamlTypeVariable _ ->
            False

        OcamlTypeConstruct typeConstruct ->
            case typeConstruct.arguments of
                [] ->
                    False

                _ :: _ ->
                    True

        OcamlTypeTuple _ ->
            False

        OcamlTypeRecord _ ->
            False

        OcamlTypeFunction _ ->
            True


printOcamlTypeParenthesizedIfSpaceSeparated : OcamlType -> Print
printOcamlTypeParenthesizedIfSpaceSeparated ocamlType =
    if ocamlType |> typeIsSpaceSeparated then
        printParenthesized
            (ocamlType |> printOcamlTypeNotParenthesized)

    else
        ocamlType |> printOcamlTypeNotParenthesized


printOcamlTypeRecord : FastDict.Dict String OcamlType -> Print
printOcamlTypeRecord syntaxRecordFields =
    if syntaxRecordFields |> FastDict.isEmpty then
        Print.exactly "{}"

    else
        let
            fieldsPrint : Print
            fieldsPrint =
                syntaxRecordFields
                    |> FastDict.toList
                    |> Print.listMapAndIntersperseAndFlatten
                        (\( fieldName, fieldValue ) ->
                            let
                                valuePrint : Print
                                valuePrint =
                                    printOcamlTypeNotParenthesized fieldValue
                            in
                            Print.withIndentIncreasedBy 2
                                (Print.exactly (fieldName ++ " :")
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            (Print.spaceOrLinebreakIndented
                                                (valuePrint |> Print.lineSpread)
                                                |> Print.followedBy valuePrint
                                            )
                                        )
                                )
                        )
                        (Print.linebreakIndented
                            |> Print.followedBy
                                (Print.exactly "; ")
                        )
        in
        printExactlyCurlyOpeningSpace
            |> Print.followedBy fieldsPrint
            |> Print.followedBy
                (Print.spaceOrLinebreakIndented
                    (fieldsPrint |> Print.lineSpread)
                )
            |> Print.followedBy printExactlyCurlyClosing


ocamlReferenceToString :
    { moduleOrigin : Maybe String
    , name : String
    }
    -> String
ocamlReferenceToString reference =
    case reference.moduleOrigin of
        Nothing ->
            reference.name

        Just moduleName ->
            moduleName
                ++ "."
                ++ reference.name


qualifiedToString :
    { qualification : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    -> String
qualifiedToString reference =
    case reference.qualification of
        [] ->
            reference.name

        qualificationPart0 :: qualificationPart1Up ->
            ((qualificationPart0 :: qualificationPart1Up)
                |> String.join "."
            )
                ++ "."
                ++ reference.name


stringFirstCharToLower : String -> String
stringFirstCharToLower string =
    case string |> String.uncons of
        Nothing ->
            ""

        Just ( headChar, tailString ) ->
            String.cons (Char.toLower headChar) tailString


stringFirstCharToUpper : String -> String
stringFirstCharToUpper string =
    case string |> String.uncons of
        Nothing ->
            ""

        Just ( headChar, tailString ) ->
            String.cons (Char.toUpper headChar) tailString


stringFirstCharIsUpper : String -> Bool
stringFirstCharIsUpper string =
    case string |> String.uncons of
        Nothing ->
            False

        Just ( headChar, _ ) ->
            headChar |> Char.isUpper


stringLiteral : String -> Print
stringLiteral stringContent =
    let
        singleDoubleQuotedStringContentEscaped : String
        singleDoubleQuotedStringContentEscaped =
            stringContent
                |> String.foldl
                    (\contentChar soFar ->
                        soFar ++ singleDoubleQuotedStringCharToEscaped contentChar ++ ""
                    )
                    ""
    in
    Print.exactly ("\"" ++ singleDoubleQuotedStringContentEscaped ++ "\"")


singleDoubleQuotedStringCharToEscaped : Char -> String
singleDoubleQuotedStringCharToEscaped character =
    case character of
        '"' ->
            "\\\""

        '\\' ->
            "\\\\"

        '\t' ->
            "\\t"

        '\n' ->
            "\\n"

        '$' ->
            "\\$"

        '\u{000D}' ->
            "\\r"

        otherCharacter ->
            if characterIsNotPrint otherCharacter then
                "\\u{" ++ characterHex otherCharacter ++ "}"

            else
                String.fromChar otherCharacter


intToHexString : Int -> String
intToHexString int =
    -- IGNORE TCO
    if int < 16 then
        unsafeHexDigitIntToString int

    else
        intToHexString (int // 16)
            ++ unsafeHexDigitIntToString (int |> Basics.remainderBy 16)
            ++ ""


unsafeHexDigitIntToString : Int -> String
unsafeHexDigitIntToString int =
    case int of
        0 ->
            "0"

        1 ->
            "1"

        2 ->
            "2"

        3 ->
            "3"

        4 ->
            "4"

        5 ->
            "5"

        6 ->
            "6"

        7 ->
            "7"

        8 ->
            "8"

        9 ->
            "9"

        10 ->
            "A"

        11 ->
            "B"

        12 ->
            "C"

        13 ->
            "D"

        14 ->
            "E"

        -- 15
        _ ->
            "F"


characterHex : Char -> String
characterHex character =
    String.toUpper
        (intToHexString (Char.toCode character)
            |> String.padLeft 8 '0'
        )


characterIsNotPrint : Char -> Bool
characterIsNotPrint character =
    if
        -- Unicode.getCategory is very expensive so we shortcut if at all possible
        charIsLatinAlphaNumOrUnderscoreFast character
            || (character == ' ')
            || (character == '.')
            || (character == '!')
            || (character == '?')
            || (character == '-')
            || (character == ':')
    then
        False

    else
        case Unicode.getCategory character of
            Nothing ->
                True

            Just category ->
                case category of
                    Unicode.SeparatorLine ->
                        True

                    Unicode.SeparatorParagraph ->
                        True

                    Unicode.OtherControl ->
                        True

                    Unicode.OtherFormat ->
                        True

                    Unicode.OtherSurrogate ->
                        True

                    Unicode.OtherPrivateUse ->
                        True

                    Unicode.OtherNotAssigned ->
                        True

                    Unicode.LetterUppercase ->
                        False

                    Unicode.LetterLowercase ->
                        False

                    Unicode.LetterTitlecase ->
                        False

                    Unicode.MarkNonSpacing ->
                        False

                    Unicode.MarkSpacingCombining ->
                        False

                    Unicode.MarkEnclosing ->
                        False

                    Unicode.NumberDecimalDigit ->
                        False

                    Unicode.NumberLetter ->
                        False

                    Unicode.NumberOther ->
                        False

                    Unicode.SeparatorSpace ->
                        True

                    Unicode.LetterModifier ->
                        False

                    Unicode.LetterOther ->
                        False

                    Unicode.PunctuationConnector ->
                        False

                    Unicode.PunctuationDash ->
                        False

                    Unicode.PunctuationOpen ->
                        False

                    Unicode.PunctuationClose ->
                        False

                    Unicode.PunctuationInitialQuote ->
                        False

                    Unicode.PunctuationFinalQuote ->
                        False

                    Unicode.PunctuationOther ->
                        False

                    Unicode.SymbolMath ->
                        False

                    Unicode.SymbolCurrency ->
                        False

                    Unicode.SymbolModifier ->
                        False

                    Unicode.SymbolOther ->
                        False


charLiteral : Char -> String
charLiteral charContent =
    "'"
        ++ quotedCharToEscaped charContent
        ++ "'"


quotedCharToEscaped : Char -> String
quotedCharToEscaped character =
    case character of
        '\'' ->
            "\\'"

        '\\' ->
            "\\\\"

        '\t' ->
            "\\t"

        '\n' ->
            "\\n"

        '\u{000D}' ->
            "\\r"

        otherCharacter ->
            if characterIsNotPrint otherCharacter then
                "\\u{" ++ characterHex otherCharacter ++ "}"

            else
                String.fromChar otherCharacter


charCodeIsLower : Int -> Bool
charCodeIsLower code =
    0x61 <= code && code <= 0x7A


charCodeIsUpper : Int -> Bool
charCodeIsUpper code =
    code <= 0x5A && 0x41 <= code


charCodeIsDigit : Int -> Bool
charCodeIsDigit code =
    code <= 0x39 && 0x30 <= code


charIsLatinAlphaNumOrUnderscoreFast : Char -> Bool
charIsLatinAlphaNumOrUnderscoreFast c =
    let
        code : Int
        code =
            Char.toCode c
    in
    charCodeIsLower code
        || charCodeIsUpper code
        || charCodeIsDigit code
        || -- (c == '_')
           (code == 95)


pattern :
    ModuleContext
    -> Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern
    ->
        Result
            String
            { pattern : OcamlPattern
            , introducedVariables : FastSet.Set String
            }
pattern moduleOriginLookup (Elm.Syntax.Node.Node _ syntaxPattern) =
    -- IGNORE TCO
    case syntaxPattern of
        Elm.Syntax.Pattern.AllPattern ->
            Ok
                { pattern = OcamlPatternIgnore
                , introducedVariables = FastSet.empty
                }

        Elm.Syntax.Pattern.UnitPattern ->
            Ok
                { pattern = OcamlPatternIgnore
                , introducedVariables = FastSet.empty
                }

        Elm.Syntax.Pattern.CharPattern charValue ->
            Ok
                { pattern = OcamlPatternChar charValue
                , introducedVariables = FastSet.empty
                }

        Elm.Syntax.Pattern.StringPattern stringValue ->
            Ok
                { pattern = OcamlPatternString stringValue
                , introducedVariables = FastSet.empty
                }

        Elm.Syntax.Pattern.IntPattern intValue ->
            Ok
                { pattern = OcamlPatternFloat (intValue |> Basics.toFloat)
                , introducedVariables = FastSet.empty
                }

        Elm.Syntax.Pattern.HexPattern intValue ->
            Ok
                { pattern = OcamlPatternFloat (intValue |> Basics.toFloat)
                , introducedVariables = FastSet.empty
                }

        Elm.Syntax.Pattern.FloatPattern _ ->
            Err "float pattern is invalid syntax"

        Elm.Syntax.Pattern.VarPattern variableName ->
            let
                disambiguatedVariableName : String
                disambiguatedVariableName =
                    variableName |> variableNameDisambiguateFromOcamlKeywords
            in
            Ok
                { pattern =
                    OcamlPatternVariable disambiguatedVariableName
                , introducedVariables =
                    FastSet.singleton disambiguatedVariableName
                }

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            pattern moduleOriginLookup inParens

        Elm.Syntax.Pattern.TuplePattern parts ->
            case parts of
                [] ->
                    -- should be covered by UnitPattern
                    Ok
                        { pattern = OcamlPatternIgnore
                        , introducedVariables = FastSet.empty
                        }

                [ inParens ] ->
                    -- should be covered by ParenthesizedPattern
                    pattern moduleOriginLookup inParens

                [ part0Node, part1Node ] ->
                    Result.map2
                        (\part0 part1 ->
                            { pattern =
                                OcamlPatternTuple
                                    { part0 = part0.pattern
                                    , part1 = part1.pattern
                                    , part2Up = []
                                    }
                            , introducedVariables =
                                FastSet.union
                                    part0.introducedVariables
                                    part0.introducedVariables
                            }
                        )
                        (part0Node |> pattern moduleOriginLookup)
                        (part1Node |> pattern moduleOriginLookup)

                [ part0Node, part1Node, part2Node ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            { pattern =
                                OcamlPatternTuple
                                    { part0 = part0.pattern
                                    , part1 = part1.pattern
                                    , part2Up = [ part2.pattern ]
                                    }
                            , introducedVariables =
                                FastSet.union
                                    part0.introducedVariables
                                    (FastSet.union
                                        part0.introducedVariables
                                        part1.introducedVariables
                                    )
                            }
                        )
                        (part0Node |> pattern moduleOriginLookup)
                        (part1Node |> pattern moduleOriginLookup)
                        (part2Node |> pattern moduleOriginLookup)

                _ :: _ :: _ :: _ :: _ ->
                    -- invalid syntax
                    Err "too many tuple parts"

        Elm.Syntax.Pattern.RecordPattern fields ->
            let
                fieldNames : FastSet.Set String
                fieldNames =
                    fields
                        |> listMapAndToFastSet
                            (\(Elm.Syntax.Node.Node _ fieldName) ->
                                fieldName |> variableNameDisambiguateFromOcamlKeywords
                            )
            in
            Ok
                { pattern = OcamlPatternRecordInexhaustive fieldNames
                , introducedVariables = fieldNames
                }

        Elm.Syntax.Pattern.UnConsPattern headNode tailNode ->
            Result.map2
                (\head tail ->
                    { pattern =
                        OcamlPatternListCons
                            { head = head.pattern
                            , tail = tail.pattern
                            }
                    , introducedVariables =
                        FastSet.union
                            head.introducedVariables
                            tail.introducedVariables
                    }
                )
                (headNode |> pattern moduleOriginLookup)
                (tailNode |> pattern moduleOriginLookup)

        Elm.Syntax.Pattern.ListPattern elementPatterns ->
            Result.map
                (\elements ->
                    { pattern =
                        OcamlPatternListExact (elements |> List.map .pattern)
                    , introducedVariables =
                        elements
                            |> listMapToFastSetsAndUnify .introducedVariables
                    }
                )
                (elementPatterns
                    |> listMapAndCombineOk
                        (\element -> element |> pattern moduleOriginLookup)
                )

        Elm.Syntax.Pattern.NamedPattern syntaxQualifiedNameRef argumentPatterns ->
            Result.map2
                (\values reference ->
                    { pattern =
                        OcamlPatternVariant
                            { moduleOrigin = reference.moduleOrigin
                            , name = reference.name
                            , value =
                                case values of
                                    [] ->
                                        Nothing

                                    [ onlyValue ] ->
                                        Just onlyValue.pattern

                                    value0 :: value1 :: value2Up ->
                                        Just
                                            (OcamlPatternTuple
                                                { part0 = value0.pattern
                                                , part1 = value1.pattern
                                                , part2Up =
                                                    value2Up
                                                        |> List.map .pattern
                                                }
                                            )
                            }
                    , introducedVariables =
                        values
                            |> listMapToFastSetsAndUnify .introducedVariables
                    }
                )
                (argumentPatterns
                    |> listMapAndCombineOk
                        (\argument -> argument |> pattern moduleOriginLookup)
                )
                (case moduleOriginLookup.variantLookup |> FastDict.get ( syntaxQualifiedNameRef.moduleName, syntaxQualifiedNameRef.name ) of
                    Nothing ->
                        Err
                            ("could not find origin choice type for the variant "
                                ++ qualifiedToString
                                    { qualification = syntaxQualifiedNameRef.moduleName
                                    , name = syntaxQualifiedNameRef.name
                                    }
                            )

                    Just variantInfo ->
                        Ok
                            (case { moduleOrigin = variantInfo.moduleOrigin, name = syntaxQualifiedNameRef.name } |> referenceToCoreOcaml of
                                Just ocamlReference ->
                                    ocamlReference

                                Nothing ->
                                    { moduleOrigin = Nothing
                                    , name =
                                        referenceToOcamlName
                                            { moduleOrigin = variantInfo.moduleOrigin
                                            , name = syntaxQualifiedNameRef.name
                                            }
                                            |> stringFirstCharToUpper
                                    }
                            )
                )

        Elm.Syntax.Pattern.AsPattern aliasedPatternNode (Elm.Syntax.Node.Node _ variable) ->
            Result.map
                (\aliasedPattern ->
                    let
                        variableDisambiguated : String
                        variableDisambiguated =
                            variable |> variableNameDisambiguateFromOcamlKeywords
                    in
                    { pattern =
                        OcamlPatternAs
                            { pattern = aliasedPattern.pattern
                            , variable = variableDisambiguated
                            }
                    , introducedVariables =
                        aliasedPattern.introducedVariables
                            |> FastSet.insert variableDisambiguated
                    }
                )
                (aliasedPatternNode |> pattern moduleOriginLookup)


referenceToCoreOcaml :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    ->
        Maybe
            { moduleOrigin : Maybe String
            , name : String
            }
referenceToCoreOcaml reference =
    case reference.moduleOrigin of
        [ "Basics" ] ->
            case reference.name of
                "identity" ->
                    Just { moduleOrigin = Just "Fun", name = "id" }

                "always" ->
                    Just { moduleOrigin = Just "Fun", name = "const" }

                "compare" ->
                    Just { moduleOrigin = Nothing, name = "basics_compare" }

                "max" ->
                    Just { moduleOrigin = Just "Stdlib", name = "max" }

                "min" ->
                    Just { moduleOrigin = Just "Stdlib", name = "min" }

                "Order" ->
                    Just { moduleOrigin = Nothing, name = "basics_Order" }

                "LT" ->
                    Just { moduleOrigin = Nothing, name = "Basics_LT" }

                "EQ" ->
                    Just { moduleOrigin = Nothing, name = "Basics_EQ" }

                "GT" ->
                    Just { moduleOrigin = Nothing, name = "Basics_GT" }

                "Bool" ->
                    Just { moduleOrigin = Nothing, name = "bool" }

                "True" ->
                    Just { moduleOrigin = Nothing, name = "true" }

                "False" ->
                    Just { moduleOrigin = Nothing, name = "false" }

                "not" ->
                    Just { moduleOrigin = Just "Bool", name = "not" }

                "xor" ->
                    Just { moduleOrigin = Nothing, name = "basics_neq" }

                "Int" ->
                    Just { moduleOrigin = Nothing, name = "float" }

                "Float" ->
                    Just { moduleOrigin = Nothing, name = "float" }

                "e" ->
                    Just { moduleOrigin = Just "Float", name = "e" }

                "pi" ->
                    Just { moduleOrigin = Just "Float", name = "pi" }

                "ceiling" ->
                    Just { moduleOrigin = Just "Float", name = "ceil" }

                "floor" ->
                    Just { moduleOrigin = Just "Float", name = "floor" }

                "round" ->
                    Just { moduleOrigin = Just "Float", name = "round" }

                "truncate" ->
                    Just { moduleOrigin = Just "Float", name = "trunc" }

                "negate" ->
                    Just { moduleOrigin = Just "Float", name = "neg" }

                "abs" ->
                    Just { moduleOrigin = Just "Float", name = "abs" }

                "toFloat" ->
                    Just { moduleOrigin = Just "Fun", name = "id" }

                "isNaN" ->
                    Just { moduleOrigin = Just "Float", name = "is_nan" }

                "isInfinite" ->
                    Just { moduleOrigin = Just "Float", name = "is_infinite" }

                "remainderBy" ->
                    Just { moduleOrigin = Just "Float", name = "rem" }

                "modBy" ->
                    Just { moduleOrigin = Nothing, name = "basics_modBy" }

                "sin" ->
                    Just { moduleOrigin = Just "Float", name = "sin" }

                "cos" ->
                    Just { moduleOrigin = Just "Float", name = "cos" }

                "tan" ->
                    Just { moduleOrigin = Just "Float", name = "tan" }

                "asin" ->
                    Just { moduleOrigin = Just "Float", name = "asin" }

                "acos" ->
                    Just { moduleOrigin = Just "Float", name = "acos" }

                "atan" ->
                    Just { moduleOrigin = Just "Float", name = "atan" }

                "atan2" ->
                    Just { moduleOrigin = Just "Float", name = "atan2" }

                "sqrt" ->
                    Just { moduleOrigin = Just "Float", name = "sqrt" }

                _ ->
                    Nothing

        [ "String" ] ->
            case reference.name of
                "String" ->
                    Just { moduleOrigin = Nothing, name = "string" }

                "isEmpty" ->
                    Just { moduleOrigin = Nothing, name = "string_isEmpty" }

                "length" ->
                    Just { moduleOrigin = Nothing, name = "string_length" }

                "append" ->
                    Just { moduleOrigin = Just "String", name = "cat" }

                "trim" ->
                    Just { moduleOrigin = Just "String", name = "trim" }

                "trimLeft" ->
                    Just { moduleOrigin = Nothing, name = "string_trimLeft" }

                "trimRight" ->
                    Just { moduleOrigin = Nothing, name = "string_trimRight" }

                "left" ->
                    Just { moduleOrigin = Nothing, name = "string_left" }

                "right" ->
                    Just { moduleOrigin = Nothing, name = "string_right" }

                "dropLeft" ->
                    Just { moduleOrigin = Nothing, name = "string_dropLeft" }

                "dropRight" ->
                    Just { moduleOrigin = Nothing, name = "string_dropRight" }

                "padLeft" ->
                    Just { moduleOrigin = Nothing, name = "string_padLeft" }

                "padRight" ->
                    Just { moduleOrigin = Nothing, name = "string_padRight" }

                "toList" ->
                    Just { moduleOrigin = Nothing, name = "string_toList" }

                "join" ->
                    Just { moduleOrigin = Just "String", name = "concat" }

                "filter" ->
                    Just { moduleOrigin = Nothing, name = "string_filter" }

                "any" ->
                    Just { moduleOrigin = Just "String", name = "exists" }

                "all" ->
                    Just { moduleOrigin = Just "String", name = "for_all" }

                "map" ->
                    Just { moduleOrigin = Just "String", name = "map" }

                "repeat" ->
                    Just { moduleOrigin = Nothing, name = "string_repeat" }

                "lines" ->
                    Just { moduleOrigin = Nothing, name = "string_lines" }

                "startsWith" ->
                    Just { moduleOrigin = Just "String", name = "starts_with" }

                "endsWith" ->
                    Just { moduleOrigin = Just "String", name = "ends_with" }

                "toInt" ->
                    Just { moduleOrigin = Nothing, name = "string_toInt" }

                "toFloat" ->
                    Just { moduleOrigin = Just "Float", name = "of_string_opt" }

                "fromInt" ->
                    Just { moduleOrigin = Just "Float", name = "to_string" }

                "fromFloat" ->
                    Just { moduleOrigin = Just "Float", name = "to_string" }

                "fromChar" ->
                    Just { moduleOrigin = Nothing, name = "string_fromChar" }

                "cons" ->
                    Just { moduleOrigin = Nothing, name = "string_cons" }

                "slice" ->
                    Just { moduleOrigin = Nothing, name = "string_slice" }

                "split" ->
                    Just { moduleOrigin = Nothing, name = "string_split" }

                "contains" ->
                    Just { moduleOrigin = Nothing, name = "string_contains" }

                "toLower" ->
                    Just { moduleOrigin = Just "String", name = "lowercase_ascii" }

                "toUpper" ->
                    Just { moduleOrigin = Just "String", name = "uppercase_ascii" }

                _ ->
                    Nothing

        [ "Char" ] ->
            case reference.name of
                "Char" ->
                    Just { moduleOrigin = Nothing, name = "char" }

                "toCode" ->
                    Just { moduleOrigin = Nothing, name = "char_toCode" }

                "fromCode" ->
                    Just { moduleOrigin = Nothing, name = "char_fromCode" }

                "toLower" ->
                    Just { moduleOrigin = Just "Char", name = "lowercase_ascii" }

                "toUpper" ->
                    Just { moduleOrigin = Just "Char", name = "uppercase_ascii" }

                _ ->
                    Nothing

        [ "List" ] ->
            case reference.name of
                "List" ->
                    Just { moduleOrigin = Nothing, name = "list" }

                "singleton" ->
                    Just { moduleOrigin = Nothing, name = "list_singleton" }

                "isEmpty" ->
                    Just { moduleOrigin = Just "List", name = "is_empty" }

                "length" ->
                    Just { moduleOrigin = Just "List", name = "length" }

                "member" ->
                    Just { moduleOrigin = Just "List", name = "mem" }

                "sum" ->
                    Just { moduleOrigin = Nothing, name = "list_sum" }

                "minimum" ->
                    Just { moduleOrigin = Nothing, name = "list_minimum" }

                "product" ->
                    Just { moduleOrigin = Nothing, name = "list_product" }

                "append" ->
                    Just { moduleOrigin = Just "List", name = "append" }

                "concat" ->
                    Just { moduleOrigin = Just "List", name = "concat" }

                "reverse" ->
                    Just { moduleOrigin = Just "List", name = "rev" }

                "repeat" ->
                    Just { moduleOrigin = Just "List", name = "replicate" }

                "all" ->
                    Just { moduleOrigin = Just "List", name = "for_all" }

                "any" ->
                    Just { moduleOrigin = Just "List", name = "exists" }

                "filter" ->
                    Just { moduleOrigin = Just "List", name = "filter" }

                "filterMap" ->
                    Just { moduleOrigin = Just "List", name = "filter_map" }

                "map" ->
                    Just { moduleOrigin = Just "List", name = "map" }

                "map2" ->
                    Just { moduleOrigin = Just "List", name = "map2" }

                "zip" ->
                    Just { moduleOrigin = Just "List", name = "combine" }

                "unzip" ->
                    Just { moduleOrigin = Just "List", name = "split" }

                "concatMap" ->
                    Just { moduleOrigin = Just "List", name = "concat_map" }

                "sort" ->
                    Just { moduleOrigin = Nothing, name = "list_sort" }

                "sortWith" ->
                    Just { moduleOrigin = Nothing, name = "list_sortWith" }

                "range" ->
                    Just { moduleOrigin = Nothing, name = "list_range" }

                "take" ->
                    Just { moduleOrigin = Nothing, name = "list_take" }

                "drop" ->
                    Just { moduleOrigin = Nothing, name = "list_drop" }

                "intersperse" ->
                    Just { moduleOrigin = Nothing, name = "list_intersperse" }

                "foldl" ->
                    Just { moduleOrigin = Nothing, name = "list_foldl" }

                "foldr" ->
                    Just { moduleOrigin = Nothing, name = "list_foldr" }

                "partition" ->
                    Just { moduleOrigin = Just "List", name = "partition" }

                _ ->
                    Nothing

        [ "Parser" ] ->
            case reference.name of
                -- refers to either a type or variant
                "Problem" ->
                    Just { moduleOrigin = Nothing, name = "Parser_Problem" }

                "Expecting" ->
                    Just { moduleOrigin = Nothing, name = "Parser_Expecting" }

                "ExpectingInt" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingInt" }

                "ExpectingHex" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingHex" }

                "ExpectingOctal" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingOctal" }

                "ExpectingBinary" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingBinary" }

                "ExpectingFloat" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingFloat" }

                "ExpectingNumber" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingNumber" }

                "ExpectingVariable" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingVariable" }

                "ExpectingSymbol" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingSymbol" }

                "ExpectingKeyword" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingKeyword" }

                "ExpectingEnd" ->
                    Just { moduleOrigin = Nothing, name = "Parser_ExpectingEnd" }

                "UnexpectedChar" ->
                    Just { moduleOrigin = Nothing, name = "Parser_UnexpectedChar" }

                "BadRepeat" ->
                    Just { moduleOrigin = Nothing, name = "Parser_BadRepeat" }

                _ ->
                    Nothing

        [ "Maybe" ] ->
            case reference.name of
                "Maybe" ->
                    Just { moduleOrigin = Nothing, name = "option" }

                "Nothing" ->
                    Just { moduleOrigin = Nothing, name = "None" }

                "Just" ->
                    Just { moduleOrigin = Nothing, name = "Some" }

                _ ->
                    Nothing

        _ ->
            Nothing


referenceToOcamlName :
    { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
    , name : String
    }
    -> String
referenceToOcamlName reference =
    (reference.moduleOrigin
        |> String.concat
        |> stringFirstCharToLower
    )
        ++ "_"
        ++ reference.name


printOcamlPatternNotParenthesized : OcamlPattern -> Print
printOcamlPatternNotParenthesized ocamlPattern =
    -- IGNORE TCO
    case ocamlPattern of
        OcamlPatternIgnore ->
            printExactlyUnderscore

        OcamlPatternFloat floatValue ->
            Print.exactly (floatLiteral floatValue)

        OcamlPatternChar charValue ->
            Print.exactly (charLiteral charValue)

        OcamlPatternString string ->
            stringLiteral string

        OcamlPatternVariable name ->
            Print.exactly name

        OcamlPatternListCons ocamlPatternListCons ->
            printOcamlPatternListCons ocamlPatternListCons

        OcamlPatternListExact elements ->
            printOcamlPatternListExact elements

        OcamlPatternRecordInexhaustive recordInexhaustiveFieldNames ->
            Print.exactly "{ "
                |> Print.followedBy
                    (recordInexhaustiveFieldNames
                        |> FastSet.toList
                        |> Print.listMapAndIntersperseAndFlatten
                            Print.exactly
                            (Print.exactly "; ")
                    )
                |> Print.followedBy (Print.exactly "; _ }")

        OcamlPatternVariant patternVariant ->
            Print.exactly patternVariant.name
                |> Print.followedBy
                    (case patternVariant.value of
                        Nothing ->
                            Print.empty

                        Just variantValue ->
                            Print.exactly " "
                                |> Print.followedBy
                                    (printOcamlPatternParenthesizedIfSpaceSeparated
                                        variantValue
                                    )
                    )

        OcamlPatternAs patternAs ->
            printOcamlPatternAs patternAs

        OcamlPatternTuple parts ->
            Print.exactly "( "
                |> Print.followedBy
                    ((parts.part0 :: parts.part1 :: parts.part2Up)
                        |> Print.listMapAndIntersperseAndFlatten
                            printOcamlPatternNotParenthesized
                            (Print.exactly ", ")
                    )
                |> Print.followedBy (Print.exactly " )")


printOcamlPatternListExact : List OcamlPattern -> Print
printOcamlPatternListExact elements =
    case elements of
        [] ->
            Print.exactly "[]"

        element0 :: element1Up ->
            Print.exactly "[ "
                |> Print.followedBy
                    ((element0 :: element1Up)
                        |> Print.listMapAndIntersperseAndFlatten
                            (\elementNode ->
                                Print.withIndentIncreasedBy 2
                                    (printOcamlPatternNotParenthesized elementNode)
                            )
                            (Print.exactly "; ")
                    )
                |> Print.followedBy (Print.exactly " ]")


printOcamlPatternListCons :
    { head : OcamlPattern
    , tail : OcamlPattern
    }
    -> Print
printOcamlPatternListCons syntaxCons =
    printOcamlPatternParenthesizedIfSpaceSeparated
        syntaxCons.head
        |> Print.followedBy
            (Print.exactly " :: ")
        |> Print.followedBy
            (printOcamlPatternParenthesizedIfSpaceSeparated
                syntaxCons.tail
            )


printOcamlPatternAs :
    { variable : String
    , pattern : OcamlPattern
    }
    -> Print
printOcamlPatternAs syntaxAs =
    printOcamlPatternParenthesizedIfSpaceSeparated
        syntaxAs.pattern
        |> Print.followedBy
            (Print.exactly (" as " ++ syntaxAs.variable))


printOcamlExpressionRecord : FastDict.Dict String OcamlExpression -> Print
printOcamlExpressionRecord syntaxRecordFields =
    if syntaxRecordFields |> FastDict.isEmpty then
        Print.exactly "{}"

    else
        let
            fieldsPrint : Print
            fieldsPrint =
                syntaxRecordFields
                    |> FastDict.toList
                    |> Print.listMapAndIntersperseAndFlatten
                        (\( fieldName, fieldValue ) ->
                            let
                                fieldValuePrint : Print
                                fieldValuePrint =
                                    case fieldValue of
                                        OcamlExpressionMatchWith fieldValueMatchWith ->
                                            -- parens are necessary because
                                            -- the last inner match case result would is seen as a **statement**
                                            -- For some reason, this behavior seems to only occur with match and not with e.g. if
                                            printParenthesized
                                                (printOcamlExpressionMatchWith fieldValueMatchWith)

                                        fieldValueNotMatchWith ->
                                            printOcamlExpressionNotParenthesized fieldValueNotMatchWith
                            in
                            Print.withIndentIncreasedBy 2
                                (Print.exactly (fieldName ++ " =")
                                    |> Print.followedBy
                                        (Print.withIndentAtNextMultipleOf4
                                            (Print.spaceOrLinebreakIndented
                                                (fieldValuePrint |> Print.lineSpread)
                                                |> Print.followedBy
                                                    fieldValuePrint
                                            )
                                        )
                                )
                        )
                        (Print.linebreakIndented
                            |> Print.followedBy
                                (Print.exactly "; ")
                        )
        in
        printExactlyCurlyOpeningSpace
            |> Print.followedBy fieldsPrint
            |> Print.followedBy
                (Print.spaceOrLinebreakIndented
                    (fieldsPrint |> Print.lineSpread)
                )
            |> Print.followedBy printExactlyCurlyClosing


printParenthesized : Print -> Print
printParenthesized notParenthesizedPrint =
    printExactlyParensOpening
        |> Print.followedBy
            (Print.withIndentIncreasedBy 1
                notParenthesizedPrint
            )
        |> Print.followedBy
            (Print.emptyOrLinebreakIndented
                (notParenthesizedPrint |> Print.lineSpread)
            )
        |> Print.followedBy printExactlyParensClosing


{-| Transpile a list of [`Elm.Syntax.Declaration.Declaration`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Declaration#Declaration)s
across multiple modules to [`OcamlDeclaration`]s.
Declarations that use unsupported stuff like parser kernel code (directly or indirectly)
will not be present in the final declarations.
Their errors can be found alongside the valid transpiled declarations.

The given list of files must also include files from used dependencies
including `elm/core`.

-}
modules :
    List Elm.Syntax.File.File
    ->
        { errors : List String
        , declarations :
            { valuesAndFunctions :
                FastDict.Dict
                    String
                    { parameters : List OcamlPattern
                    , result : OcamlExpression
                    , type_ : Maybe OcamlType
                    }
            , typeAliases :
                FastDict.Dict
                    String
                    { parameters : List String
                    , type_ : OcamlType
                    }
            , choiceTypes :
                FastDict.Dict
                    String
                    { parameters : List String
                    , variants : FastDict.Dict String (Maybe OcamlType)
                    }
            }
        }
modules syntaxDeclarationsIncludingOverwrittenOnes =
    let
        syntaxModules : List Elm.Syntax.File.File
        syntaxModules =
            syntaxDeclarationsIncludingOverwrittenOnes
                |> List.filter
                    (\syntaxModule ->
                        case
                            syntaxModule.moduleDefinition
                                |> Elm.Syntax.Node.value
                                |> moduleHeaderName
                        of
                            [ "Basics" ] ->
                                False

                            [ "Array" ] ->
                                False

                            -- https://github.com/elm/core/blob/1.0.5/src/Elm/JsArray.elm
                            [ "Elm", "JsArray" ] ->
                                False

                            [ "Bitwise" ] ->
                                False

                            [ "Debug" ] ->
                                False

                            [ "Char" ] ->
                                False

                            [ "String" ] ->
                                False

                            [ "List" ] ->
                                False

                            [ "Dict" ] ->
                                False

                            [ "Set" ] ->
                                False

                            [ "Platform" ] ->
                                False

                            [ "Platform", "Cmd" ] ->
                                False

                            [ "Platform", "Sub" ] ->
                                False

                            [ "Process" ] ->
                                False

                            [ "Task" ] ->
                                False

                            [ "Json", "Decode" ] ->
                                False

                            [ "Json", "Encode" ] ->
                                False

                            [ "Parser" ] ->
                                False

                            [ "Parser", "Advanced" ] ->
                                False

                            [ "Regex" ] ->
                                False

                            [ "File" ] ->
                                False

                            [ "Bytes" ] ->
                                False

                            [ "Bytes", "Encode" ] ->
                                False

                            [ "Bytes", "Decode" ] ->
                                False

                            [ "Http" ] ->
                                False

                            [ "VirtualDom" ] ->
                                False

                            [ "Browser" ] ->
                                False

                            [ "Browser", "Events" ] ->
                                False

                            [ "Browser", "Navigation" ] ->
                                False

                            [ "Browser", "Dom" ] ->
                                False

                            -- https://github.com/elm/browser/blob/master/src/Browser/AnimationManager.elm
                            [ "Browser", "AnimationManager" ] ->
                                False

                            -- https://github.com/elm/browser/tree/master/src/Debugger
                            [ "Debugger", "Expando" ] ->
                                False

                            [ "Debugger", "History" ] ->
                                False

                            [ "Debugger", "Main" ] ->
                                False

                            [ "Debugger", "Metadata" ] ->
                                False

                            [ "Debugger", "Overlay" ] ->
                                False

                            [ "Debugger", "Report" ] ->
                                False

                            [ "Html" ] ->
                                False

                            [ "Html", "Attributes" ] ->
                                False

                            [ "Html", "Events" ] ->
                                False

                            [ "Html", "Keyed" ] ->
                                False

                            [ "Html", "Lazy" ] ->
                                False

                            [ "Svg" ] ->
                                False

                            [ "Svg", "Attributes" ] ->
                                False

                            [ "Svg", "Events" ] ->
                                False

                            [ "Svg", "Keyed" ] ->
                                False

                            [ "Svg", "Lazy" ] ->
                                False

                            [ "Time" ] ->
                                False

                            [ "Random" ] ->
                                False

                            [ "Url" ] ->
                                False

                            [ "Url", "Builder" ] ->
                                False

                            [ "Url", "Parser" ] ->
                                False

                            [ "Url", "Parser", "Query" ] ->
                                False

                            [ "Markdown" ] ->
                                False

                            [ "Benchmark" ] ->
                                False

                            [ "WebGL" ] ->
                                False

                            [ "WebGL", "Settings" ] ->
                                False

                            [ "WebGL", "Settings", "Blend" ] ->
                                False

                            [ "WebGL", "Settings", "DepthTest" ] ->
                                False

                            [ "WebGL", "Settings", "StencilTest" ] ->
                                False

                            [ "WebGL", "Texture" ] ->
                                False

                            [ "Math", "Matrix4" ] ->
                                False

                            [ "Math", "Vector2" ] ->
                                False

                            [ "Math", "Vector3" ] ->
                                False

                            [ "Math", "Vector4" ] ->
                                False

                            _ ->
                                True
                    )

        moduleMembers :
            FastDict.Dict
                Elm.Syntax.ModuleName.ModuleName
                { valueOrFunctionOrTypeAliasNames : FastSet.Set String
                , choiceTypesExposingVariants :
                    FastDict.Dict String (FastDict.Dict String { valueCount : Int })
                }
        moduleMembers =
            syntaxDeclarationsIncludingOverwrittenOnes
                |> List.filter
                    (\syntaxModule ->
                        -- remove those modules we don't have a replacement for, yet
                        case
                            syntaxModule.moduleDefinition
                                |> Elm.Syntax.Node.value
                                |> moduleHeaderName
                        of
                            [ "Array" ] ->
                                False

                            -- https://github.com/elm/core/blob/1.0.5/src/Elm/JsArray.elm
                            [ "Elm", "JsArray" ] ->
                                False

                            [ "Bitwise" ] ->
                                False

                            [ "Debug" ] ->
                                False

                            [ "Set" ] ->
                                False

                            [ "Platform" ] ->
                                False

                            [ "Platform", "Cmd" ] ->
                                False

                            [ "Platform", "Sub" ] ->
                                False

                            [ "Process" ] ->
                                False

                            [ "Task" ] ->
                                False

                            [ "Json", "Decode" ] ->
                                False

                            [ "Json", "Encode" ] ->
                                False

                            [ "Parser" ] ->
                                False

                            [ "Parser", "Advanced" ] ->
                                False

                            [ "Regex" ] ->
                                False

                            [ "File" ] ->
                                False

                            [ "Bytes" ] ->
                                False

                            [ "Bytes", "Encode" ] ->
                                False

                            [ "Bytes", "Decode" ] ->
                                False

                            [ "Http" ] ->
                                False

                            [ "VirtualDom" ] ->
                                False

                            [ "Browser" ] ->
                                False

                            [ "Browser", "Events" ] ->
                                False

                            [ "Browser", "Navigation" ] ->
                                False

                            [ "Browser", "Dom" ] ->
                                False

                            -- https://github.com/elm/browser/blob/master/src/Browser/AnimationManager.elm
                            [ "Browser", "AnimationManager" ] ->
                                False

                            -- https://github.com/elm/browser/tree/master/src/Debugger
                            [ "Debugger", "Expando" ] ->
                                False

                            [ "Debugger", "History" ] ->
                                False

                            [ "Debugger", "Main" ] ->
                                False

                            [ "Debugger", "Metadata" ] ->
                                False

                            [ "Debugger", "Overlay" ] ->
                                False

                            [ "Debugger", "Report" ] ->
                                False

                            [ "Html" ] ->
                                False

                            [ "Html", "Attributes" ] ->
                                False

                            [ "Html", "Events" ] ->
                                False

                            [ "Html", "Keyed" ] ->
                                False

                            [ "Html", "Lazy" ] ->
                                False

                            [ "Svg" ] ->
                                False

                            [ "Svg", "Attributes" ] ->
                                False

                            [ "Svg", "Events" ] ->
                                False

                            [ "Svg", "Keyed" ] ->
                                False

                            [ "Svg", "Lazy" ] ->
                                False

                            [ "Time" ] ->
                                False

                            [ "Random" ] ->
                                False

                            [ "Url" ] ->
                                False

                            [ "Url", "Builder" ] ->
                                False

                            [ "Url", "Parser" ] ->
                                False

                            [ "Url", "Parser", "Query" ] ->
                                False

                            [ "Markdown" ] ->
                                False

                            [ "Benchmark" ] ->
                                False

                            [ "WebGL" ] ->
                                False

                            [ "WebGL", "Settings" ] ->
                                False

                            [ "WebGL", "Settings", "Blend" ] ->
                                False

                            [ "WebGL", "Settings", "DepthTest" ] ->
                                False

                            [ "WebGL", "Settings", "StencilTest" ] ->
                                False

                            [ "WebGL", "Texture" ] ->
                                False

                            [ "Math", "Matrix4" ] ->
                                False

                            [ "Math", "Vector2" ] ->
                                False

                            [ "Math", "Vector3" ] ->
                                False

                            [ "Math", "Vector4" ] ->
                                False

                            _ ->
                                True
                    )
                |> List.foldl
                    (\syntaxModule acrossModulesSoFar ->
                        acrossModulesSoFar
                            |> FastDict.insert
                                (syntaxModule.moduleDefinition
                                    |> Elm.Syntax.Node.value
                                    |> moduleHeaderName
                                )
                                (syntaxModule.declarations
                                    |> List.foldl
                                        (\(Elm.Syntax.Node.Node _ declaration) membersSoFar ->
                                            case declaration of
                                                Elm.Syntax.Declaration.FunctionDeclaration syntaxValueOrFunctionDeclaration ->
                                                    { valueOrFunctionOrTypeAliasNames =
                                                        membersSoFar.valueOrFunctionOrTypeAliasNames
                                                            |> FastSet.insert
                                                                (syntaxValueOrFunctionDeclaration.declaration
                                                                    |> Elm.Syntax.Node.value
                                                                    |> .name
                                                                    |> Elm.Syntax.Node.value
                                                                )
                                                    , choiceTypesExposingVariants =
                                                        membersSoFar.choiceTypesExposingVariants
                                                    }

                                                Elm.Syntax.Declaration.CustomTypeDeclaration syntaxChoiceTypeDeclaration ->
                                                    { valueOrFunctionOrTypeAliasNames =
                                                        membersSoFar.valueOrFunctionOrTypeAliasNames
                                                    , choiceTypesExposingVariants =
                                                        membersSoFar.choiceTypesExposingVariants
                                                            |> FastDict.insert
                                                                (syntaxChoiceTypeDeclaration.name |> Elm.Syntax.Node.value)
                                                                (syntaxChoiceTypeDeclaration.constructors
                                                                    |> List.foldl
                                                                        (\(Elm.Syntax.Node.Node _ variant) variantNamesSoFar ->
                                                                            variantNamesSoFar
                                                                                |> FastDict.insert
                                                                                    (variant.name
                                                                                        |> Elm.Syntax.Node.value
                                                                                    )
                                                                                    { valueCount =
                                                                                        variant.arguments |> List.length
                                                                                    }
                                                                        )
                                                                        FastDict.empty
                                                                )
                                                    }

                                                Elm.Syntax.Declaration.AliasDeclaration typeAlias ->
                                                    { valueOrFunctionOrTypeAliasNames =
                                                        membersSoFar.valueOrFunctionOrTypeAliasNames
                                                            |> FastSet.insert
                                                                (typeAlias.name
                                                                    |> Elm.Syntax.Node.value
                                                                )
                                                    , choiceTypesExposingVariants =
                                                        membersSoFar.choiceTypesExposingVariants
                                                    }

                                                Elm.Syntax.Declaration.PortDeclaration _ ->
                                                    -- not supported
                                                    membersSoFar

                                                Elm.Syntax.Declaration.InfixDeclaration _ ->
                                                    membersSoFar

                                                Elm.Syntax.Declaration.Destructuring _ _ ->
                                                    -- invalid syntax
                                                    membersSoFar
                                        )
                                        { valueOrFunctionOrTypeAliasNames = FastSet.empty
                                        , choiceTypesExposingVariants = FastDict.empty
                                        }
                                )
                    )
                    FastDict.empty

        ocamlDeclarationsWithoutExtraRecordTypeAliases :
            { errors : List String
            , declarations :
                { valuesAndFunctions :
                    FastDict.Dict
                        String
                        { parameters : List OcamlPattern
                        , result : OcamlExpression
                        , type_ : Maybe OcamlType
                        }
                , typeAliases :
                    FastDict.Dict
                        String
                        { parameters : List String
                        , type_ : OcamlType
                        }
                , choiceTypes :
                    FastDict.Dict
                        String
                        { parameters : List String
                        , variants : FastDict.Dict String (Maybe OcamlType)
                        }
                }
            }
        ocamlDeclarationsWithoutExtraRecordTypeAliases =
            syntaxModules
                |> List.foldr
                    (\syntaxModule soFarAcrossModules ->
                        let
                            moduleName : Elm.Syntax.ModuleName.ModuleName
                            moduleName =
                                syntaxModule.moduleDefinition
                                    |> Elm.Syntax.Node.value
                                    |> moduleHeaderName

                            createdModuleContext : ModuleContext
                            createdModuleContext =
                                moduleContextMerge
                                    (syntaxModule.imports |> importsToModuleContext moduleMembers)
                                    (case moduleMembers |> FastDict.get moduleName of
                                        Nothing ->
                                            { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                                                FastDict.empty
                                            , variantLookup = FastDict.empty
                                            }

                                        Just moduleLocalNames ->
                                            { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                                                FastSet.union
                                                    moduleLocalNames.valueOrFunctionOrTypeAliasNames
                                                    (moduleLocalNames.choiceTypesExposingVariants
                                                        |> FastDict.foldl
                                                            (\choiceTypeName _ soFar ->
                                                                soFar |> FastSet.insert choiceTypeName
                                                            )
                                                            FastSet.empty
                                                    )
                                                    |> FastSet.foldl
                                                        (\name soFar ->
                                                            soFar
                                                                |> FastDict.insert ( [], name )
                                                                    moduleName
                                                        )
                                                        FastDict.empty
                                            , variantLookup =
                                                moduleLocalNames.choiceTypesExposingVariants
                                                    |> FastDict.foldl
                                                        (\_ variantNames soFarAcrossChoiceTypes ->
                                                            variantNames
                                                                |> FastDict.foldl
                                                                    (\name info soFar ->
                                                                        soFar
                                                                            |> FastDict.insert ( [], name )
                                                                                { moduleOrigin = moduleName
                                                                                , valueCount = info.valueCount
                                                                                }
                                                                    )
                                                                    soFarAcrossChoiceTypes
                                                        )
                                                        FastDict.empty
                                            }
                                    )
                        in
                        syntaxModule.declarations
                            |> List.foldr
                                (\(Elm.Syntax.Node.Node _ declaration) soFar ->
                                    case declaration of
                                        Elm.Syntax.Declaration.FunctionDeclaration syntaxValueOrFunctionDeclaration ->
                                            case syntaxValueOrFunctionDeclaration |> valueOrFunctionDeclaration createdModuleContext of
                                                Ok ocamlValueOrFunctionDeclaration ->
                                                    { errors = soFar.errors
                                                    , declarations =
                                                        { typeAliases = soFar.declarations.typeAliases
                                                        , choiceTypes = soFar.declarations.choiceTypes
                                                        , valuesAndFunctions =
                                                            soFar.declarations.valuesAndFunctions
                                                                |> FastDict.insert
                                                                    ({ moduleOrigin = moduleName
                                                                     , name = ocamlValueOrFunctionDeclaration.name
                                                                     }
                                                                        |> referenceToOcamlName
                                                                    )
                                                                    { parameters = ocamlValueOrFunctionDeclaration.parameters
                                                                    , result = ocamlValueOrFunctionDeclaration.result
                                                                    , type_ = ocamlValueOrFunctionDeclaration.type_
                                                                    }
                                                        }
                                                    }

                                                Err error ->
                                                    { declarations = soFar.declarations
                                                    , errors = error :: soFar.errors
                                                    }

                                        Elm.Syntax.Declaration.AliasDeclaration syntaxTypeAliasDeclaration ->
                                            case syntaxTypeAliasDeclaration |> typeAliasDeclaration createdModuleContext of
                                                Ok ocamlTypeAliasDeclaration ->
                                                    { errors = soFar.errors
                                                    , declarations =
                                                        { valuesAndFunctions = soFar.declarations.valuesAndFunctions
                                                        , choiceTypes = soFar.declarations.choiceTypes
                                                        , typeAliases =
                                                            soFar.declarations.typeAliases
                                                                |> FastDict.insert
                                                                    ({ moduleOrigin = moduleName
                                                                     , name = ocamlTypeAliasDeclaration.name
                                                                     }
                                                                        |> referenceToOcamlName
                                                                    )
                                                                    { parameters = ocamlTypeAliasDeclaration.parameters
                                                                    , type_ = ocamlTypeAliasDeclaration.type_
                                                                    }
                                                        }
                                                    }

                                                Err error ->
                                                    { declarations = soFar.declarations
                                                    , errors = error :: soFar.errors
                                                    }

                                        Elm.Syntax.Declaration.CustomTypeDeclaration syntaxChoiceTypeDeclaration ->
                                            case syntaxChoiceTypeDeclaration.name |> Elm.Syntax.Node.value of
                                                "Maybe" ->
                                                    soFar

                                                _ ->
                                                    case syntaxChoiceTypeDeclaration |> choiceTypeDeclaration createdModuleContext of
                                                        Ok ocamlTypeAliasDeclaration ->
                                                            { errors = soFar.errors
                                                            , declarations =
                                                                { valuesAndFunctions = soFar.declarations.valuesAndFunctions
                                                                , typeAliases = soFar.declarations.typeAliases
                                                                , choiceTypes =
                                                                    soFar.declarations.choiceTypes
                                                                        |> FastDict.insert
                                                                            ({ moduleOrigin = moduleName
                                                                             , name = ocamlTypeAliasDeclaration.name
                                                                             }
                                                                                |> referenceToOcamlName
                                                                            )
                                                                            { parameters = ocamlTypeAliasDeclaration.parameters
                                                                            , variants =
                                                                                ocamlTypeAliasDeclaration.variants
                                                                                    |> FastDict.foldl
                                                                                        (\variantName maybeValue variantsSoFar ->
                                                                                            variantsSoFar
                                                                                                |> FastDict.insert
                                                                                                    ({ moduleOrigin = moduleName
                                                                                                     , name = variantName
                                                                                                     }
                                                                                                        |> referenceToOcamlName
                                                                                                        |> stringFirstCharToUpper
                                                                                                    )
                                                                                                    maybeValue
                                                                                        )
                                                                                        FastDict.empty
                                                                            }
                                                                }
                                                            }

                                                        Err error ->
                                                            { declarations = soFar.declarations
                                                            , errors = error :: soFar.errors
                                                            }

                                        Elm.Syntax.Declaration.PortDeclaration _ ->
                                            soFar

                                        Elm.Syntax.Declaration.InfixDeclaration _ ->
                                            soFar

                                        Elm.Syntax.Declaration.Destructuring _ _ ->
                                            soFar
                                )
                                soFarAcrossModules
                    )
                    { errors = []
                    , declarations =
                        { valuesAndFunctions = FastDict.empty
                        , typeAliases = FastDict.empty
                        , choiceTypes = FastDict.empty
                        }
                    }

        additionalRecordTypeAliases :
            FastDict.Dict
                String
                { parameters : List String
                , type_ : OcamlType
                }
        additionalRecordTypeAliases =
            FastSet.union
                (ocamlDeclarationsWithoutExtraRecordTypeAliases.declarations.valuesAndFunctions
                    |> fastDictMapToFastSetAndUnify
                        (\valueOrFunctionInfo ->
                            FastSet.union
                                (valueOrFunctionInfo.result
                                    |> ocamlExpressionContainedConstructedRecords
                                )
                                (case valueOrFunctionInfo.type_ of
                                    Nothing ->
                                        FastSet.empty

                                    Just valueOrFunctionType ->
                                        valueOrFunctionType |> ocamlTypeContainedRecords
                                )
                        )
                )
                (FastSet.union
                    (ocamlDeclarationsWithoutExtraRecordTypeAliases.declarations.typeAliases
                        |> fastDictMapToFastSetAndUnify
                            (\typeAliasInfo ->
                                typeAliasInfo.type_
                                    |> ocamlTypeContainedRecords
                            )
                    )
                    (ocamlDeclarationsWithoutExtraRecordTypeAliases.declarations.choiceTypes
                        |> fastDictMapToFastSetAndUnify
                            (\choiceTypeInfo ->
                                choiceTypeInfo.variants
                                    |> fastDictMapToFastSetAndUnify
                                        (\maybeVariantValue ->
                                            case maybeVariantValue of
                                                Nothing ->
                                                    FastSet.empty

                                                Just variantValue ->
                                                    variantValue |> ocamlTypeContainedRecords
                                        )
                            )
                    )
                )
                |> FastSet.foldl
                    (\recordFields soFar ->
                        soFar
                            |> FastDict.insert
                                (generatedOcamlRecordTypeAliasName recordFields)
                                { parameters = recordFields
                                , type_ =
                                    OcamlTypeRecord
                                        (recordFields
                                            |> List.map
                                                (\field ->
                                                    ( field, OcamlTypeVariable field )
                                                )
                                            |> FastDict.fromList
                                        )
                                }
                    )
                    FastDict.empty
    in
    { declarations =
        { valuesAndFunctions =
            ocamlDeclarationsWithoutExtraRecordTypeAliases.declarations.valuesAndFunctions
                |> FastDict.map
                    (\_ valueOrFunctionInfo ->
                        { type_ =
                            valueOrFunctionInfo.type_
                                |> Maybe.map
                                    ocamlTypeReplaceRecordsByGeneratedAliases
                        , parameters = valueOrFunctionInfo.parameters
                        , result = valueOrFunctionInfo.result
                        }
                    )
        , choiceTypes =
            ocamlDeclarationsWithoutExtraRecordTypeAliases.declarations.choiceTypes
                |> FastDict.map
                    (\_ typeAliasInfo ->
                        { parameters = typeAliasInfo.parameters
                        , variants =
                            typeAliasInfo.variants
                                |> FastDict.map
                                    (\_ variantValues ->
                                        variantValues
                                            |> Maybe.map
                                                ocamlTypeReplaceRecordsByGeneratedAliases
                                    )
                        }
                    )
        , typeAliases =
            FastDict.union
                additionalRecordTypeAliases
                (ocamlDeclarationsWithoutExtraRecordTypeAliases.declarations.typeAliases
                    |> FastDict.map
                        (\_ typeAliasInfo ->
                            { parameters = typeAliasInfo.parameters
                            , type_ =
                                typeAliasInfo.type_
                                    |> ocamlTypeReplaceRecordsByGeneratedAliases
                            }
                        )
                )
        }
    , errors = ocamlDeclarationsWithoutExtraRecordTypeAliases.errors
    }


fastDictMapToFastSetAndUnify :
    (value -> FastSet.Set comparableFastSetElement)
    -> FastDict.Dict key_ value
    -> FastSet.Set comparableFastSetElement
fastDictMapToFastSetAndUnify valueToFastSet fastDict =
    fastDict
        |> FastDict.foldl
            (\_ value soFar ->
                FastSet.union
                    (value |> valueToFastSet)
                    soFar
            )
            FastSet.empty


generatedOcamlRecordTypeAliasName : List String -> String
generatedOcamlRecordTypeAliasName recordFields =
    "generated_" ++ (recordFields |> String.join "_")


moduleHeaderName : Elm.Syntax.Module.Module -> Elm.Syntax.ModuleName.ModuleName
moduleHeaderName moduleHeader =
    case moduleHeader of
        Elm.Syntax.Module.NormalModule header ->
            header.moduleName |> Elm.Syntax.Node.value

        Elm.Syntax.Module.PortModule header ->
            header.moduleName |> Elm.Syntax.Node.value

        Elm.Syntax.Module.EffectModule header ->
            header.moduleName |> Elm.Syntax.Node.value


valueOrFunctionDeclaration :
    ModuleContext
    -> Elm.Syntax.Expression.Function
    ->
        Result
            String
            { name : String
            , parameters : List OcamlPattern
            , result : OcamlExpression
            , type_ : Maybe OcamlType
            }
valueOrFunctionDeclaration moduleOriginLookup syntaxDeclarationValueOrFunction =
    let
        implementation : Elm.Syntax.Expression.FunctionImplementation
        implementation =
            syntaxDeclarationValueOrFunction.declaration
                |> Elm.Syntax.Node.value
    in
    resultAndThen2
        (\parameters maybeType ->
            Result.map
                (\result ->
                    { name =
                        implementation.name
                            |> Elm.Syntax.Node.value
                    , type_ = maybeType
                    , parameters =
                        parameters
                            |> List.map .pattern
                    , result = result
                    }
                )
                (implementation.expression
                    |> expression
                        { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                            moduleOriginLookup.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
                        , variantLookup =
                            moduleOriginLookup.variantLookup
                        , variablesFromWithinDeclarationInScope =
                            parameters
                                |> listMapToFastSetsAndUnify .introducedVariables
                        }
                )
        )
        (implementation.arguments
            |> listMapAndCombineOk (\p -> p |> pattern moduleOriginLookup)
        )
        (case syntaxDeclarationValueOrFunction.signature of
            Nothing ->
                Ok Nothing

            Just (Elm.Syntax.Node.Node _ signature) ->
                Result.map Just
                    (signature.typeAnnotation
                        |> type_ moduleOriginLookup
                    )
        )


variableNameDisambiguateFromOcamlKeywords : String -> String
variableNameDisambiguateFromOcamlKeywords variableName =
    if ocamlKeywords |> FastSet.member variableName then
        variableName ++ "_"

    else
        variableName


ocamlKeywords : FastSet.Set String
ocamlKeywords =
    -- https://ocaml.org/manual/5.2/lex.html#sec84
    FastSet.fromList
        [ "and"
        , "as"
        , "assert"
        , "asr"
        , "begin"
        , "class"
        , "constraint"
        , "do"
        , "done"
        , "downto"
        , "else"
        , "end"
        , "exception"
        , "external"
        , "false"
        , "for"
        , "fun"
        , "function"
        , "functor"
        , "if"
        , "in"
        , "include"
        , "inherit"
        , "initializer"
        , "land"
        , "lazy"
        , "let"
        , "lor"
        , "lsl"
        , "lsr"
        , "lxor"
        , "match"
        , "method"
        , "mod"
        , "module"
        , "mutable"
        , "new"
        , "nonrec"
        , "object"
        , "of"
        , "open"
        , "or"
        , "private"
        , "rec"
        , "sig"
        , "struct"
        , "then"
        , "to"
        , "true"
        , "try"
        , "type"
        , "val"
        , "virtual"
        , "when"
        , "while"
        , "with"
        , "value"
        , "parser"
        ]


expressionContextAddVariablesInScope :
    FastSet.Set String
    ->
        { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                Elm.Syntax.ModuleName.ModuleName
        , variantLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                , valueCount : Int
                }
        , variablesFromWithinDeclarationInScope : FastSet.Set String
        }
    ->
        { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                Elm.Syntax.ModuleName.ModuleName
        , variantLookup :
            FastDict.Dict
                ( Elm.Syntax.ModuleName.ModuleName, String )
                { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
                , valueCount : Int
                }
        , variablesFromWithinDeclarationInScope : FastSet.Set String
        }
expressionContextAddVariablesInScope additionalVariablesInScope context =
    { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
        context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
    , variantLookup =
        context.variantLookup
    , variablesFromWithinDeclarationInScope =
        FastSet.union
            additionalVariablesInScope
            context.variablesFromWithinDeclarationInScope
    }


expression :
    { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression
    -> Result String OcamlExpression
expression context (Elm.Syntax.Node.Node _ syntaxExpression) =
    -- IGNORE TCO
    case syntaxExpression of
        Elm.Syntax.Expression.UnitExpr ->
            Ok OcamlExpressionUnit

        Elm.Syntax.Expression.Integer intValue ->
            Ok (OcamlExpressionFloat (intValue |> Basics.toFloat))

        Elm.Syntax.Expression.Hex intValue ->
            Ok (OcamlExpressionFloat (intValue |> Basics.toFloat))

        Elm.Syntax.Expression.Floatable floatValue ->
            Ok (OcamlExpressionFloat floatValue)

        Elm.Syntax.Expression.CharLiteral charValue ->
            Ok (OcamlExpressionChar charValue)

        Elm.Syntax.Expression.Literal stringValue ->
            Ok (OcamlExpressionString stringValue)

        Elm.Syntax.Expression.RecordAccessFunction fieldName ->
            let
                recordVariableName : String
                recordVariableName =
                    "generated_record"
            in
            Ok
                (OcamlExpressionLambda
                    { parameter0 = OcamlPatternVariable recordVariableName
                    , parameter1Up = []
                    , result =
                        OcamlExpressionRecordAccess
                            { record =
                                OcamlExpressionReference
                                    { moduleOrigin = Nothing
                                    , name = recordVariableName
                                    }
                            , field =
                                fieldName
                                    |> String.replace "." ""
                                    |> variableNameDisambiguateFromOcamlKeywords
                            }
                    }
                )

        Elm.Syntax.Expression.Operator _ ->
            -- invalid syntax
            Err "operator is invalid syntax"

        Elm.Syntax.Expression.PrefixOperator operatorSymbol ->
            Result.map
                (\operationFunctionReference ->
                    OcamlExpressionReference operationFunctionReference
                )
                (expressionOperatorToOcamlFunctionReference operatorSymbol)

        Elm.Syntax.Expression.GLSLExpression _ ->
            Err "glsl not supported"

        Elm.Syntax.Expression.Application application ->
            case application of
                [] ->
                    Err "application without any parts is invalid"

                [ inParens ] ->
                    -- invalid syntax
                    expression context inParens

                calledNode :: argument0Node :: argument1UpNodes ->
                    Result.map3
                        (\called argument0 argument1Up ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument0
                                , argument1Up = argument1Up
                                }
                        )
                        (calledNode |> expression context)
                        (argument0Node |> expression context)
                        (argument1UpNodes
                            |> listMapAndCombineOk
                                (\argument -> argument |> expression context)
                        )

        Elm.Syntax.Expression.OperatorApplication operatorSymbol _ leftNode rightNode ->
            case operatorSymbol of
                "|>" ->
                    Result.map2
                        (\argument called ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument
                                , argument1Up = []
                                }
                        )
                        (leftNode |> expression context)
                        (rightNode |> expression context)

                "<|" ->
                    Result.map2
                        (\called argument ->
                            condenseExpressionCall
                                { called = called
                                , argument0 = argument
                                , argument1Up = []
                                }
                        )
                        (leftNode |> expression context)
                        (rightNode |> expression context)

                "++" ->
                    Result.map2
                        (\left right ->
                            if
                                (left |> ocamlExpressionIsDefinitelyOfTypeString)
                                    || (right |> ocamlExpressionIsDefinitelyOfTypeString)
                            then
                                OcamlExpressionCall
                                    { called =
                                        OcamlExpressionReference
                                            { moduleOrigin = Just "String"
                                            , name = "cat"
                                            }
                                    , argument0 = left
                                    , argument1Up = [ right ]
                                    }

                            else
                                OcamlExpressionCall
                                    { called =
                                        OcamlExpressionReference
                                            { moduleOrigin = Just "List"
                                            , name = "append"
                                            }
                                    , argument0 = left
                                    , argument1Up = [ right ]
                                    }
                        )
                        (leftNode |> expression context)
                        (rightNode |> expression context)

                otherOperatorSymbol ->
                    Result.map3
                        (\operationFunctionReference left right ->
                            OcamlExpressionCall
                                { called =
                                    OcamlExpressionReference operationFunctionReference
                                , argument0 = left
                                , argument1Up = [ right ]
                                }
                        )
                        (expressionOperatorToOcamlFunctionReference otherOperatorSymbol)
                        (leftNode |> expression context)
                        (rightNode |> expression context)

        Elm.Syntax.Expression.FunctionOrValue qualification name ->
            let
                asVariableFromWithinDeclaration : Maybe String
                asVariableFromWithinDeclaration =
                    case qualification of
                        _ :: _ ->
                            Nothing

                        [] ->
                            let
                                ocamlName : String
                                ocamlName =
                                    name |> variableNameDisambiguateFromOcamlKeywords
                            in
                            if
                                context.variablesFromWithinDeclarationInScope
                                    |> FastSet.member ocamlName
                            then
                                Just ocamlName

                            else
                                Nothing
            in
            case asVariableFromWithinDeclaration of
                Just variableFromWithinDeclaration ->
                    Ok
                        (OcamlExpressionReference
                            { moduleOrigin = Nothing
                            , name = variableFromWithinDeclaration
                            }
                        )

                Nothing ->
                    case context.variantLookup |> FastDict.get ( qualification, name ) of
                        Just variantInfo ->
                            let
                                reference : { moduleOrigin : Maybe String, name : String }
                                reference =
                                    case { moduleOrigin = variantInfo.moduleOrigin, name = name } |> referenceToCoreOcaml of
                                        Just ocamlReference ->
                                            ocamlReference

                                        Nothing ->
                                            { moduleOrigin = Nothing
                                            , name =
                                                referenceToOcamlName
                                                    { moduleOrigin = variantInfo.moduleOrigin
                                                    , name = name
                                                    }
                                                    |> stringFirstCharToUpper
                                            }
                            in
                            Ok
                                (case variantInfo.valueCount of
                                    0 ->
                                        OcamlExpressionReference reference

                                    1 ->
                                        OcamlExpressionReference reference

                                    valueCountAtLeast2 ->
                                        let
                                            generatedValueVariableReference : Int -> OcamlExpression
                                            generatedValueVariableReference valueIndex =
                                                OcamlExpressionReference
                                                    { moduleOrigin = Nothing
                                                    , name =
                                                        "generated_"
                                                            ++ (valueIndex |> String.fromInt)
                                                    }

                                            generatedValuePattern : Int -> OcamlPattern
                                            generatedValuePattern valueIndex =
                                                OcamlPatternVariable
                                                    ("generated_"
                                                        ++ (valueIndex |> String.fromInt)
                                                    )
                                        in
                                        OcamlExpressionLambda
                                            { parameter0 = generatedValuePattern 0
                                            , parameter1Up =
                                                generatedValuePattern 1
                                                    :: (List.range 2 (valueCountAtLeast2 - 1)
                                                            |> List.map generatedValuePattern
                                                       )
                                            , result =
                                                OcamlExpressionCall
                                                    { called = OcamlExpressionReference reference
                                                    , argument0 =
                                                        OcamlExpressionTuple
                                                            { part0 = generatedValueVariableReference 0
                                                            , part1 = generatedValueVariableReference 1
                                                            , part2Up =
                                                                List.range 2 (valueCountAtLeast2 - 1)
                                                                    |> List.map generatedValueVariableReference
                                                            }
                                                    , argument1Up = []
                                                    }
                                            }
                                )

                        Nothing ->
                            case context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup |> FastDict.get ( qualification, name ) of
                                Just moduleOrigin ->
                                    Ok
                                        (OcamlExpressionReference
                                            (case { moduleOrigin = moduleOrigin, name = name } |> referenceToCoreOcaml of
                                                Just ocamlReference ->
                                                    ocamlReference

                                                Nothing ->
                                                    { moduleOrigin = Nothing
                                                    , name =
                                                        -- TODO should be redundant because variant check
                                                        if name |> stringFirstCharIsUpper then
                                                            referenceToOcamlName
                                                                { moduleOrigin = moduleOrigin
                                                                , name = name
                                                                }
                                                                |> stringFirstCharToUpper

                                                        else
                                                            referenceToOcamlName
                                                                { moduleOrigin = moduleOrigin
                                                                , name = name
                                                                }
                                                    }
                                            )
                                        )

                                Nothing ->
                                    case qualification of
                                        qualificationPart0 :: qualificationPart1Up ->
                                            Err
                                                ("could not find module origin of the qualified reference "
                                                    ++ (((qualificationPart0 :: qualificationPart1Up) |> String.join ".")
                                                            ++ "."
                                                            ++ name
                                                       )
                                                )

                                        [] ->
                                            -- TODO convert to error
                                            Ok
                                                (OcamlExpressionReference
                                                    { moduleOrigin = Nothing
                                                    , name = name |> variableNameDisambiguateFromOcamlKeywords
                                                    }
                                                )

        Elm.Syntax.Expression.IfBlock conditionNode onTrueNode onFalseNode ->
            Result.map3
                (\condition onTrue onFalse ->
                    OcamlExpressionIfThenElse
                        { condition = condition
                        , onTrue = onTrue
                        , onFalse = onFalse
                        }
                )
                (conditionNode |> expression context)
                (onTrueNode |> expression context)
                (onFalseNode |> expression context)

        Elm.Syntax.Expression.ParenthesizedExpression inParens ->
            inParens |> expression context

        Elm.Syntax.Expression.Negation inNegationNode ->
            Result.map
                (\inNegation ->
                    OcamlExpressionCall
                        { called =
                            OcamlExpressionReference
                                { moduleOrigin = Just "Float", name = "neg" }
                        , argument0 = inNegation
                        , argument1Up = []
                        }
                )
                (inNegationNode |> expression context)

        Elm.Syntax.Expression.RecordAccess recordNode (Elm.Syntax.Node.Node _ fieldName) ->
            Result.map
                (\record ->
                    OcamlExpressionRecordAccess
                        { record = record
                        , field =
                            fieldName
                                |> String.replace "." ""
                                |> variableNameDisambiguateFromOcamlKeywords
                        }
                )
                (recordNode |> expression context)

        Elm.Syntax.Expression.TupledExpression parts ->
            case parts of
                [] ->
                    -- invalid syntax
                    -- should be handled by Elm.Syntax.Expression.UnitExpr
                    Ok OcamlExpressionUnit

                [ inParens ] ->
                    -- invalid syntax
                    -- should be handled by Elm.Syntax.Expression.ParenthesizedExpression
                    expression context inParens

                [ part0Node, part1Node ] ->
                    Result.map2
                        (\part0 part1 ->
                            OcamlExpressionTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = []
                                }
                        )
                        (part0Node |> expression context)
                        (part1Node |> expression context)

                [ part0Node, part1Node, part2Node ] ->
                    Result.map3
                        (\part0 part1 part2 ->
                            OcamlExpressionTuple
                                { part0 = part0
                                , part1 = part1
                                , part2Up = [ part2 ]
                                }
                        )
                        (part0Node |> expression context)
                        (part1Node |> expression context)
                        (part2Node |> expression context)

                _ :: _ :: _ :: _ :: _ ->
                    Err "too many tuple parts"

        Elm.Syntax.Expression.ListExpr elementNodes ->
            Result.map (\elements -> OcamlExpressionList elements)
                (elementNodes
                    |> listMapAndCombineOk
                        (\element -> element |> expression context)
                )

        Elm.Syntax.Expression.RecordExpr fieldNodes ->
            Result.map (\fields -> OcamlExpressionRecord fields)
                (fieldNodes
                    |> listMapAndCombineOk
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, fieldValueNode )) ->
                            Result.map
                                (\fieldValue ->
                                    ( fieldName
                                        |> variableNameDisambiguateFromOcamlKeywords
                                    , fieldValue
                                    )
                                )
                                (fieldValueNode |> expression context)
                        )
                    |> Result.map FastDict.fromList
                )

        Elm.Syntax.Expression.RecordUpdateExpression (Elm.Syntax.Node.Node _ originalRecordVariable) fieldNodes ->
            Result.map
                (\fields ->
                    OcamlExpressionRecordUpdate
                        { originalRecordVariable =
                            referenceToOcamlName
                                { moduleOrigin =
                                    case context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup |> FastDict.get ( [], originalRecordVariable ) of
                                        Nothing ->
                                            []

                                        Just moduleOrigin ->
                                            moduleOrigin
                                , name = originalRecordVariable |> variableNameDisambiguateFromOcamlKeywords
                                }
                        , fields = fields
                        }
                )
                (fieldNodes
                    |> listMapAndCombineOk
                        (\(Elm.Syntax.Node.Node _ ( Elm.Syntax.Node.Node _ fieldName, fieldValueNode )) ->
                            Result.map
                                (\fieldValue ->
                                    ( fieldName, fieldValue )
                                )
                                (fieldValueNode |> expression context)
                        )
                    |> Result.map FastDict.fromList
                )

        Elm.Syntax.Expression.LambdaExpression lambda ->
            case lambda.args of
                [] ->
                    Err "lambda without parameters is invalid syntax"

                parameter0Node :: parameter1UpNodes ->
                    resultAndThen2
                        (\parameter0 parameter1Up ->
                            Result.map
                                (\result ->
                                    OcamlExpressionLambda
                                        { parameter0 = parameter0.pattern
                                        , parameter1Up =
                                            parameter1Up |> List.map .pattern
                                        , result =
                                            result
                                        }
                                )
                                (lambda.expression
                                    |> expression
                                        (context
                                            |> expressionContextAddVariablesInScope
                                                (FastSet.union
                                                    parameter0.introducedVariables
                                                    (parameter1Up
                                                        |> listMapToFastSetsAndUnify .introducedVariables
                                                    )
                                                )
                                        )
                                )
                        )
                        (parameter0Node
                            |> pattern
                                { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                                    context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
                                , variantLookup = context.variantLookup
                                }
                        )
                        (parameter1UpNodes
                            |> listMapAndCombineOk
                                (\parameter ->
                                    parameter
                                        |> pattern
                                            { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                                                context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
                                            , variantLookup = context.variantLookup
                                            }
                                )
                        )

        Elm.Syntax.Expression.CaseExpression caseOf ->
            case caseOf.cases of
                [] ->
                    Err "case-of without cases invalid syntax"

                case0Node :: case1Node ->
                    Result.map3
                        (\matched case0 case1Up ->
                            OcamlExpressionMatchWith
                                { matched = matched
                                , case0 = case0
                                , case1Up = case1Up
                                }
                        )
                        (caseOf.expression |> expression context)
                        (case0Node |> case_ context)
                        (case1Node
                            |> listMapAndCombineOk
                                (\parameter ->
                                    parameter |> case_ context
                                )
                        )

        Elm.Syntax.Expression.LetExpression letIn ->
            case letIn.declarations of
                [] ->
                    Err "let-in without declarations is invalid syntax"

                declaration0Node :: declaration1UpNode ->
                    let
                        variablesForWholeLetIn : FastSet.Set String
                        variablesForWholeLetIn =
                            (declaration0Node :: declaration1UpNode)
                                |> listMapToFastSetsAndUnify
                                    (\(Elm.Syntax.Node.Node _ syntaxLetDeclaration) ->
                                        case syntaxLetDeclaration of
                                            Elm.Syntax.Expression.LetFunction letFunction ->
                                                FastSet.singleton
                                                    (letFunction.declaration
                                                        |> Elm.Syntax.Node.value
                                                        |> .name
                                                        |> Elm.Syntax.Node.value
                                                        |> variableNameDisambiguateFromOcamlKeywords
                                                    )

                                            Elm.Syntax.Expression.LetDestructuring patternNode _ ->
                                                patternNode
                                                    |> patternBindings
                                                    |> listMapAndToFastSet
                                                        variableNameDisambiguateFromOcamlKeywords
                                    )
                    in
                    Result.map3
                        (\declaration0 declaration1Up result ->
                            OcamlExpressionLetIn
                                { declaration0 = declaration0
                                , declaration1Up = declaration1Up
                                , result = result
                                }
                        )
                        (declaration0Node
                            |> letDeclaration
                                (context
                                    |> expressionContextAddVariablesInScope
                                        variablesForWholeLetIn
                                )
                        )
                        (declaration1UpNode
                            |> listMapAndCombineOk
                                (\letDecl ->
                                    letDecl
                                        |> letDeclaration
                                            (context
                                                |> expressionContextAddVariablesInScope
                                                    variablesForWholeLetIn
                                            )
                                )
                        )
                        (letIn.expression
                            |> expression
                                (context
                                    |> expressionContextAddVariablesInScope
                                        variablesForWholeLetIn
                                )
                        )


{-| Recursively find all introduced variables
in the [pattern](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/Elm-Syntax-Pattern)
(like `a` and `b` in `( Just a, { b } )`)
-}
patternBindings : Elm.Syntax.Node.Node Elm.Syntax.Pattern.Pattern -> List String
patternBindings (Elm.Syntax.Node.Node _ syntaxPattern) =
    -- IGNORE TCO
    case syntaxPattern of
        Elm.Syntax.Pattern.VarPattern name ->
            [ name ]

        Elm.Syntax.Pattern.AsPattern afterAsPattern (Elm.Syntax.Node.Node _ name) ->
            name :: (afterAsPattern |> patternBindings)

        Elm.Syntax.Pattern.ParenthesizedPattern inParens ->
            inParens |> patternBindings

        Elm.Syntax.Pattern.ListPattern patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.TuplePattern patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.RecordPattern fields ->
            fields |> List.map Elm.Syntax.Node.value

        Elm.Syntax.Pattern.NamedPattern _ patterns ->
            patterns |> List.concatMap patternBindings

        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            (tailPattern |> patternBindings) ++ (headPattern |> patternBindings)

        Elm.Syntax.Pattern.AllPattern ->
            []

        Elm.Syntax.Pattern.UnitPattern ->
            []

        Elm.Syntax.Pattern.CharPattern _ ->
            []

        Elm.Syntax.Pattern.StringPattern _ ->
            []

        Elm.Syntax.Pattern.IntPattern _ ->
            []

        Elm.Syntax.Pattern.HexPattern _ ->
            []

        Elm.Syntax.Pattern.FloatPattern _ ->
            []


resultAndThen2 :
    (a -> b -> Result error c)
    -> Result error a
    -> Result error b
    -> Result error c
resultAndThen2 abToResult aResult bResult =
    case aResult of
        Err error ->
            Err error

        Ok a ->
            case bResult of
                Err error ->
                    Err error

                Ok b ->
                    abToResult a b


listMapToFastSetsAndUnify :
    (listElement -> FastSet.Set comparableFastSetElement)
    -> List listElement
    -> FastSet.Set comparableFastSetElement
listMapToFastSetsAndUnify elementToSet list =
    list
        |> List.foldl
            (\element soFar ->
                FastSet.union
                    (element |> elementToSet)
                    soFar
            )
            FastSet.empty


listMapAndToFastSet :
    (a -> comparable)
    -> List a
    -> FastSet.Set comparable
listMapAndToFastSet elementToSetElement list =
    list
        |> List.foldl
            (\element soFar ->
                soFar
                    |> FastSet.insert
                        (element |> elementToSetElement)
            )
            FastSet.empty


condenseExpressionCall :
    { called : OcamlExpression
    , argument0 : OcamlExpression
    , argument1Up : List OcamlExpression
    }
    -> OcamlExpression
condenseExpressionCall call =
    case call.called of
        OcamlExpressionCall calledCall ->
            condenseExpressionCall
                { called = calledCall.called
                , argument0 = calledCall.argument0
                , argument1Up =
                    calledCall.argument1Up
                        ++ (call.argument0 :: call.argument1Up)
                }

        OcamlExpressionLambda calledLambda ->
            case ( calledLambda.parameter0, calledLambda.result ) of
                ( OcamlPatternVariable "generated_record", OcamlExpressionRecordAccess recordAccess ) ->
                    case call.argument1Up of
                        [] ->
                            OcamlExpressionRecordAccess
                                { record = call.argument0
                                , field = recordAccess.field
                                }

                        argument1 :: argument2Up ->
                            OcamlExpressionCall
                                { called =
                                    OcamlExpressionRecordAccess
                                        { record = call.argument0
                                        , field = recordAccess.field
                                        }
                                , argument0 = argument1
                                , argument1Up = argument2Up
                                }

                _ ->
                    OcamlExpressionCall
                        { called = OcamlExpressionLambda calledLambda
                        , argument0 = call.argument0
                        , argument1Up = call.argument1Up
                        }

        calledNotCall ->
            OcamlExpressionCall
                { called = calledNotCall
                , argument0 = call.argument0
                , argument1Up = call.argument1Up
                }


ocamlExpressionIsDefinitelyOfTypeString : OcamlExpression -> Bool
ocamlExpressionIsDefinitelyOfTypeString ocamlExpression =
    case ocamlExpression of
        OcamlExpressionString _ ->
            True

        OcamlExpressionCall call ->
            call.called
                == OcamlExpressionReference { moduleOrigin = Just "String", name = "cat" }
                && ((call.argument1Up |> List.length) == 1)

        OcamlExpressionUnit ->
            False

        OcamlExpressionChar _ ->
            False

        OcamlExpressionFloat _ ->
            False

        OcamlExpressionReference _ ->
            False

        OcamlExpressionRecordAccess _ ->
            False

        OcamlExpressionTuple _ ->
            False

        OcamlExpressionIfThenElse _ ->
            False

        OcamlExpressionList _ ->
            False

        OcamlExpressionRecord _ ->
            False

        OcamlExpressionRecordUpdate _ ->
            False

        OcamlExpressionLambda _ ->
            False

        OcamlExpressionMatchWith _ ->
            False

        OcamlExpressionLetIn _ ->
            False


case_ :
    { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Expression.Case
    ->
        Result
            String
            { pattern : OcamlPattern, result : OcamlExpression }
case_ context ( patternNode, resultNode ) =
    Result.andThen
        (\casePattern ->
            Result.map
                (\result ->
                    { pattern = casePattern.pattern
                    , result = result
                    }
                )
                (resultNode
                    |> expression
                        (context
                            |> expressionContextAddVariablesInScope
                                casePattern.introducedVariables
                        )
                )
        )
        (patternNode
            |> pattern
                { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                    context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
                , variantLookup = context.variantLookup
                }
        )


letDeclaration :
    { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration
    -> Result String OcamlLetDeclaration
letDeclaration context (Elm.Syntax.Node.Node _ syntaxLetDeclaration) =
    case syntaxLetDeclaration of
        Elm.Syntax.Expression.LetDestructuring destructuringPatternNode destructuringExpressionNode ->
            Result.map2
                (\destructuringPattern destructuringExpression ->
                    OcamlLetDestructuring
                        { pattern = destructuringPattern.pattern
                        , expression = destructuringExpression
                        }
                )
                (destructuringPatternNode
                    |> pattern
                        { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                            context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
                        , variantLookup = context.variantLookup
                        }
                )
                (destructuringExpressionNode |> expression context)

        Elm.Syntax.Expression.LetFunction letValueOrFunction ->
            Result.map
                OcamlLetDeclarationValueOrFunction
                (letValueOrFunction
                    |> letValueOrFunctionDeclaration context
                )


letValueOrFunctionDeclaration :
    { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            Elm.Syntax.ModuleName.ModuleName
    , variantLookup :
        FastDict.Dict
            ( Elm.Syntax.ModuleName.ModuleName, String )
            { moduleOrigin : Elm.Syntax.ModuleName.ModuleName
            , valueCount : Int
            }
    , variablesFromWithinDeclarationInScope : FastSet.Set String
    }
    -> Elm.Syntax.Expression.Function
    ->
        Result
            String
            { name : String
            , parameters : List OcamlPattern
            , result : OcamlExpression
            , type_ : Maybe OcamlType
            }
letValueOrFunctionDeclaration context syntaxDeclarationValueOrFunction =
    let
        implementation : Elm.Syntax.Expression.FunctionImplementation
        implementation =
            syntaxDeclarationValueOrFunction.declaration |> Elm.Syntax.Node.value
    in
    resultAndThen2
        (\parameters maybeType ->
            Result.map
                (\result ->
                    { name =
                        implementation.name
                            |> Elm.Syntax.Node.value
                            |> variableNameDisambiguateFromOcamlKeywords
                    , type_ = maybeType
                    , parameters =
                        parameters
                            |> List.map .pattern
                    , result = result
                    }
                )
                (implementation.expression
                    |> expression
                        { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                            context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
                        , variantLookup =
                            context.variantLookup
                        , variablesFromWithinDeclarationInScope =
                            FastSet.union
                                (parameters
                                    |> listMapToFastSetsAndUnify .introducedVariables
                                )
                                context.variablesFromWithinDeclarationInScope
                        }
                )
        )
        (implementation.arguments
            |> listMapAndCombineOk
                (\p ->
                    p
                        |> pattern
                            { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                )
        )
        (case syntaxDeclarationValueOrFunction.signature of
            Nothing ->
                Ok Nothing

            Just (Elm.Syntax.Node.Node _ signature) ->
                Result.map Just
                    (signature.typeAnnotation
                        |> type_
                            { valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup =
                                context.valueAndFunctionAndTypeAliasAndChoiceTypeModuleOriginLookup
                            , variantLookup = context.variantLookup
                            }
                        |> Result.map ocamlTypeReplaceRecordsByGeneratedAliases
                    )
        )


expressionOperatorToOcamlFunctionReference :
    String
    -> Result String { moduleOrigin : Maybe String, name : String }
expressionOperatorToOcamlFunctionReference operatorSymbol =
    case operatorSymbol of
        "+" ->
            Ok { moduleOrigin = Just "Float", name = "add" }

        "-" ->
            Ok { moduleOrigin = Just "Float", name = "sub" }

        "*" ->
            Ok { moduleOrigin = Just "Float", name = "sub" }

        "/" ->
            Ok { moduleOrigin = Just "Float", name = "div" }

        "//" ->
            Ok { moduleOrigin = Nothing, name = "basics_idiv" }

        "^" ->
            Ok { moduleOrigin = Just "Float", name = "pow" }

        "==" ->
            Ok { moduleOrigin = Nothing, name = "basics_eq" }

        "/=" ->
            Ok { moduleOrigin = Nothing, name = "basics_neq" }

        "||" ->
            Ok { moduleOrigin = Nothing, name = "basics_or" }

        "&&" ->
            Ok { moduleOrigin = Nothing, name = "basics_and" }

        "<" ->
            Ok { moduleOrigin = Nothing, name = "basics_lt" }

        ">" ->
            Ok { moduleOrigin = Nothing, name = "basics_gt" }

        "<=" ->
            Ok { moduleOrigin = Nothing, name = "basics_le" }

        ">=" ->
            Ok { moduleOrigin = Nothing, name = "basics_ge" }

        "::" ->
            Ok { moduleOrigin = Just "List", name = "cons" }

        "++" ->
            Ok { moduleOrigin = Just "List", name = "append" }

        unknownOrUnsupportedOperator ->
            Err ("unknown/unsupported operator " ++ unknownOrUnsupportedOperator)


{-| Print a ocaml value/function declaration
-}
printOcamlValueOrFunctionDeclaration :
    { name : String
    , parameters : List OcamlPattern
    , result : OcamlExpression
    , type_ : Maybe OcamlType
    }
    -> Print
printOcamlValueOrFunctionDeclaration ocamlValueOrFunctionDeclaration =
    Print.exactly ocamlValueOrFunctionDeclaration.name
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                ((case ocamlValueOrFunctionDeclaration.type_ of
                    Nothing ->
                        case ocamlValueOrFunctionDeclaration.parameters of
                            [] ->
                                Print.empty

                            parameter0 :: parameter1Up ->
                                Print.exactly " "
                                    |> Print.followedBy
                                        ((parameter0 :: parameter1Up)
                                            |> Print.listMapAndIntersperseAndFlatten
                                                printOcamlPatternParenthesizedIfSpaceSeparated
                                                (Print.exactly " ")
                                        )

                    Just declaredType ->
                        let
                            typedParametersAndResultType :
                                { parameters : List { pattern : OcamlPattern, type_ : OcamlType }
                                , result : OcamlType
                                }
                            typedParametersAndResultType =
                                ocamlTypeSplitForParametersAndResult
                                    ocamlValueOrFunctionDeclaration.parameters
                                    declaredType

                            typePrint : Print
                            typePrint =
                                printOcamlTypeNotParenthesized
                                    typedParametersAndResultType.result

                            typedParameterPrints : List Print
                            typedParameterPrints =
                                typedParametersAndResultType.parameters
                                    |> List.map
                                        (\typedParameter ->
                                            let
                                                parameterTypePrint : Print
                                                parameterTypePrint =
                                                    printOcamlTypeNotParenthesized
                                                        typedParameter.type_
                                            in
                                            printParenthesized
                                                (printOcamlPatternNotParenthesized
                                                    typedParameter.pattern
                                                    |> Print.followedBy (Print.exactly " :")
                                                    |> Print.followedBy
                                                        (Print.spaceOrLinebreakIndented
                                                            (parameterTypePrint |> Print.lineSpread)
                                                        )
                                                    |> Print.followedBy
                                                        parameterTypePrint
                                                )
                                        )

                            fullLineSpread : Print.LineSpread
                            fullLineSpread =
                                (typePrint |> Print.lineSpread)
                                    |> Print.lineSpreadMergeWith
                                        (\() ->
                                            typedParameterPrints
                                                |> Print.lineSpreadListMapAndCombine
                                                    Print.lineSpread
                                        )
                        in
                        (case typedParameterPrints of
                            [] ->
                                Print.empty

                            typedParameterPrint0 :: typedParameterPrint1Up ->
                                Print.spaceOrLinebreakIndented fullLineSpread
                                    |> Print.followedBy
                                        ((typedParameterPrint0 :: typedParameterPrint1Up)
                                            |> Print.listIntersperseAndFlatten
                                                (Print.spaceOrLinebreakIndented fullLineSpread)
                                        )
                        )
                            |> Print.followedBy
                                (Print.spaceOrLinebreakIndented fullLineSpread
                                    |> Print.followedBy
                                        (Print.exactly ":")
                                    |> Print.followedBy
                                        (Print.spaceOrLinebreakIndented
                                            fullLineSpread
                                            |> Print.followedBy typePrint
                                        )
                                )
                 )
                    |> Print.followedBy
                        (Print.exactly " =")
                    |> Print.followedBy
                        (Print.linebreakIndented
                            |> Print.followedBy
                                (ocamlValueOrFunctionDeclaration.result
                                    |> printOcamlExpressionNotParenthesized
                                )
                        )
                )
            )


ocamlValueOrFunctionDeclarationsGroupByDependencies :
    List
        { name : String
        , parameters : List OcamlPattern
        , result : OcamlExpression
        , type_ : Maybe OcamlType
        }
    ->
        { mostToLeastDependedOn :
            List
                (OcamlValueOrFunctionDependencyBucket
                    { parameters : List OcamlPattern
                    , result : OcamlExpression
                    , type_ : Maybe OcamlType
                    , name : String
                    }
                    { name : String
                    , parameters : List OcamlPattern
                    , result : OcamlExpression
                    , type_ : Maybe OcamlType
                    }
                )
        }
ocamlValueOrFunctionDeclarationsGroupByDependencies ocamlValueOrFunctionDeclarations =
    let
        ordered :
            List
                (Data.Graph.SCC
                    { name : String
                    , parameters : List OcamlPattern
                    , result : OcamlExpression
                    , type_ : Maybe OcamlType
                    }
                )
        ordered =
            Data.Graph.stronglyConnComp
                (ocamlValueOrFunctionDeclarations
                    |> List.map
                        (\ocamlValueOrFunctionDeclaration ->
                            ( ocamlValueOrFunctionDeclaration
                            , ocamlValueOrFunctionDeclaration.name
                            , ocamlValueOrFunctionDeclaration.result
                                |> ocamlExpressionContainedLocalReferences
                                |> FastSet.toList
                            )
                        )
                )
    in
    { mostToLeastDependedOn =
        ordered
            |> List.map
                (\ocamlValueOrFunctionDependencyGroup ->
                    case ocamlValueOrFunctionDependencyGroup of
                        Data.Graph.CyclicSCC recursiveGroup ->
                            OcamlValueOrFunctionDependencyRecursiveBucket
                                recursiveGroup

                        Data.Graph.AcyclicSCC single ->
                            OcamlValueOrFunctionDependencySingle single
                )
    }


type OcamlValueOrFunctionDependencyBucket single recursiveGroupElement
    = OcamlValueOrFunctionDependencySingle single
    | OcamlValueOrFunctionDependencyRecursiveBucket (List recursiveGroupElement)


ocamlTypeSplitForParametersAndResult :
    List OcamlPattern
    -> OcamlType
    ->
        { parameters :
            List
                { pattern : OcamlPattern
                , type_ : OcamlType
                }
        , result : OcamlType
        }
ocamlTypeSplitForParametersAndResult parameters ocamlType =
    let
        splitWithReverseTypedParameters :
            { parametersReverse : List { pattern : OcamlPattern, type_ : OcamlType }
            , result : OcamlType
            }
        splitWithReverseTypedParameters =
            ocamlTypeSplitForParametersAndResultReversePrependToTypedParameters
                []
                parameters
                ocamlType
    in
    { parameters =
        splitWithReverseTypedParameters.parametersReverse
            |> List.reverse
    , result = splitWithReverseTypedParameters.result
    }


ocamlTypeSplitForParametersAndResultReversePrependToTypedParameters :
    List
        { pattern : OcamlPattern
        , type_ : OcamlType
        }
    -> List OcamlPattern
    -> OcamlType
    ->
        { parametersReverse :
            List
                { pattern : OcamlPattern
                , type_ : OcamlType
                }
        , result : OcamlType
        }
ocamlTypeSplitForParametersAndResultReversePrependToTypedParameters typedParametersSoFarReverse parameters ocamlType =
    case ocamlType of
        OcamlTypeFunction typeFunction ->
            case parameters of
                [] ->
                    { parametersReverse = typedParametersSoFarReverse
                    , result = OcamlTypeFunction typeFunction
                    }

                parameter0 :: parameter1Up ->
                    ocamlTypeSplitForParametersAndResultReversePrependToTypedParameters
                        ({ pattern = parameter0
                         , type_ = typeFunction.input
                         }
                            :: typedParametersSoFarReverse
                        )
                        parameter1Up
                        typeFunction.output

        OcamlTypeVariable name ->
            { parametersReverse = typedParametersSoFarReverse
            , result = OcamlTypeVariable name
            }

        OcamlTypeTuple parts ->
            { parametersReverse = typedParametersSoFarReverse
            , result = OcamlTypeTuple parts
            }

        OcamlTypeRecord fields ->
            { parametersReverse = typedParametersSoFarReverse
            , result = OcamlTypeRecord fields
            }

        OcamlTypeConstruct typeConstruct ->
            -- TODO in theory could be a type alias for a function
            { parametersReverse = typedParametersSoFarReverse
            , result = OcamlTypeConstruct typeConstruct
            }


qualifiedReferenceToOcamlName :
    { moduleOrigin : Maybe String
    , name : String
    }
    -> String
qualifiedReferenceToOcamlName reference =
    case reference.moduleOrigin of
        Nothing ->
            reference.name

        Just moduleOrigin ->
            moduleOrigin
                ++ "."
                ++ reference.name


printOcamlExpressionParenthesizedIfSpaceSeparated : OcamlExpression -> Print
printOcamlExpressionParenthesizedIfSpaceSeparated ocamlExpression =
    if ocamlExpression |> ocamlExpressionIsSpaceSeparated then
        printParenthesized (printOcamlExpressionNotParenthesized ocamlExpression)

    else
        printOcamlExpressionNotParenthesized ocamlExpression


ocamlExpressionIsSpaceSeparated : OcamlExpression -> Bool
ocamlExpressionIsSpaceSeparated ocamlExpression =
    case ocamlExpression of
        OcamlExpressionUnit ->
            False

        OcamlExpressionChar _ ->
            False

        OcamlExpressionFloat _ ->
            False

        OcamlExpressionString _ ->
            False

        OcamlExpressionReference _ ->
            False

        OcamlExpressionRecordAccess _ ->
            False

        OcamlExpressionTuple _ ->
            False

        OcamlExpressionIfThenElse _ ->
            True

        OcamlExpressionList _ ->
            False

        OcamlExpressionRecord _ ->
            False

        OcamlExpressionRecordUpdate _ ->
            False

        OcamlExpressionCall _ ->
            True

        OcamlExpressionLambda _ ->
            True

        OcamlExpressionMatchWith _ ->
            True

        OcamlExpressionLetIn _ ->
            True


{-| Print a [`OcamlExpression`](#OcamlExpression)
-}
printOcamlExpressionNotParenthesized : OcamlExpression -> Print
printOcamlExpressionNotParenthesized ocamlExpression =
    -- IGNORE TCO
    case ocamlExpression of
        OcamlExpressionUnit ->
            Print.exactly "()"

        OcamlExpressionCall call ->
            printOcamlExpressionCall call

        OcamlExpressionReference reference ->
            Print.exactly
                (reference |> qualifiedReferenceToOcamlName)

        OcamlExpressionIfThenElse ifThenElse ->
            printOcamlExpressionIfThenElse ifThenElse

        OcamlExpressionChar charValue ->
            Print.exactly (charLiteral charValue)

        OcamlExpressionFloat float ->
            Print.exactly (floatLiteral float)

        OcamlExpressionString string ->
            stringLiteral string

        OcamlExpressionTuple parts ->
            let
                part0Print : Print
                part0Print =
                    parts.part0 |> printOcamlExpressionNotParenthesized

                part1Print : Print
                part1Print =
                    parts.part1 |> printOcamlExpressionNotParenthesized

                part2UpPrints : List Print
                part2UpPrints =
                    parts.part2Up
                        |> List.map printOcamlExpressionNotParenthesized

                lineSpread : Print.LineSpread
                lineSpread =
                    part0Print
                        |> Print.lineSpread
                        |> Print.lineSpreadMergeWith
                            (\() -> part1Print |> Print.lineSpread)
                        |> Print.lineSpreadMergeWith
                            (\() ->
                                part2UpPrints
                                    |> Print.lineSpreadListMapAndCombine
                                        Print.lineSpread
                            )
            in
            Print.exactly "( "
                |> Print.followedBy
                    ((part0Print :: part1Print :: part2UpPrints)
                        |> Print.listIntersperseAndFlatten
                            (Print.emptyOrLinebreakIndented lineSpread
                                |> Print.followedBy
                                    (Print.exactly ", ")
                            )
                    )
                |> Print.followedBy
                    (Print.spaceOrLinebreakIndented lineSpread)
                |> Print.followedBy (Print.exactly ")")

        OcamlExpressionLetIn expressionWithLetDeclarations ->
            printOcamlExpressionLetIn expressionWithLetDeclarations

        OcamlExpressionMatchWith syntaxWhenIs ->
            printOcamlExpressionMatchWith syntaxWhenIs

        OcamlExpressionLambda syntaxLambda ->
            printOcamlExpressionLambda syntaxLambda

        OcamlExpressionRecord fields ->
            printOcamlExpressionRecord fields

        OcamlExpressionList elements ->
            printOcamlExpressionList elements

        OcamlExpressionRecordAccess syntaxRecordAccess ->
            printOcamlExpressionParenthesizedIfSpaceSeparated
                syntaxRecordAccess.record
                |> Print.followedBy
                    (Print.exactly
                        ("." ++ syntaxRecordAccess.field)
                    )

        OcamlExpressionRecordUpdate syntaxRecordUpdate ->
            printOcamlExpressionRecordUpdate syntaxRecordUpdate


printOcamlExpressionCall :
    { called : OcamlExpression
    , argument0 : OcamlExpression
    , argument1Up : List OcamlExpression
    }
    -> Print
printOcamlExpressionCall call =
    let
        calledPrint : Print
        calledPrint =
            printOcamlExpressionParenthesizedIfSpaceSeparated
                call.called

        argumentPrints : List Print
        argumentPrints =
            (call.argument0 :: call.argument1Up)
                |> List.map printOcamlExpressionParenthesizedIfSpaceSeparated

        fullLineSpread : Print.LineSpread
        fullLineSpread =
            argumentPrints
                |> Print.lineSpreadListMapAndCombine Print.lineSpread
                |> Print.lineSpreadMergeWith
                    (\() -> calledPrint |> Print.lineSpread)
    in
    calledPrint
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented fullLineSpread
                    |> Print.followedBy
                        (argumentPrints
                            |> Print.listIntersperseAndFlatten
                                (Print.spaceOrLinebreakIndented fullLineSpread)
                        )
                )
            )


floatLiteral : Float -> String
floatLiteral float =
    let
        asString : String
        asString =
            float |> String.fromFloat
    in
    if asString |> String.contains "." then
        asString

    else
        asString ++ ".0"


printOcamlExpressionList : List OcamlExpression -> Print
printOcamlExpressionList listElements =
    case listElements of
        [] ->
            Print.exactly "[]"

        element0 :: element1Up ->
            let
                elementsPrint : Print
                elementsPrint =
                    (element0 :: element1Up)
                        |> Print.listMapAndIntersperseAndFlatten
                            (\element ->
                                Print.withIndentIncreasedBy 2
                                    (case element of
                                        OcamlExpressionMatchWith elementMatchWith ->
                                            -- parens are necessary because
                                            -- the last inner match case result would is seen as a **statement**
                                            -- For some reason, this behavior seems to only occur with match and not with e.g. if
                                            printParenthesized
                                                (printOcamlExpressionMatchWith elementMatchWith)

                                        elementNotMatchWith ->
                                            printOcamlExpressionNotParenthesized elementNotMatchWith
                                    )
                            )
                            (Print.linebreakIndented
                                |> Print.followedBy (Print.exactly "; ")
                            )
            in
            Print.exactly "[ "
                |> Print.followedBy elementsPrint
                |> Print.followedBy
                    (Print.spaceOrLinebreakIndented
                        (elementsPrint |> Print.lineSpread)
                    )
                |> Print.followedBy
                    (Print.exactly "]")


printOcamlExpressionRecordUpdate :
    { originalRecordVariable : String
    , fields : FastDict.Dict String OcamlExpression
    }
    -> Print
printOcamlExpressionRecordUpdate syntaxRecordUpdate =
    printExactlyCurlyOpeningSpace
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                (Print.exactly syntaxRecordUpdate.originalRecordVariable)
            )
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy (Print.exactly " with ")
                    |> Print.followedBy
                        (syntaxRecordUpdate.fields
                            |> FastDict.toList
                            |> Print.listMapAndIntersperseAndFlatten
                                (\( fieldName, fieldValue ) ->
                                    Print.withIndentIncreasedBy 2
                                        (Print.exactly (fieldName ++ " ="))
                                        |> Print.followedBy
                                            (Print.withIndentAtNextMultipleOf4
                                                (Print.linebreakIndented
                                                    |> Print.followedBy
                                                        (case fieldValue of
                                                            OcamlExpressionMatchWith fieldValueMatchWith ->
                                                                -- parens are necessary because
                                                                -- the last inner match case result would is seen as a **statement**
                                                                -- For some reason, this behavior seems to only occur with match and not with e.g. if
                                                                printParenthesized
                                                                    (printOcamlExpressionMatchWith fieldValueMatchWith)

                                                            fieldValueNotMatchWith ->
                                                                printOcamlExpressionNotParenthesized fieldValueNotMatchWith
                                                        )
                                                )
                                            )
                                )
                                (Print.linebreakIndented
                                    |> Print.followedBy
                                        (Print.exactly "; ")
                                )
                        )
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy printExactlyCurlyClosing


patternIsSpaceSeparated : OcamlPattern -> Bool
patternIsSpaceSeparated ocamlPattern =
    case ocamlPattern of
        OcamlPatternIgnore ->
            False

        OcamlPatternFloat _ ->
            False

        OcamlPatternChar _ ->
            False

        OcamlPatternString _ ->
            False

        OcamlPatternVariable _ ->
            False

        OcamlPatternAs _ ->
            True

        OcamlPatternListCons _ ->
            True

        OcamlPatternListExact _ ->
            False

        OcamlPatternRecordInexhaustive _ ->
            False

        OcamlPatternVariant patternVariant ->
            case patternVariant.value of
                Nothing ->
                    False

                Just _ ->
                    True

        OcamlPatternTuple _ ->
            False


printOcamlPatternParenthesizedIfSpaceSeparated : OcamlPattern -> Print
printOcamlPatternParenthesizedIfSpaceSeparated ocamlPattern =
    if ocamlPattern |> patternIsSpaceSeparated then
        printParenthesized
            (ocamlPattern |> printOcamlPatternNotParenthesized)

    else
        ocamlPattern |> printOcamlPatternNotParenthesized


printOcamlExpressionLambda :
    { parameter0 : OcamlPattern
    , parameter1Up : List OcamlPattern
    , result : OcamlExpression
    }
    -> Print
printOcamlExpressionLambda syntaxLambda =
    printExactlyFunSpace
        |> Print.followedBy
            ((syntaxLambda.parameter0 :: syntaxLambda.parameter1Up)
                |> Print.listMapAndIntersperseAndFlatten
                    printOcamlPatternParenthesizedIfSpaceSeparated
                    (Print.exactly " ")
            )
        |> Print.followedBy (Print.exactly " ->")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printOcamlExpressionNotParenthesized syntaxLambda.result)
                )
            )


printOcamlExpressionIfThenElse :
    { condition : OcamlExpression
    , onTrue : OcamlExpression
    , onFalse : OcamlExpression
    }
    -> Print
printOcamlExpressionIfThenElse syntaxIfThenElse =
    let
        conditionPrint : Print
        conditionPrint =
            printOcamlExpressionNotParenthesized syntaxIfThenElse.condition

        conditionLineSpread : Print.LineSpread
        conditionLineSpread =
            conditionPrint |> Print.lineSpread
    in
    printExactlyIf
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented conditionLineSpread
                    |> Print.followedBy conditionPrint
                )
            )
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented conditionLineSpread)
        |> Print.followedBy (Print.exactly "then")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printOcamlExpressionNotParenthesized syntaxIfThenElse.onTrue)
                    |> Print.followedBy Print.linebreak
                )
            )
        |> Print.followedBy Print.linebreakIndented
        |> Print.followedBy printExactlyElse
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printOcamlExpressionNotParenthesized syntaxIfThenElse.onFalse)
                )
            )


printOcamlExpressionMatchWith :
    { matched : OcamlExpression
    , case0 : { pattern : OcamlPattern, result : OcamlExpression }
    , case1Up : List { pattern : OcamlPattern, result : OcamlExpression }
    }
    -> Print
printOcamlExpressionMatchWith matchWith =
    let
        matchedPrint : Print
        matchedPrint =
            printOcamlExpressionNotParenthesized matchWith.matched

        matchedPrintLineSpread : Print.LineSpread
        matchedPrintLineSpread =
            matchedPrint |> Print.lineSpread
    in
    printExactlyMatch
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.spaceOrLinebreakIndented matchedPrintLineSpread
                    |> Print.followedBy matchedPrint
                )
            )
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented matchedPrintLineSpread)
        |> Print.followedBy (Print.exactly "with")
        |> Print.followedBy
            (Print.linebreakIndented
                |> Print.followedBy
                    ((matchWith.case0 :: matchWith.case1Up)
                        |> Print.listMapAndIntersperseAndFlatten
                            (\syntaxCase -> syntaxCase |> printOcamlExpressionMatchWithCase)
                            printLinebreakLinebreakIndented
                    )
            )


printOcamlExpressionLetIn :
    { declaration0 : OcamlLetDeclaration
    , declaration1Up : List OcamlLetDeclaration
    , result : OcamlExpression
    }
    -> Print
printOcamlExpressionLetIn syntaxLetIn =
    let
        letDestructurings :
            List
                { pattern : OcamlPattern
                , expression : OcamlExpression
                }
        letDestructurings =
            (syntaxLetIn.declaration0 :: syntaxLetIn.declaration1Up)
                |> List.filterMap
                    (\declaration ->
                        case declaration of
                            OcamlLetDestructuring letDestructuring ->
                                Just letDestructuring

                            OcamlLetDeclarationValueOrFunction _ ->
                                Nothing
                    )

        letValueOrFunctions :
            List
                { name : String
                , parameters : List OcamlPattern
                , result : OcamlExpression
                , type_ : Maybe OcamlType
                }
        letValueOrFunctions =
            (syntaxLetIn.declaration0 :: syntaxLetIn.declaration1Up)
                |> List.filterMap
                    (\declaration ->
                        case declaration of
                            OcamlLetDeclarationValueOrFunction letValueOrFunction ->
                                Just letValueOrFunction

                            OcamlLetDestructuring _ ->
                                Nothing
                    )

        ordered :
            { mostToLeastDependedOn :
                List
                    (OcamlValueOrFunctionDependencyBucket
                        { parameters : List OcamlPattern
                        , result : OcamlExpression
                        , type_ : Maybe OcamlType
                        , name : String
                        }
                        { name : String
                        , parameters : List OcamlPattern
                        , result : OcamlExpression
                        , type_ : Maybe OcamlType
                        }
                    )
            }
        ordered =
            letValueOrFunctions
                |> ocamlValueOrFunctionDeclarationsGroupByDependencies
    in
    (letDestructurings
        |> Print.listMapAndIntersperseAndFlatten
            (\letDestructuring ->
                Print.exactly "let "
                    |> Print.followedBy
                        (letDestructuring |> printOcamlLetDestructuring)
                    |> Print.followedBy Print.linebreakIndented
                    |> Print.followedBy (Print.exactly "in")
                    |> Print.followedBy Print.linebreakIndented
            )
            Print.empty
    )
        |> Print.followedBy
            (ordered.mostToLeastDependedOn
                |> Print.listMapAndIntersperseAndFlatten
                    (\dependencyGroup ->
                        case dependencyGroup of
                            OcamlValueOrFunctionDependencySingle letValueOrFunction ->
                                Print.exactly "let "
                                    |> Print.followedBy
                                        (letValueOrFunction |> printOcamlValueOrFunctionDeclaration)
                                    |> Print.followedBy Print.linebreakIndented
                                    |> Print.followedBy (Print.exactly "in")
                                    |> Print.followedBy Print.linebreakIndented

                            OcamlValueOrFunctionDependencyRecursiveBucket recursiveGroup ->
                                Print.exactly "let rec "
                                    |> Print.followedBy
                                        (recursiveGroup
                                            |> Print.listMapAndIntersperseAndFlatten
                                                (\letValueOrFunction ->
                                                    (letValueOrFunction |> printOcamlValueOrFunctionDeclaration)
                                                        |> Print.followedBy Print.linebreakIndented
                                                )
                                                (Print.exactly "and ")
                                        )
                    )
                    Print.empty
            )
        |> Print.followedBy
            (printOcamlExpressionNotParenthesized syntaxLetIn.result)


printOcamlLetDeclaration : OcamlLetDeclaration -> Print
printOcamlLetDeclaration ocamlLetDeclaration =
    case ocamlLetDeclaration of
        OcamlLetDeclarationValueOrFunction letDeclarationExpression ->
            printOcamlValueOrFunctionDeclaration letDeclarationExpression

        OcamlLetDestructuring letDestructuring ->
            printOcamlLetDestructuring letDestructuring


printOcamlLetDestructuring :
    { pattern : OcamlPattern, expression : OcamlExpression }
    -> Print
printOcamlLetDestructuring letDestructuring =
    printOcamlPatternParenthesizedIfSpaceSeparated letDestructuring.pattern
        |> Print.followedBy (Print.exactly " =")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (printOcamlExpressionNotParenthesized letDestructuring.expression)
                )
            )


printOcamlExpressionMatchWithCase :
    { pattern : OcamlPattern, result : OcamlExpression }
    -> Print
printOcamlExpressionMatchWithCase branch =
    let
        patternPrint : Print
        patternPrint =
            printOcamlPatternNotParenthesized branch.pattern
    in
    Print.exactly "| "
        |> Print.followedBy
            (Print.withIndentIncreasedBy 2
                patternPrint
            )
        |> Print.followedBy
            (Print.spaceOrLinebreakIndented
                (patternPrint |> Print.lineSpread)
            )
        |> Print.followedBy (Print.exactly "->")
        |> Print.followedBy
            (Print.withIndentAtNextMultipleOf4
                (Print.linebreakIndented
                    |> Print.followedBy
                        (case branch.result of
                            OcamlExpressionMatchWith resultMatchWith ->
                                -- parens are necessary because
                                -- the inner match cases would count our current match branches as their own
                                -- since ocaml doesn't rely on significant indentation here
                                printParenthesized
                                    (printOcamlExpressionMatchWith resultMatchWith)

                            resultNotMatchWith ->
                                printOcamlExpressionNotParenthesized resultNotMatchWith
                        )
                )
            )


{-| Print value/function declarations into
an F# module called `Elm` in the global namespace that exposes all members.
Will also add some internal wrapper declarations.
-}
ocamlDeclarationsToModuleString :
    { valuesAndFunctions :
        FastDict.Dict
            String
            { parameters : List OcamlPattern
            , result : OcamlExpression
            , type_ : Maybe OcamlType
            }
    , typeAliases :
        FastDict.Dict
            String
            { parameters : List String
            , type_ : OcamlType
            }
    , choiceTypes :
        FastDict.Dict
            String
            { parameters : List String
            , variants : FastDict.Dict String (Maybe OcamlType)
            }
    }
    -> String
ocamlDeclarationsToModuleString ocamlDeclarations =
    let
        valueAndFunctionDeclarationsOrdered :
            { mostToLeastDependedOn :
                List
                    (OcamlValueOrFunctionDependencyBucket
                        { parameters : List OcamlPattern
                        , result : OcamlExpression
                        , type_ : Maybe OcamlType
                        , name : String
                        }
                        { name : String
                        , parameters : List OcamlPattern
                        , result : OcamlExpression
                        , type_ : Maybe OcamlType
                        }
                    )
            }
        valueAndFunctionDeclarationsOrdered =
            ocamlDeclarations.valuesAndFunctions
                |> fastDictMapAndToList
                    (\name valueOrFunctionInfo ->
                        { name = name
                        , type_ = valueOrFunctionInfo.type_
                        , parameters = valueOrFunctionInfo.parameters
                        , result = valueOrFunctionInfo.result
                        }
                    )
                |> ocamlValueOrFunctionDeclarationsGroupByDependencies
    in
    defaultDeclarations
        ++ """

type """
        ++ ((ocamlDeclarations.typeAliases
                |> fastDictMapAndToList
                    (\name typeAliasInfo ->
                        printOcamlTypeAliasDeclaration
                            { name = name
                            , parameters = typeAliasInfo.parameters
                            , type_ = typeAliasInfo.type_
                            }
                    )
            )
                ++ (ocamlDeclarations.choiceTypes
                        |> fastDictMapAndToList
                            (\name choiceTypeInfo ->
                                printOcamlChoiceTypeDeclaration
                                    { name = name
                                    , parameters = choiceTypeInfo.parameters
                                    , variants = choiceTypeInfo.variants
                                    }
                            )
                   )
                |> Print.listIntersperseAndFlatten
                    (Print.linebreak
                        |> Print.followedBy Print.linebreak
                        |> Print.followedBy
                            (Print.exactly "and ")
                    )
                |> Print.toString
           )
        ++ """
"""
        ++ ((valueAndFunctionDeclarationsOrdered.mostToLeastDependedOn
                |> Print.listMapAndIntersperseAndFlatten
                    (\dependencyGroup ->
                        case dependencyGroup of
                            OcamlValueOrFunctionDependencySingle letValueOrFunction ->
                                Print.exactly "let "
                                    |> Print.followedBy
                                        (letValueOrFunction |> printOcamlValueOrFunctionDeclaration)
                                    |> Print.followedBy Print.linebreak
                                    |> Print.followedBy Print.linebreakIndented

                            OcamlValueOrFunctionDependencyRecursiveBucket recursiveGroup ->
                                Print.exactly "let rec "
                                    |> Print.followedBy
                                        (recursiveGroup
                                            |> Print.listMapAndIntersperseAndFlatten
                                                (\letValueOrFunction ->
                                                    (letValueOrFunction |> printOcamlValueOrFunctionDeclaration)
                                                        |> Print.followedBy Print.linebreak
                                                        |> Print.followedBy Print.linebreakIndented
                                                )
                                                (Print.exactly "and ")
                                        )
                    )
                    Print.empty
            )
                |> Print.toString
           )
        ++ """
"""


defaultDeclarations : String
defaultDeclarations =
    """let basics_eq : 'a -> 'a -> bool =
    fun a b -> a = b
let basics_neq : 'a -> 'a -> bool =
    fun a b -> a <> b

let basics_lt : 'a -> 'a -> bool =
    fun a b -> a < b
let basics_gt : 'a -> 'a -> bool =
    fun a b -> a > b
let basics_le : 'a -> 'a -> bool =
    fun a b -> a <= b
let basics_ge : 'a -> 'a -> bool =
    fun a b -> a >= b
type basics_Order = | Basics_LT | Basics_EQ | Basics_GT
let intToBasics_order: int -> basics_Order =
    fun comparisonInt ->
        if comparisonInt < 0 then
            Basics_LT
        else if comparisonInt > 0 then
            Basics_GT 
        else
            Basics_EQ
let basics_orderToInt: basics_Order -> int =
    fun order ->
        match order with
        | Basics_LT -> -1
        | Basics_EQ -> 0
        | Basics_GT -> 1
let basics_compare : 'a -> 'a -> basics_Order =
    fun a b ->
        intToBasics_order (Stdlib.compare a b)

let basics_modBy =
    fun divisor toDivide ->
        let remainder = Float.div toDivide divisor
        in
        if
            (remainder > 0. && divisor < 0.)
                || (remainder < 0. && divisor > 0.)
        then
            remainder +. toDivide
        else
            remainder
let basics_idiv : float -> float -> float =
    fun a b -> Float.trunc (Float.div a b)

let basics_or : bool -> bool -> bool =
    fun a b -> Bool.(||) a b
let basics_and : bool -> bool -> bool =
    fun a b -> Bool.(&&) a b

let char_toCode : char -> float =
    fun char -> Int.to_float (Char.code char)
let char_fromCode : float -> char =
    fun char -> Char.chr (Float.to_int char)

let string_toInt : string -> float option =
    fun string -> Option.map Float.of_int (Stdlib.int_of_string_opt string)
let string_length : string -> float =
    fun string -> Float.of_int (String.length string)

let list_singleton : 'a -> 'a list =
    fun onlyElement -> [ onlyElement ]
let rec list_drop : float -> 'a list -> 'a list =
    fun skippedCount list ->
        if skippedCount <= 0.0 then
            list
        else
            match list with
            | [] ->
                []
            | _ :: tail ->
                list_drop (skippedCount -. 1.0) tail
let rec list_take_and_reverse_prepend_to : 'a list -> int -> 'a list -> 'a list =
    fun soFar toTakeCount list ->
        if toTakeCount <= 0 then
            soFar
        else
            match list with
            | [] ->
                soFar
            | head :: tail ->
                list_take_and_reverse_prepend_to
                    (head :: soFar)
                    (toTakeCount - 1)
                    tail
let list_take : float -> 'a list -> 'a list =
    fun toTakeCount list ->
        List.rev
            (list_take_and_reverse_prepend_to
                []
                (Float.to_int toTakeCount)
                list
            )
let list_sort : 'a list -> 'a list  =
    fun list -> List.sort compare list
let list_sortWith : ('a -> 'a -> basics_Order) -> 'a list -> 'a list  =
    fun elementCompare list ->
        List.sort (fun a b -> basics_orderToInt (elementCompare a b)) list
let list_range : float -> float -> float list =
    fun lowest highest ->
        List.init
            (Float.to_int (Float.sub highest lowest) + 1)
            Int.to_float
let list_intersperse : 'a -> 'a list -> 'a list =
    fun sep list ->
        match list with
        | [] -> []
        | listHead :: listTail ->
            List.fold_right
                (fun x soFar -> x :: sep :: soFar)
                listTail
                [ listHead ]
let list_foldl
    : ('a -> 'state -> 'state)
    -> 'state
    -> 'a list
    -> 'state =
    fun reduce initialState list ->
        List.fold_left
            (fun soFar element -> reduce element soFar)
            initialState
            list
let list_foldr
    : ('a -> 'state -> 'state)
    -> 'state
    -> 'a list
    -> 'state =
    fun reduce initialState list ->
        List.fold_right reduce list initialState
let list_sum : float list -> float =
    fun numbers -> List.fold_left Float.add 0.0 numbers
let list_product : float list -> float =
    fun numbers -> List.fold_left Float.mul 1.0 numbers
let list_minimum : 'a list -> 'a option =
    fun list ->
        match list with
        | x :: xs ->
            Some (List.fold_left Stdlib.min x xs)
        | _ ->
            None
let list_maximum : 'a list -> 'a option =
    fun list ->
        match list with
        | x :: xs ->
            Some (List.fold_left Stdlib.max x xs)
        | _ ->
            None
let list_repeat: float -> 'a -> 'a list =
    fun count element ->
        List.init (Float.to_int count) (fun _ -> element)

let string_toList : string -> char list =
    fun string -> List.of_seq (String.to_seq string)
let string_repeat : float -> string -> string =
    fun count segment ->
        String.concat ""
            (list_repeat count segment)
let string_slice : float -> float -> string -> string =
    fun startIndexInclusive endIndexExclusive string ->
        let realStartIndex =
            if startIndexInclusive < 0.0 then
                String.length string + Float.to_int startIndexInclusive
            else
                Float.to_int startIndexInclusive
        and realEndIndexExclusive =
            if endIndexExclusive < 0.0 then
                String.length string + Float.to_int endIndexExclusive
            else
                Float.to_int endIndexExclusive
        in
        String.sub
            string
            realStartIndex
            (realEndIndexExclusive - 1 - realStartIndex)
let string_fromChar : char -> string =
    fun char -> String.make 1 char
let string_cons : char -> string -> string =
    fun newHead tail ->
        String.cat (string_fromChar newHead) tail
let string_toInt : string -> float option =
    fun string ->
        Option.map Int64.to_float
            (Int64.of_string_opt string)
let string_concat : string list -> string =
    fun strings -> String.concat "" strings
let string_filter : (char -> bool) -> string -> string =
    fun shouldBeKept string ->
        String.of_seq
            (Seq.filter shouldBeKept (String.to_seq string))
let string_lines : string -> string list =
    fun string ->
        List.map
            (fun lineWithPotentialCarriageReturns ->
                string_filter (fun char -> char <> '\\r')
                    lineWithPotentialCarriageReturns
            )
            (String.split_on_char '\\n' string)
let string_foldl
    : (char -> 'state -> 'state)
    -> 'state
    -> string
    -> 'state =
    fun reduce initialState list ->
        String.fold_left
            (fun soFar element -> reduce element soFar)
            initialState
            list
let string_foldr
    : (char -> 'state -> 'state)
    -> 'state
    -> string
    -> 'state =
    fun reduce initialState list ->
        String.fold_right reduce list initialState
let string_reverse : string -> string =
    fun string ->
        String.of_seq
            (List.to_seq
                (List.rev (List.of_seq (String.to_seq string)))
            )
let string_right  : float -> string -> string =
    fun takenElementCount string ->
        String.sub string
            (String.length string - Float.to_int takenElementCount - 1)
            (Float.to_int takenElementCount)
let string_left : float -> string -> string =
    fun takenElementCount string ->
        String.sub string 0 (Float.to_int takenElementCount)
let string_dropRight : float -> string -> string =
    fun skippedElementCount string ->
        String.sub string
            0
            (String.length string - Float.to_int skippedElementCount)
let string_dropLeft : float -> string -> string =
    fun skippedElementCount string ->
        String.sub string
            (Float.to_int skippedElementCount - 1)
            (String.length string - Float.to_int skippedElementCount)"""


fastDictMapAndToList :
    (key -> value -> element)
    -> FastDict.Dict key value
    -> List element
fastDictMapAndToList keyValueToElement fastDict =
    fastDict
        |> FastDict.foldr
            (\key value soFar ->
                keyValueToElement key value
                    :: soFar
            )
            []


listMapAndCombineOk : (a -> Result err ok) -> List a -> Result err (List ok)
listMapAndCombineOk elementToResult list =
    listMapAndCombineOkFrom [] elementToResult list


listMapAndCombineOkFrom : List ok -> (a -> Result err ok) -> List a -> Result err (List ok)
listMapAndCombineOkFrom soFar elementToResult list =
    case list of
        [] ->
            Ok (soFar |> List.reverse)

        head :: tail ->
            case head |> elementToResult of
                Err headErr ->
                    Err headErr

                Ok headOk ->
                    listMapAndCombineOkFrom (headOk :: soFar)
                        elementToResult
                        tail


printLinebreakLinebreakIndented : Print.Print
printLinebreakLinebreakIndented =
    Print.linebreak
        |> Print.followedBy Print.linebreakIndented


printExactlyCurlyOpeningSpace : Print.Print
printExactlyCurlyOpeningSpace =
    Print.exactly "{ "


printExactlyParensOpening : Print
printExactlyParensOpening =
    Print.exactly "("


printExactlyParensClosing : Print
printExactlyParensClosing =
    Print.exactly ")"


printExactlyCurlyClosing : Print
printExactlyCurlyClosing =
    Print.exactly "}"


printExactlyFunSpace : Print
printExactlyFunSpace =
    Print.exactly "fun "


printExactlyUnderscore : Print
printExactlyUnderscore =
    Print.exactly "_"


printExactlyMatch : Print
printExactlyMatch =
    Print.exactly "match"


printExactlyIf : Print
printExactlyIf =
    Print.exactly "if"


printExactlyElse : Print
printExactlyElse =
    Print.exactly "else"
