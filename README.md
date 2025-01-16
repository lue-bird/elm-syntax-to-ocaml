Print pure [`elm-syntax`](https://dark.elm.dmy.fr/packages/stil4m/elm-syntax/latest/) declarations as
[ocaml](https://ocaml.org/) code.

```elm
import Elm.Parser
import ElmSyntaxToOcaml

"""module Sample exposing (..)

plus2 : Int -> Int
plus2 n =
    n + ([ 2 ] |> List.sum)
"""
    |> Elm.Parser.parseToFile
    |> Result.mapError (\_ -> "failed to parse elm source code")
    |> Result.map
        (\syntaxModule ->
            [ syntaxModule ]
                |> ElmSyntaxToOcaml.modules
                |> .declarations
                |> ElmSyntaxToOcaml.ocamlDeclarationsToModuleString
        )
-->
Ok """..some default declarations..

let sample_plus2 (n : float) : float =
    Float.add n (list_sum [ 2.0 ])
"""
```

To try it out, you can
run [this node script](https://github.com/lue-bird/elm-syntax-to-ocaml/tree/main/node-elm-to-ocaml).

### be aware

-   only a subset of elm is currently supported. not supported:
    -   `elm/regex`, `elm/file`, `elm/bytes`, `elm/http`, `elm/random`, `elm/url`, `elm/json`, `elm/parser`, `elm/virtual-dom`,
        `elm/html`, `elm/svg`, `elm/browser`, `elm/time`, `elm-explorations/markdown`, `elm-explorations/webgl`, `elm-explorations/benchmark`, `elm-explorations/linear-algebra`
    -   `Platform`, `Platform.Cmd`, `Platform.Sub`, `Task`, `Process`
    -   ports, glsl, the prefix operator functions `(>>)` and `(<<)`, empty records
    -   `++` will default to `List.append` unless one of the arguments is a string literal. So e.g. use `a ++ b ++ ""` to append string variables (which is also faster in elm)
    -   Working with **generic** anonymous records won't work if the field name is ambiguous across record representations (at let or module level):
        ```elm
        myPet =
            { name = "blobollo", hungerPercentage = 0.5 }
        defaultUser =
            { name = "bob", email = "pizza@bob.blue" }
        -- will transpile just fine
        -- but now the field `name` is ambiguous

        getName =
            .name
        -- is inferred { r | name : name } -> name
        -- I don't know on which record it should work so it'll fail
        
        setName new r =
            { r | name = new }
        -- inferred name -> { r | name : name } -> { r | name : name }
        -- will fail
        
        getName : { r | name : name } -> name
        getName =
            .name
        -- even if used in explicit annotation
        -- it will fail
        
        type alias Named otherFields =
            { otherFields | name : String }
        
        getName : Named { email : String } -> String
        getName =
            .name
        -- even if the extensible record type is "fully constructed"
        -- it will fail


        -- Allowed however are for example:

        -- annotating with an explicit record type
        userGetName : { name : String, email : String } -> String
        userGetName =
            .name

        -- explicitly referring to a specific type alias
        type alias User =
            { name : String, email : Email }
        userGetName : User -> String
        userGetName =
            .name

        -- accessing a concretely inferrable value
        List.foldl
            (\element soFar ->
                { soFar 
                    | -- update allowed
                      index =
                        soFar.index -- access allowed
                            + 1
                      result =
                        ( soFar.index, element ) :: soFar.result
                }
            )
            { index = 0, result = [] }
            someList
            |> .result -- accessor allowed
        ```
        Incidentally, avoiding these kinds of extensible record types
        also tends to improve your elm code because it's simpler and makes the compiler errors more concrete
    -   potential future candidates: `Basics.(<<)`, `Basics.(>>)`, `Basics.clamp`, `Basics.degrees`, `Basics.turns`,
        `Basics.radians`, `Basics.logBase`, `Basics.toPolar`, `Basics.fromPolar`, `Basics.never`, `List.map5`, `List.map4`, `Char.toLocaleLower`, `Char.toLocaleUpper`, `Char.isAlpha`, `Char.isAlphaNum`, `Char.isDigit`, `Char.isOctDigit`, `Char.isHexDigit`, `String.split`, `String.contains`, `List.head`, `List.tail`, `List.map3`, `List.map4`, `List.map5`, `List.sortBy`, `Bitwise`, `Array`. Any help appreciated!
-   no checks are performed before transpiling to ocaml. So if you don't add a compile check of your elm input,
    you might e.g. get a running program that circumvents an elm opaque type or phantom type, or a ocaml program that can't be run
-   not much care has been put into making the resulting code readable or even conventionally formatted
    and comments are not preserved

Please [report any issues](https://github.com/lue-bird/elm-syntax-to-ocaml/issues/new) you notice <3

### why ocaml?
-   it runs decently fast and [can target Wasm through a third party](https://github.com/ocaml-wasm/wasm_of_ocaml)
-   it's pretty much a superset of elm which makes transpiling easy

### shoutout
-  I was struggling a lot with correctly ordering (mutually recursive) lets
   and [guida-lang/graph: Data.Graph.stronglyConnComp](https://dark.elm.dmy.fr/packages/guida-lang/graph/latest/Data-Graph#stronglyConnComp) singlehandedly solved it :)
