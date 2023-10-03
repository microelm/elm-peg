module Peg exposing
    ( parse, fromString
    , Grammar, Actions, Predicate, Error
    )

{-|


## Functions

@docs parse, fromString


## Types

@docs Grammar, Actions, Predicate, Error

-}

import Peg.Parse
import Peg.PegInPeg
import Peg.Rule exposing (Rule)


{-| Represents a PEG grammar as a list of rules, each consisting of a name and a
corresponding `Rule` value.

The `Rule` value should be a valid PEG grammar rule that can be parsed by the
`fromString` function.

See [`fromString`](#fromString) for an example of how to create a `Grammar` from a
string representation of a PEG grammar.

-}
type alias Grammar =
    List ( String, Rule )


{-| Represents an error that occurred during parsing.

The `position` field is the 0-based index in the input string where the error
occurred, and the `message` field provides a description of the error.

-}
type alias Error =
    { position : Int
    , message : String
    }


{-| The `Actions` type alias represents a function that is responsible for
performing an action on collected data. An `Action` is associated with a string,
which is the name of the action.

Actions take the following arguments:


## Arguments

  - `actionName` (String): the name of the action
  - `matchedText` (String): the matched text
  - `currentState` (state): the current state


## Returns

  - `Result` (Result String state): a `Result` that contains either an error
    message or the updated state.

  - If the `Result` contains an error message, parsing will be halted and the
    error message will be returned.

  - If the `Result` contains an updated state, parsing will continue with the new
    state.

See [`parse`](#parse) for an example of how to use parsing actions with the parser.

-}
type alias Actions state =
    String -> String -> state -> Result String state


{-| Represents a predicate that accepts the name of the predicate, the matched string,
and the current state, and returns a Boolean value and an updated state.


## Arguments

  - `predicateName` (String) : The name of the predicate.
  - `matchedText` (String) : The matched text.
  - `state` (state) : The current state.


## Returns

A tuple containing a Boolean value indicating whether the predicate succeeded or failed, and an updated state.


## Example

    myPredicate : Predicate String
    myPredicate predicateName matchedText state =
        ( String.contains "hello" matchedText, state )

-}
type alias Predicate state =
    String -> String -> state -> ( Bool, state )


{-| Parses the given input string using the specified PEG grammar.
Returns a `Result` that is either `Ok` with the parsed result or `Err` with an error message.


## Arguments

  - `grammar` (`Grammar`) : A valid PEG grammar represented as a list of `(String, Rule)` pairs.
  - `state` (`a`) : The initial state that will be passed to the actions.
  - `actions` (`Actions a`) : The actions that will be performed on the matched input.
  - `predicate` (`Predicate a`) : The predicates that will be evaluated on the matched input.
  - `input` (`String`) : The input string to parse.


## Returns

  - `Result Error a` : The result of the parse, which is either `Ok` with the parsed result or `Err` with an error message.


## Example

    import Html
    import Peg exposing (Error)

    grammarString : String
    grammarString =
        """
        start <- <digit+> {action}
        digit <- [0-9]
        """

    {-| Parse a string with the grammar
    -}
    result : Result Error String
    result =
        let
            actions _ found _ =
                Ok found

            predicate _ _ state =
                ( True, state )
        in
        grammarString
            |> Peg.fromString
            |> Result.andThen (\grammar -> Peg.parse grammar "" actions predicate "123")

    {-| Check if the parse succeeded
    -}
    main =
        case result of
            Ok value ->
                Html.text ("Parsed value: " ++ value)

            Err error ->
                Html.text ("Parse error: " ++ error.message)

-}
parse : Grammar -> state -> Actions state -> Predicate state -> String -> Result Error state
parse grammar initialState actions predicates input =
    Peg.Parse.parse grammar initialState actions predicates input
        |> stripError


{-| Parses a PEG grammar represented as a string and returns a `Result`
containing the parsed `Grammar`.


## Arguments

  - `input` (`String`) : The PEG grammar represented as a string.


## Returns

  - `Result Error Grammar` : A `Result` containing the parsed `Grammar`, or an error message.


## Example

    import Peg exposing (Error, fromString, parse)

    grammarString : String
    grammarString =
        """
        start <- digit+
        digit <- [0-9]
        """

    -- Parse the grammar string
    result : Result Error Grammar
    result =
        fromString grammarString

    -- Check if the parse succeeded
    case result of
        Ok grammar ->
            Debug.log "Parsed grammar" grammar

        Err error ->
            Debug.log "Parse error" error

-}
fromString : String -> Result Error Grammar
fromString =
    Peg.PegInPeg.fromString
        >> stripError


stripError : Result { a | position : b, message : c } d -> Result { position : b, message : c } d
stripError =
    Result.mapError (\err -> { position = err.position, message = err.message })
