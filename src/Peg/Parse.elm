module Peg.Parse exposing (..)

import Dict exposing (Dict)
import Peg.Rule exposing (Rule(..))


{-| -}
type alias Grammar =
    List ( String, Rule )


type alias Actions state =
    String -> String -> state -> Result String state


type alias Predicate state =
    String -> String -> state -> ( Bool, state )



--Dict String (state -> Bool)


parse : Grammar -> state -> Actions state -> Predicate state -> String -> Result Error state
parse grammar initialState actions predicates input =
    let
        initialParserState : ParserState state
        initialParserState =
            { state = initialState
            , grammar = Dict.fromList grammar
            , actions = actions
            , predicates = predicates
            , input = input
            , position = 0
            , accumulate = ""
            , collect = False
            }
    in
    case grammar of
        ( _, startRule ) :: _ ->
            parseRule initialParserState startRule
                |> Result.map .state

        [] ->
            Err
                { position = 0
                , message = "No rules in grammar"
                , code = Critical
                }


type ErrorCode
    = Critical
    | NotMatch


type alias Error =
    { position : Int
    , message : String
    , code : ErrorCode
    }


type alias ParserState state =
    { state : state
    , grammar : Dict String Rule
    , actions : Actions state
    , predicates : Predicate state
    , input : String
    , position : Int
    , accumulate : String
    , collect : Bool
    }


{-| parseRule parses a single rule in a PEG grammar.
-}
parseRule : ParserState state -> Rule -> Result Error (ParserState state)
parseRule parserState rule =
    case rule of
        Sequence sequence ->
            parseSequence parserState sequence

        Choice choice ->
            parseOrderedChoice parserState choice

        ZeroOrMore repeatRule ->
            parseZeroOrMore parserState repeatRule

        OneOrMore repeatRule ->
            parseOneOrMore parserState repeatRule

        Optional optionalRule ->
            parseOptional parserState optionalRule

        PositiveLookahead andRule ->
            parseAndPredicate parserState andRule

        ConditionalPredicate expr ->
            let
                ( success, newState ) =
                    parsePredicate parserState expr
            in
            if success then
                Ok newState

            else
                Err
                    { position = parserState.position
                    , message = "ConditionalPredicate not fully implemented"
                    , code = NotMatch
                    }

        NegativeLookahead notRule ->
            parseNotPredicate parserState notRule

        Collect collect ->
            parseCollect parserState collect

        Action action ->
            parseAction parserState action

        MatchLiteral string ->
            parseMatchLiteral parserState string

        MatchAny ->
            parseMatchAny parserState

        MatchNot character ->
            parseMatchNot parserState character

        RuleRef ref ->
            parseRuleRef parserState ref


{-| parseSequence parses a sequence of rules in a PEG grammar.
-}
parseSequence : ParserState state -> List Rule -> Result Error (ParserState state)
parseSequence parserState rules =
    case rules of
        [] ->
            Ok parserState

        rule :: remainingRules ->
            case parseRule parserState rule of
                Ok updatedState ->
                    parseSequence updatedState remainingRules

                Err error ->
                    Err error


{-| parseOrderedChoice parses a choice of rules in a PEG grammar.
-}
parseOrderedChoice : ParserState state -> List Rule -> Result Error (ParserState state)
parseOrderedChoice parserState rules =
    case rules of
        [] ->
            Err
                { position = parserState.position
                , message = "No rules matched"
                , code = NotMatch
                }

        rule :: remainingRules ->
            case parseRule parserState rule of
                Ok updatedState ->
                    Ok updatedState

                Err error ->
                    if error.code == Critical then
                        Err error

                    else
                        parseOrderedChoice parserState remainingRules


{-| parseZeroOrMore parses zero or more repetitions of a rule in a PEG grammar.
-}
parseZeroOrMore : ParserState state -> Rule -> Result Error (ParserState state)
parseZeroOrMore parserState rule =
    let
        parseMore updatedState =
            case parseRule updatedState rule of
                Ok nextState ->
                    parseMore nextState

                Err err ->
                    if err.code == Critical then
                        Err err

                    else
                        Ok updatedState
    in
    parseMore parserState


{-| parseOneOrMore parses one or more repetitions of a rule in a PEG grammar.
-}
parseOneOrMore : ParserState state -> Rule -> Result Error (ParserState state)
parseOneOrMore parserState rule =
    case parseRule parserState rule of
        Ok updatedState ->
            parseZeroOrMore updatedState rule

        Err error ->
            Err error


{-| parseOptional parses an optional rule in a PEG grammar.
-}
parseOptional : ParserState state -> Rule -> Result Error (ParserState state)
parseOptional parserState rule =
    case parseRule parserState rule of
        Ok updatedState ->
            Ok updatedState

        Err _ ->
            Ok parserState


{-| parseAndPredicate parses a predicate in a PEG grammar that must be matched for the overall match to succeed but not consumed from the input and remains available for subsequent matching.
-}
parseAndPredicate : ParserState state -> Rule -> Result Error (ParserState state)
parseAndPredicate parserState rule =
    case parseRule parserState rule of
        Ok updatedState ->
            Ok updatedState

        Err error ->
            Err error


{-| parseNotPredicate parses a predicate in a PEG grammar that must not be matched for the overall match to succeed.
-}
parseNotPredicate : ParserState state -> Rule -> Result Error (ParserState state)
parseNotPredicate parserState rule =
    case parseRule parserState rule of
        Ok _ ->
            Err
                { position = parserState.position
                , message = "Unexpected match when parseNotPredicate"
                , code = NotMatch
                }

        Err err ->
            if err.code == Critical then
                Err err

            else
                Ok parserState


{-| parsePredicate parses a conditional predicate in a PEG grammar,
where the associated expression is evaluated and used to determine the success or failure of the match.
-}
parsePredicate : ParserState state -> String -> ( Bool, ParserState state )
parsePredicate parserState name =
    parserState.predicates name parserState.accumulate parserState.state
        |> Tuple.mapSecond (\state -> { parserState | state = state })


{-| parseAction parses an action in a PEG grammar that should be performed when the rule is matched.
-}
parseAction : ParserState state -> String -> Result Error (ParserState state)
parseAction parserState action =
    case parserState.actions action parserState.accumulate parserState.state of
        Ok state ->
            Ok { parserState | state = state }

        Err err ->
            Err
                { position = .position parserState
                , message = "Fail Action(" ++ action ++ "): " ++ err
                , code = Critical
                }


parseCollect : ParserState state -> Rule -> Result Error (ParserState state)
parseCollect parserState rule =
    let
        preRuleAccumulateState =
            { parserState
                | collect = True
                , accumulate = ""
            }
    in
    parseRule preRuleAccumulateState rule
        |> Result.map
            (\stateAfterRule -> { stateAfterRule | collect = False })


parseMatchAny : ParserState state -> Result Error (ParserState state)
parseMatchAny parserState =
    let
        { input, position, collect, accumulate } =
            parserState

        char =
            String.slice position (position + 1) input
    in
    if String.length char == 1 then
        let
            updatedParserState =
                { parserState | position = position + 1 }
        in
        if collect then
            Ok { updatedParserState | accumulate = accumulate ++ char }

        else
            Ok updatedParserState

    else
        Err
            { position = position
            , message = "parseMatchAny: Unexpected end of input"
            , code = NotMatch
            }


parseMatchNot : ParserState state -> Char -> Result Error (ParserState state)
parseMatchNot parserState character =
    let
        { input, position, collect, accumulate } =
            parserState

        char =
            String.slice position (position + 1) input
                |> String.toList
                |> List.head
                |> Maybe.withDefault ' '
    in
    if char /= character then
        let
            updatedParserState =
                { parserState | position = position + 1 }
        in
        if collect then
            Ok { updatedParserState | accumulate = accumulate ++ String.fromChar char }

        else
            Ok updatedParserState

    else
        Err
            { position = position
            , message = "parseMatchNot: Unexpected end of input"
            , code = NotMatch
            }


{-| parseMatch parses a rule in a PEG grammar that matches a specific string.
-}
parseMatchLiteral : ParserState state -> String -> Result Error (ParserState state)
parseMatchLiteral parserState string =
    let
        { input, position, collect, accumulate } =
            parserState
    in
    if String.startsWith string (String.dropLeft position input) then
        let
            updatedParserState =
                { parserState | position = position + String.length string }
        in
        if collect then
            Ok { updatedParserState | accumulate = updatedParserState.accumulate ++ string }

        else
            Ok updatedParserState

    else
        Err
            { position = position
            , message = "parseMatchLiteral: Match failed"
            , code = NotMatch
            }


{-| parseRuleRef parses a reference to another rule in a PEG grammar.
-}
parseRuleRef : ParserState state -> String -> Result Error (ParserState state)
parseRuleRef parserState ref =
    case Dict.get ref (.grammar parserState) of
        Just rule ->
            parseRule parserState rule

        Nothing ->
            Err
                { position = .position parserState
                , message = "Rule \"" ++ ref ++ "\" not found"
                , code = Critical
                }
