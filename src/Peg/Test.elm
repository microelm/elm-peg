module Peg.Test exposing (compareRules, compareRuleLists)

{-|

@docs compareRules, compareRuleLists

-}

import Peg.Rule exposing (..)


{-| Compare two PEG grammar rules for equality. Returns `Err message` if the rules are different, or `Ok ()` if they are the same.


## Arguments

  - `expected` - The expected rule to compare.
  - `actual` - The actual rule to compare.


## Returns

  - `Result String ()` - `Err message` if the rules are different, or `Ok ()` if they are the same.


## Example

    compareRules
        (Sequence [MatchLiteral "a", MatchLiteral "b"])
        (Sequence [MatchLiteral "a", MatchLiteral "b"])
    --> Ok ()

    compareRules
        (Sequence [MatchLiteral "a", MatchLiteral "b"])
        (Sequence [MatchLiteral "a", MatchLiteral "c"])
    --> Err "expected: MatchLiteral \"b\", actual: MatchLiteral \"c\" at position 2"

-}
compareRules : Rule -> Rule -> Result String ()
compareRules rule1a rule2a =
    case ( rule1a, rule2a ) of
        ( Sequence rules1, Sequence rules2 ) ->
            compareRuleLists rules1 rules2

        ( Choice rules1, Choice rules2 ) ->
            compareRuleLists rules1 rules2

        ( ZeroOrMore rule1, ZeroOrMore rule2 ) ->
            compareRules rule1 rule2

        ( OneOrMore rule1, OneOrMore rule2 ) ->
            compareRules rule1 rule2

        ( Optional rule1, Optional rule2 ) ->
            compareRules rule1 rule2

        ( ConditionalPredicate pred1, ConditionalPredicate pred2 ) ->
            if pred1 == pred2 then
                Ok ()

            else
                Err "ConditionalPredicate values are different"

        ( PositiveLookahead rule1, PositiveLookahead rule2 ) ->
            compareRules rule1 rule2

        ( NegativeLookahead rule1, NegativeLookahead rule2 ) ->
            compareRules rule1 rule2

        ( Collect rule1, Collect rule2 ) ->
            compareRules rule1 rule2

        ( Action action1, Action action2 ) ->
            if action1 == action2 then
                Ok ()

            else
                Err "Action values are different"

        ( MatchLiteral lit1, MatchLiteral lit2 ) ->
            if lit1 == lit2 then
                Ok ()

            else
                Err <| "MatchLiteral values are different (" ++ lit1 ++ ":" ++ lit2 ++ ")"

        ( MatchAny, MatchAny ) ->
            Ok ()

        ( MatchNot char1, MatchNot char2 ) ->
            if char1 == char2 then
                Ok ()

            else
                Err "MatchNot values are different"

        ( RuleRef name1, RuleRef name2 ) ->
            if name1 == name2 then
                Ok ()

            else
                "RuleRef values are different(" ++ name1 ++ " / " ++ name2 ++ ")" |> Err

        _ ->
            ("\n2:" ++ Debug.toString rule2a)
                |> (++) ("\n1:" ++ Debug.toString rule1a)
                |> (++) "Rule types are different:"
                |> Err


{-| Compare two lists of PEG grammar rules for equality. Returns `Err message` if the rules are different, or `Ok ()` if they are the same.


# Arguments

  - `expected` - The expected list of rules to compare.
  - `actual` - The actual list of rules to compare.


# Returns

  - `Result String ()` - `Err message` if the rules are different, or `Ok ()` if they are the same.


# Example

    compareRuleLists
        [ Sequence [MatchLiteral "a", MatchLiteral "b"]
        , Optional (MatchLiteral "c")
        , RuleRef "foo"
        ]
        [ Sequence [MatchLiteral "a", MatchLiteral "b"]
        , Optional (MatchLiteral "c")
        , RuleRef "foo"
        ]
    --> Ok ()

    compareRuleLists
        [ Sequence [MatchLiteral "a", MatchLiteral "b"]
        , Optional (MatchLiteral "c")
        , RuleRef "foo"
        ]
        [ Sequence [MatchLiteral "a", MatchLiteral "b"]
        , ZeroOrMore (MatchLiteral "c")
        , RuleRef "foo"
        ]
    --> Err "expected: Optional (MatchLiteral \"c\"), actual: ZeroOrMore (MatchLiteral \"c\") at position 1"

-}
compareRuleLists : List Rule -> List Rule -> Result String ()
compareRuleLists rules1 rules2 =
    if List.length rules1 /= List.length rules2 then
        ("\n2:" ++ Debug.toString rules2)
            |> (++) ("\n1:" ++ Debug.toString rules1)
            |> (++) "List lengths are different:"
            |> Err

    else
        case ( rules1, rules2 ) of
            ( x1 :: xs1, x2 :: xs2 ) ->
                case compareRules x1 x2 of
                    Ok _ ->
                        compareRuleLists xs1 xs2

                    Err err ->
                        Err err

            ( [], [] ) ->
                Ok ()

            _ ->
                Err "Something went Wrong"
