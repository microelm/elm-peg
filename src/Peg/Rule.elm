module Peg.Rule exposing (Rule(..))

{-|

@docs Rule

-}


{-| Represents a PEG rule.


## Variants

  - **Sequence** Represents a sequence of rules that must be matched in order
  - **Choice** Represents a list of rules, where only one must be matched
  - **ZeroOrMore** Represents zero or more repetitions of a rule
  - **OneOrMore** Represents one or more repetitions of a rule
  - **Optional** Represents an optional rule that may or may not be matched
  - **ConditionalPredicate** Represents a conditional predicate, where the associated expression is evaluated to determine the success or failure of the match
  - **PositiveLookahead** Represents a predicate that must be matched for the overall match to succeed, but the input is not consumed and remains available for subsequent matching
  - **NegativeLookahead** Represents a predicate that must not be matched for the overall match to succeed
  - **Collect** Represents a rule that collects all mached data
  - **Action** Represents an action that should be performedi on collected data.
  - **MatchLiteral** Represents a rule that matches a specific string
  - **MatchAny** Represents a rule that matches any single character
  - **MatchNot** Represents a rule that matches any single character except the specified one
  - **RuleRef** Represents a reference to another rule in the PEG grammar

-}
type Rule
    = Sequence (List Rule)
    | Choice (List Rule)
    | ZeroOrMore Rule
    | OneOrMore Rule
    | Optional Rule
    | ConditionalPredicate String
    | PositiveLookahead Rule
    | NegativeLookahead Rule
    | Collect Rule
    | Action String
    | MatchLiteral String
    | MatchAny
    | MatchNot Char
    | RuleRef String
