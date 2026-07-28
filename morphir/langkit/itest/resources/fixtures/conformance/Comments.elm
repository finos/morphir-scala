module Conformance.Comments exposing (only)

{-| A module doc comment, which trivia association attaches to the module
rather than to the declaration below it.
-}

-- a line comment

{- a block comment,
   spanning lines
-}


{-| A declaration doc comment.
-}
only : Int
only =
    -- a comment inside a declaration
    1
