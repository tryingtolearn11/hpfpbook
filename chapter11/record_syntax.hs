-- record_syntanx.hs --

module RecordSyntax where

{-
data Person = MkPerson String Int deriving (Eq, Show)
-}


data Person =
    Person { name :: String
           , age :: Int }
           deriving (Eq, Show)


