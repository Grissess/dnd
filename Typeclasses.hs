module Typeclasses where

class ExpectedValue a where
	expected :: a -> Float

