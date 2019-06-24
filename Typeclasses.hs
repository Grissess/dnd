module Typeclasses where

class ExpectedValue a where
	expected :: a -> Float

newtype IIDSum a = IIDSum [a]

events :: IIDSum a -> [a]
events (IIDSum l) = l

instance (ExpectedValue a) => ExpectedValue (IIDSum a) where
	expected = sum . (map expected) . events

-- To the poor souls who have to hook this to Data.Default: I apologize in advance.
class Default a where
	def :: a

class Identifiable a where
	ident :: a -> Int
