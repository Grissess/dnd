module Space where

data Area =
	  Line { length :: Int, width :: Int }
	| Cylinder { length :: Int, radius :: Int }
	| Sphere { radius :: Int }
	| Cone { length :: Int }
	| Cube { length :: Int }
