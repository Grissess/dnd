module Space where

-- 5e PHB, p. 204
-- "length" is depth because I'm not fighting the prelude right now
data Area =
	  Line { depth :: Float, width :: Float }
	| Cylinder { height :: Float, radius :: Float }
	| Sphere { radius :: Float }
	| Cone { depth :: Float }
	| Cube { depth :: Float }
	deriving (Show)

-- Used for AreaDensity calculations
effectFloorArea :: Area -> Float
effectFloorArea Line { depth = d, width = w } = d * w
effectFloorArea Cylinder { radius = r } = pi * r * r
effectFloorArea Sphere { radius = r } = pi * r * r
effectFloorArea Cone { depth = d } = 0.5 * (sqrt 3) * d * d
effectFloorArea Cube { depth = d } = d * d
