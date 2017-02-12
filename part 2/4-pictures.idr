-- Use `:doc Shape` to view information, provided via |||

||| Represents shapes
data Shape = ||| A triangle, with its base length and height 
             Triangle Double Double
             | ||| A rectangle, with its length and height
             Rectangle Double Double -- width and height
             | ||| A circle, with its radius
             Circle Double -- radius

-- Implement interactively function `area` which calculates area of the given shape

area : Shape -> Double

-- Add documenting comments to this datatype representing pictures

data Picture = Primitive Shape
               | Combine Picture Picture
               | Rotate Double Picture
               | Translate Double Double Picture

-- Make sure you understand the meaning of word "translate" in this context

-- Examples of pictures
rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

test_picture : Picture
test_picture = Combine (Translate 5 5 rectangle)
                    (Combine (Translate 35 5 circle)
                    (Translate 15 25 triangle))

-- Implement interactively
picture_area : Picture -> Double


||| Returns the area of the biggest triangle in a picture
biggestTriangle : Picture -> Maybe Double
