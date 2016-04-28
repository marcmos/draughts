data Figure = Black | White | BlackKing | WhiteKing
data Field = Field (Maybe Figure)
type BoardLine = [Field]
type Board = [BoardLine]

instance Show Figure where
  show (Black) = "b"
  show (White) = "w"
  show (BlackKing) = "B"
  show (WhiteKing) = "W"

instance (Show Field) where
  show (Field Nothing) = "."
  show (Field (Just figure)) = show figure

prefixDot x = "." ++ x
postfixDot x = x ++ "."

serializeBoardLine boardLine lineNumber =
  concat (map fillFunc lineString)
  where lineString = map show boardLine
        fillFunc = (if even lineNumber then prefixDot else postfixDot)

exampleBoard = [
  [Field (Just Black), Field Nothing, Field (Just WhiteKing), Field (Just BlackKing)],
  [Field Nothing, Field Nothing, Field (Just White), Field (Just Black)]]
