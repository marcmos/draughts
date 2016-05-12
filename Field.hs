module Field where
import Figure

data Field = Field (Maybe Figure) deriving (Eq, Show)

showField :: Field -> Char
showField (Field Nothing) = '.'
showField (Field (Just figure)) = showFigure figure

readField :: Char -> Field
readField '.' = Field Nothing
readField c = Field (Just (readFigure c))
