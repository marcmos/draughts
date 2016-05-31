module Field where

import Data.Maybe

import Figure

data Field = Field {fieldFigure :: Maybe Figure} deriving (Eq, Show)

showField :: Field -> Char
showField field = maybe '.' showFigure (fieldFigure field)

readField :: Char -> Field
readField '.' = Field Nothing
readField c = Field (Just (readFigure c))

occupiedField :: Field -> Bool
occupiedField = isJust . fieldFigure
