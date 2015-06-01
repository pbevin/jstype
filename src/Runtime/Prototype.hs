module Runtime.Prototype (makePrototype) where

import Runtime.Types
import Runtime.Object
import Runtime.Error
import Runtime.Conversion

makePrototype :: String -> Runtime (Shared JSObj)
makePrototype name = do
  objectPrototype <- findPrototypeForClass "Object"
  newObject
    >>= setClass name
    >>= objSetPrototype objectPrototype

findPrototypeForClass :: String -> Runtime (Shared JSObj)
findPrototypeForClass name =
  getGlobalProperty name >>= valGetPrototype >>= maybe oops return
    where
      valGetPrototype (VObj objRef) = fromObj <$> objGet "prototype" objRef
      valGetPrototype _ = return Nothing
      oops = raiseError $ "No prototype for " ++ name
