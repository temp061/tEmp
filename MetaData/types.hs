module MetaData.Types where
 
data Operation = Add | Remove | Search | Look | Get | Append deriving (Show, Eq, Read, Ord) 
