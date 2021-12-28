{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

firstWord :: String -> String 
firstWord = head . words

logMessageString :: Int -> [String] -> String
logMessageString x y = unwords $ drop x y

intFromStringAtIndex :: [String] -> Int -> Int
intFromStringAtIndex x y = read $ x !! y


createInfo :: [String] -> LogMessage 
createInfo x = LogMessage Info (intFromStringAtIndex x 1) (logMessageString 2 x)

createWarning :: [String] -> LogMessage 
createWarning x = LogMessage Warning (intFromStringAtIndex x 1) (logMessageString 2 x)

createError :: [String] -> LogMessage 
createError x = LogMessage (Error (intFromStringAtIndex x 1)) (intFromStringAtIndex x 2) (logMessageString 3 x)


parseMessage :: String -> LogMessage 
parseMessage msg 
    | firstWord msg == "I" = createInfo $ words msg
    | firstWord msg == "E" = createError $ words msg
    | firstWord msg == "W" = createWarning $ words msg
    | otherwise = Unknown msg

parse :: String -> [LogMessage]
parse msgs = map parseMessage $ lines msgs

insert :: LogMessage -> MessageTree -> MessageTree 
insert (Unknown _) tree = tree
insert logMsg Leaf = Node Leaf logMsg Leaf 
insert logMsg@(LogMessage _ x _) (Node left treeMsg@(LogMessage _ y _) right)
    | x < y = Node (insert logMsg left) treeMsg right
    | otherwise = Node left treeMsg (insert logMsg right)

build :: [LogMessage] -> MessageTree 
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder tree = case tree of
    Leaf -> []
    Node left logMsg right -> inOrder left ++ [logMsg] ++ inOrder right

checkErrorAndTimestamp :: LogMessage -> Bool 
checkErrorAndTimestamp x = case x of
    LogMessage (Error severity) _ _ -> severity >= 50
    _ -> False

logMessageToString :: LogMessage -> String 
logMessageToString (LogMessage _ _ str) = str

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = map logMessageToString (filter checkErrorAndTimestamp $ inOrder (build x))