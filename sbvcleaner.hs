import System.Environment
import System.IO
import System.Directory

dropEvery2Lines :: [String] -> [String]
dropEvery2Lines (a:b:c:_:_:_:xs) = a : b : c : dropEvery2Lines xs
dropEvery2Lines xs = xs

removeBR' :: String -> String
removeBR' (a:b:c:d:xs) =
    if a == '[' && b == 'b' && c == 'r' && d == ']' then
        "" ++ removeBR' xs
    else
        a : (removeBR' $ b : c : d : xs)

removeBR' x = x

dropInvalidLines :: [String] -> [String]
dropInvalidLines (a:b:c:xs) = 
    if (';' `elem` b) || (b == "") || (b == "[br]") then 
        dropInvalidLines xs
    else
        removeBR' b : dropInvalidLines xs
dropInvalidLines xs = xs

unlines' :: [String] -> String
unlines' []         = ""
unlines' [x]        = x ++ "\n"
unlines' (x:xs)     = x ++ "\n\n" ++ unlines' xs

insertCC :: [String] -> [String]
insertCC s = s ++ ["//Subtitled by Wintergatan Writers. Join us on amara.org//"]



convertFile :: FilePath -> IO ()
convertFile filepath = do
    fileExists <- doesFileExist filepath
    if fileExists then do
        contents <- readFile filepath

        let newContent = unlines' . insertCC .  dropInvalidLines . lines $ contents

        writeFile (filepath ++ ".fixed.txt") newContent

        putStrLn "Done :)"
    else
        error "404 - File not found"



main :: IO ()
main = do
    args <- getArgs
    if length args == 0 then do
        -- Verbal Mode
        putStrLn "Please enter the Filename:"
        file <- getLine
        convertFile file

    else do
        -- Argument Mode
        sequence_ (map convertFile args)


    -- if doesFileExist file then


    -- let content = unlines' . insertCC .  dropInvalidLines . lines $


    putStrLn $ "Now have fun subtitling!\n"

    putStrLn "You can close this window now"
    idk <- getChar
    putStrLn "Bye"

