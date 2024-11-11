-- https://stackoverflow.com/questions/61280735/how-to-implement-split-function-haskell
module Formator (
      format
    , formatBy
    , concatWith
    , splitBy
    , printf
    , printfLn
    , nextLine
    , headIs
    , catLines
    , join
    , findSplit
    , replace
    , replaceAll
    , leftAligned
    , rightAligned
    , centerAligned
    , (==?)
    , (%<)
    , (%<<)
) where

import System.IO

keySym = "$S"

catLines "" = ""
catLines (x:xs) = 
    case x of
      '-'  -> case xs of
                ('\n':ys) -> catLines ys
                (' ':ys) -> catLines ys
                ('\r':'\n':ys) -> catLines ys
                _ -> x:catLines xs 
      '\n' -> catLines xs
      _    -> x:catLines xs

join :: String -> [String] -> String
join s [] = ""
join s [x] = x
join s (x:xs) = x ++ s ++ join s xs

leftAligned n s
  | len >= n = take n s
  | otherwise = s ++ replicate (n - len) ' '
  where 
    len = length s

rightAligned n s 
  | len >= n = take n s
  | otherwise = replicate (n - len)  ' ' ++ s
  where 
    len = length s

centerAligned n s
  | len >= n = take n s
  | otherwise = replicate lenl ' ' ++ s ++ replicate lenr ' '
  where 
    len = length s
    flen = fromIntegral (n - len)
    flen2 = flen/2
    lenl = ceiling flen2 
    lenr = floor flen2

splitBy :: String -> String -> [String]
splitBy ""  sym = []
splitBy str sym = splitBy' str "" sym

splitBy' :: String -> String -> String -> [String]
splitBy' "" s  _  = [s]
splitBy' str@(x:xs) curr sym
  | str ==? sym = curr:splitBy' (drop (length sym) str) "" sym
  | otherwise   = splitBy' xs (curr++[x]) sym

headIs "" "" = True
headIs _  "" = True
headIs "" _  = False
headIs (x:xs) (y:ys) 
  | x == y    = headIs xs ys
  | otherwise = False

(==?) = headIs

concatWith :: [String] -> [String] -> String
concatWith []  _  = []
concatWith [s] _  = s
concatWith (x:xs) [] = x ++ keySym ++ concatWith xs []
concatWith (x:xs) (y:ys) = x ++ y ++ concatWith xs ys

formatBy :: String -> String -> [String] -> String
formatBy sym str lst =
  let strLst = splitBy str sym
   in concatWith strLst lst

format :: String -> [String] -> String
format = formatBy keySym

printfLn x y = putStrLn $ format x y
printf x y = putStr $ format x y

nextLine = putStrLn ""

(%<) = format
(%<<) x y = format x [y]

deRepeat :: String -> Char -> String
deRepeat = undefined

findSplit :: String -> String -> (String,String)
findSplit s trg = findSplit' s trg ""
  where
    findSplit' "" _ buf = (reverse buf,"")
    findSplit' src@(x:xs) trg' buf 
      | src ==?  trg' = (reverse buf,src)
      | otherwise = findSplit' xs trg' (x:buf)

replace :: String -> String -> String -> String
replace "" _ _ = ""
replace src@(x:xs) trg s 
  | src ==? trg = s ++ drop (length trg) src
  | otherwise = x:replace xs trg s


replaceAll :: String -> String -> String -> String
replaceAll "" _ _ = ""
replaceAll src@(x:xs) trg s
  | src ==? trg = s ++ replaceAll (drop (length trg) src) trg s
  | otherwise = x:replaceAll xs trg s

joinWithIO :: Int -> [String] -> IO [String]
joinWithIO n l = do
  putStr $ "str" ++ show n ++ ">> "
  hFlush stdout
  line <- getLine
  case line of
    "" -> return $ reverse l
    s  -> joinWithIO (n+1) $ s:l

tempRepl s = do
  putStrLn "Input parameter:"
  paras <- joinWithIO 1 []
  case paras of
    [] -> return ()
    _  -> do
      putStrLn $ format s paras
      tempRepl s

formatRepl = do
  putStrLn "Input template:"
  temp <- getLine
  case temp of
    "" -> return ()
    _  -> do
      tempRepl temp
      putStrLn ""
      formatRepl

main = do
    print $ "123" ==? ""
    print $ "123" ==? "123"
    print $ "" ==? "123"
    print $ "" ==? ""
    print $ "123" ==? "12"
    print $ splitBy' "123 $S 456 $S 987" "" "$S"
    print $ splitBy' "$S 456 $S 987" "" "$S"
    print $ drop (length "$S") "$S 456 $S 987"
    print $ splitBy "123 $S 456 $S 987" "$S"
    print $ format "123 $S 456 $S 987" ["$$","^^"]
    print $ splitBy "$S is a good $S for $S" "$S"
    print $ format "$S is a good $S for $S" ["He","girl","dressing"]
    print $ format "$S is a good $S for $S $S" ["He","girl","dressing"]
    print $ format "$S is a good $S for $S$S" ["He","girl","dressing"]
    print $ splitBy "$S is a good $S for $S$S" "$S"
    print $ format "$S is a good $S for $S$S" []
    print $ format "AAAAA" []
    print $ "$S is a good $S for $S"%<<"She"%<<"boy"%<<"fighting"
    printf "Ah, $S is a $S" ["He","God"]
    nextLine
    printfLn "Ah, $S is a $S" ["He","God"]
    putStrLn $ join "," ["123","456","789"]
    putStrLn $ catLines "1234-\n567\n89-10"
    print $ findSplit "123" "CC"
    print $ findSplit "12CC3" "CC"
    putStrLn $ replaceAll "AABBCCBBDD" "B" "8"
    putStrLn $ replace "AABBCCBBDD" "B" "8"
    putStrLn $ centerAligned 10 "ABC"
    putStrLn $ centerAligned 10 "ABCD"
    putStrLn $ centerAligned 10 "ABCDE"
    -- s <- joinWithIO []
    -- print s
    putStrLn "Auto Test Finished"
    formatRepl
