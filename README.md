# A String Format Tool with Haskell

This is a library for string formating and processing in haskell. It includes the main functions as follows
- `format :: String -> [String] -> String`
- `formatBy :: String -> String -> [String] -> String`
- `concatWith :: [String] -> [String] -> String`
- `splitBy :: String -> String -> [String]`
- `printf :: String -> [String] -> IO ()`
- `printfLn :: String -> [String] -> IO ()`
- `join :: String -> [String] -> String`
- `findSplit :: String -> String -> (String,String)`
- `replace :: String -> String -> String -> String`
- `replaceAll :: String -> String -> String -> String`
- `leftAligned :: Int -> String -> String`
- `rightAligned :: Int -> String -> String`
- `centerAligned :: Int -> String -> String`
