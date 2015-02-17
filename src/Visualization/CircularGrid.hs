
module Visualization.CircularGrid where

import Autolib.Exp.Read ()
import Autolib.Exp.Type
import Data.List (sort, sortBy, transpose, or)

--------------------------------------------------------------------------------
rho :: Integer -> Integer
rho k 
    | k == 1    = 0
    | k  > 1    = floor (logBase 2 (fromInteger k))
    | otherwise = error "invalid input"

--------------------------------------------------------------------------------
phi :: Integer -> Double
phi k  
    | k == 1    = 0.0
    | k  > 1    = pi * (a + b + c)
    | otherwise = error "invalid input"
    where
        a      = 0.5
        b      = (1 / (2 ** rhoOfK))
        c      = kk / (2 ** (rhoOfK - 1))
        kk     = fromIntegral (k `mod` (floor (2 ** rhoOfK)))
        rhoOfK = fromIntegral (rho k)

--------------------------------------------------------------------------------
gridPos :: Integer -> (Double, Double)
gridPos k = (x, y)
    where
        x = r * cos (phi k)
        y = r * sin (phi k)
        r = fromIntegral (rho k)

--------------------------------------------------------------------------------
gridPosS :: Integer -> (Double, Double)
gridPosS k = (x * level, y * level)
    where
        (x, y) = gridPos k
        level = fromIntegral (exponent (fromIntegral k))

--------------------------------------------------------------------------------
gridPosS' :: Integer -> Double -> (Double, Double)
gridPosS' k s = (x * s, y * s)
    where
        (x, y) = gridPosS k

--------------------------------------------------------------------------------
drawNodeLabels :: String -> [(Bool, (Double, Double), String)] -> IO ()
drawNodeLabels outFile ls = do
    appendFile outFile "0.0 0.0 0.0 c\n"
    appendFile outFile "/Courier-Bold findfont\n"
    appendFile outFile "11 scalefont\n"
    appendFile outFile "setfont\n"
    appendFile outFile "1 setlinewidth\n"
    mapM_ (appendFile outFile) $ [drawNodeLabel y z | (x,y,z) <- ls, x == True]

--------------------------------------------------------------------------------
drawNodeLabel :: (Double, Double) -> String -> String
drawNodeLabel (x, y) label =  
    "gsave\n" ++
    "1.0 0.0 0.0 c\n" ++ 
    coordString ++ "moveto\n" ++
    "(" ++ label ++ ") show\n" ++
    "grestore\n"
    where
        coordString = (show relocX) ++ " " ++ (show relocY) ++ " "
        relocX = x + 2
        relocY = y + 2

--------------------------------------------------------------------------------
drawCircle :: (Double, Double) -> String
drawCircle (x, y) = 
    coordString ++ show (circleSize) ++ " 0 360 arc\n" ++ 
    "stroke\n"
    where coordString = (show x) ++ " " ++ (show y) ++ " "

--------------------------------------------------------------------------------
drawCircles :: String -> [(Bool, (Double, Double), String)] -> IO ()
drawCircles outFile ls = do
    appendFile outFile "0.0 0.0 0.0 c\n"
    appendFile outFile "1.2 setlinewidth\n"
    mapM_ (appendFile outFile) $ [drawCircle y | (x,y,_) <- ls, x == True]

--------------------------------------------------------------------------------
drawLine :: (Double, Double) -> (Double, Double) -> String
drawLine (rx, ry) (x, y) =
    "newpath\n" ++
    (show rx) ++ " " ++ (show ry) ++ " moveto\n" ++
    (show x) ++ " " ++ (show y) ++ " lineto\n" ++
    "closepath\n" ++
    "stroke\n"

--------------------------------------------------------------------------------
drawLines :: String -> [(Bool, (Double, Double), String)] -> IO ()
drawLines outFile xs = do
    appendFile outFile "0.0 0.0 0.0 c\n"
    appendFile outFile "0.4 setlinewidth\n"
    drawLines' outFile xs

--------------------------------------------------------------------------------
drawLines' :: String -> [(Bool, (Double, Double), String)] -> IO ()
drawLines' outFile xs = do
    let snd3 = \(_,y,_) -> y
    let fst3 = \(x,_,_) -> x
    sequence_ $ do
        let len = length xs
        p <- [0..len-1]
        return $ do
            let (rx, ry) = snd3 (xs !! p)   
            sequence_ $ do
                let child = p*2
                c <- [child+1,child+2]
                return $ do
                    if(c < len)
                        then
                            if((fst3 $ xs !! c) == True)
                            then
                                let (a, b) = snd3 $ xs !! c in
                                appendFile outFile $ drawLine (rx, ry) (a, b)
                            else
                                return ()
                        else
                            return ()

--------------------------------------------------------------------------------
drawLinesC :: String -> [(Bool, (Double, Double), Double)] -> IO ()
drawLinesC outFile xs = do
    appendFile outFile "2.0 setlinewidth\n"
    let fst3 = \(x, _, _) -> x
    let snd3 = \(_, y ,_) -> y
    let thd3 = \(_, _, z) -> z
    let len = length xs
    sequence_ $ do
        p <- [0..len-1]
        return $ do
            let (rx, ry) = snd3 (xs !! p)   
            sequence_ $ do
                let child = p*2
                c <- [child+1,child+2]
                return $ do
                    if(c < len)
                        then
                            if((fst3 $ xs !! c) == True)
                            then
                                do 
                                    let (a, b) = snd3 $ xs !! c
                                    let color = thd3 $ xs !! c
                                    appendFile outFile $ 
                                        (show color) ++ " 0.0 " ++ (show $ 1- color) ++ " c\n"
                                    appendFile outFile $ drawLine (rx, ry) (a, b)
                            else return ()
                        else return ()

--------------------------------------------------------------------------------
drawLinesC2 :: [(Bool, (Double, Double), Double)] -> String
drawLinesC2 xs = 
    "2.0 setlinewidth\n" ++ foldr1 (++) 
        (map (\n -> foobar xs n) $ ([0 .. ((length xs)-1)] :: [Int]))
    where
        foobar :: [(Bool, (Double, Double), Double)] -> Int -> String
        foobar xs n =
            let len = length xs
                x = (xs !! n)
                (rx, ry) = snd3 x
                child = n*2
            in
                branch (child+1) len x (rx, ry) ++
                branch (child+2) len x (rx, ry)
        branch :: Int -> Int -> (Bool, (Double, Double), Double) -> (Double, Double) -> String 
        branch c len x (rx, ry) = if (c < len) then
                    if (fst3 x == True)
                    then
                        do
                            let (a, b) = snd3 $ xs !! c
                            let color = thd3 $ xs !! c
                            (show color) ++ " 0.0 " ++ (show (1.0 - color)) ++ " c\n" ++ 
                                drawLine (rx, ry) (a, b)
                    else ""
                else ""
        fst3 = \(x,_,_) -> x
        snd3 = \(_,y,_) -> y
        thd3 = \(_,_,z) -> z
    
--------------------------------------------------------------------------------
drawGrid :: String -> [(Bool, (Double, Double), String)] -> Integer -> IO ()
drawGrid outFile ls treeHeight = do
    appendFile outFile "0.7 0.7 0.7 c\n"
    appendFile outFile "0.2 setlinewidth\n"
    sequence_ $ do
        i <- [1..treeHeight]
        return $ do
            let diameter = sum $ take (fromInteger i) [0.0,d..dMax]
                    where 
                        d = scale * 2
                        dMax = d * fromIntegral (treeHeight) 
            let (_, (x, y), _) = head ls
            appendFile outFile $ 
                (show x) ++ " " ++ (show y) ++ " " ++ 
                (show diameter) ++ " 0 360 arc\n"
            appendFile outFile "stroke\n"

--------------------------------------------------------------------------------
drawRegex :: String -> String -> IO ()
drawRegex outFile regex = do
    if showRegex 
        then do
            appendFile outFile "0.1 0.4 0.9 c\n"
            appendFile outFile "/Courier-Bold findfont\n"
            appendFile outFile "11 scalefont\n"
            appendFile outFile "setfont\n"
            appendFile outFile "gsave\n"
            appendFile outFile "5 5 moveto\n"
            appendFile outFile $ "(" ++ regex ++ ") show\n"
            appendFile outFile "grestore\n"
        else do return ()

--------------------------------------------------------------------------------
-- konvertiert einen RX (Syntaxbaum, interpretiert als binaerer Baum) in ein
-- Array, mit den Positionen im einem vollstaendigen binaeren Baum
--
-- z.B.                       *
--                           / 
-- (aa+b)*      =>          +             =>       [1,2,4,5,8,9]
--                         / \
--                        .   b
--                       / \
--                      a   a
--
rxTreePos :: RX Char -> [Integer]
rxTreePos rx = map(\x -> fst x) $ rxTreePosP rx

--------------------------------------------------------------------------------
rxTreePosP :: RX Char -> [(Integer, String)]
rxTreePosP rx = traverse rx 0
    where traverse rx' i =
            case rx' of
            Letter c    -> [(pos, [c])]
            Dot a b     -> binary "." a b 
            Union a b   -> binary "+" a b
            PowerStar a -> unary  "*" a
            where
                unary s a    = [(pos, s)] ++ (travl a) -- immer links
                binary s a b = [(pos, s)] ++ (travl a) ++ (travr b)
                travl x      = traverse x (2*i+1) 
                travr x      = traverse x (2*i+2)
                pos          = i+1

--------------------------------------------------------------------------------
mapCoordinates :: [(Double, Double)] -> Integer -> [(Double, Double)]
mapCoordinates ps bb = map (\ (x, y) -> (x + xOffset, y + yOffset)) ps
    where
        xOffset = fromIntegral bb
        yOffset = fromIntegral bb

--------------------------------------------------------------------------------
writeCircGrid :: 
    String -> String -> Integer -> [(Bool, (Double, Double), String)] -> Integer -> 
    IO ()
writeCircGrid outFile regex treeHeight ls bb = do
    writeFile outFile "%!PS\n"
    let bbs = show (bb + bb)
    appendFile outFile $ "%%BoundingBox: 0 0 " ++ bbs ++ " " ++ bbs ++ "\n"
    appendFile outFile $ "/c {/DeviceRGB setcolorspace setrgbcolor } def\n"
    drawGrid outFile ls treeHeight
    drawCircles outFile ls
    drawLines outFile ls
    drawNodeLabels outFile ls
    drawRegex outFile regex
    appendFile outFile "showpage\n"

--------------------------------------------------------------------------------
writeCircGrid2 :: 
    String -> String -> Integer -> [(Bool, (Double, Double), Double)] -> 
    Integer ->  
    IO ()
writeCircGrid2 outFile regex treeHeight ls bb = do
    writeFile outFile "%!PS\n"
    let bbs = show (bb + bb)
    appendFile outFile $ "%%BoundingBox: 0 0 " ++ bbs ++ " " ++ bbs ++ "\n"
    appendFile outFile $ "/c {/DeviceRGB setcolorspace setrgbcolor } def\n"
    --drawGrid outFile ls treeHeight
    --drawCircles outFile ls
    --print $ drawLinesC2 ls
    --appendFile outFile $ drawLinesC2 ls
    drawLinesC outFile ls
    appendFile outFile "showpage\n"

--------------------------------------------------------------------------------
scale :: Double
scale = 8.0

circleSize :: Double
circleSize = 0.4

showRegex :: Bool
showRegex = True

--------------------------------------------------------------------------------
regex2CircGrid :: String -> String -> IO ()
regex2CircGrid outFile regex = do
    let is = sort $ rxTreePos rx where rx = (read regex) :: (RX Char)
    -- TODO: verwenden um op / character zu uebergeben
    -- TODO: eigene Funktion
    let is' = sortBy (\(_,a) (_,b) -> if a < b then LT else GT) $
            rxTreePosP rx where rx = (read regex) :: (RX Char)
    let kMax = last is
    let treeHeight = toInteger (exponent (fromIntegral kMax))
    let ks = [1..kMax]
    let bs = map (\x -> elem x is) ks
    let vs = map(\(k, b) -> getNodeValue k b) (zip ks bs)
            where 
                getNodeValue :: Integer -> Bool -> String
                getNodeValue key doSearch =
                    if doSearch then 
                    case (lookup key is') of 
                        Just a  -> a
                        Nothing -> error "invalid input data"
                    else ""
    let ps = map (\x -> gridPosS' x scale) ks
    let boundingBox = 20 + (max maxX maxY)
            where
                -- geht bestimmt auch eleganter
                maxX = round $ last $ sort $ map (\(x,_) -> abs x) ps
                maxY = round $ last $ sort $ map (\(_,y) -> abs y) ps
    let rs = mapCoordinates ps boundingBox
    writeCircGrid outFile regex treeHeight (zip3 bs rs vs) boundingBox

--------------------------------------------------------------------------------
-- TODO: kMax ist leicht mal > 1 Mio. -> out of memory
-- Impl. vollkommen ineffizient ! bei d = 24 -> max. 2^24 Listeneintraege !
pop2CircGrid :: String -> [String] -> IO ()
pop2CircGrid outFile xs = do
    let rxs = map (\regex -> (read regex) :: (RX Char)) xs
    let is = map (\x -> sort $ rxTreePos x) rxs

    let kMax = foldr1 max $ map last is
    let treeHeight = toInteger (exponent (fromIntegral kMax))
    putStrLn $ "d = " ++ (show treeHeight)

    let ks = [1..kMax]
    let bs = map (\is' -> map (\x -> elem x is') ks) is
    let bsT = transpose bs
    
    -- Gridpositionen fuer 1 .. kMax
    let ps = map (\x -> gridPosS' x scale) ks
    -- Anzahlen wie haeufig die Gridpositionen in den Baeumen vorkommen
    let cs = map (\x -> sum [if y then 1 else 0 | y <- x]) bsT
    -- [Bool] ist True, wenn die Gridposition vorkommt, sonst False
    let bs' = map or bsT
    
    let boundingBox = 20 + (max maxX maxY)
            where
                -- geht bestimmt auch eleganter
                maxX = round $ last $ sort $ map (\(x,_) -> abs x) ps
                maxY = round $ last $ sort $ map (\(_,y) -> abs y) ps
    let rs = mapCoordinates ps boundingBox
    let maxC = foldr1 max cs
    let colorMapping =  [ (1 / maxC) * x | x <- [0 .. maxC ] ] -- rgb
    let colors = map (\c -> colorMapping !! c) $ map (\x -> round x) cs
    let foob = (zip3 bs' rs colors)
    
    writeCircGrid2 outFile "" treeHeight foob boundingBox