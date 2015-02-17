
module Visualization.Dot where

import Autolib.Exp.Read ()
import Autolib.Exp.Type

--------------------------------------------------------------------------------
--
regexToDot :: String -> String
regexToDot regex =
	let rx =  (read regex) :: (RX Char) in rxToDot rx

--------------------------------------------------------------------------------
--
rxToDot :: RX Char -> String
rxToDot rx = 
	"digraph G {\n" ++
	"node [" ++
	" fontname=\"Courier-Bold\", fontcolor=red, fontsize=16," ++
	" shape=circle, height=0.2, width=0.2]\n" ++
	travPath rx 0 ++
	travNodes rx 0 ++
	"}\n" 
	where 
	travPath rx i = 
		case rx of
			Letter c    -> node [c] ++ " [arrowhead=none]\n"
			Dot a b     -> binary "." a b
			Union a b   -> binary "+" a b
			PowerStar a -> unary  "*" a
		where
			unary s a    = (travl s a) -- immer links
			binary s a b = (travl s a) ++ (travr s b)
			travl s x    = (arrow s) ++ travPath x (2*i+1) 
			travr s x    = (arrow s) ++ travPath x (2*i+2)
			arrow s      = node s ++ " -> "
			node s       = "\"" ++ s ++ show (i+1) ++ "\""
	travNodes rx i =
		case rx of
			Letter c    -> node [c]
			Dot a b     -> binary "." a b
			Union a b   -> binary "+" a b
			PowerStar a -> unary  "*" a
		where
			unary s a    = node s ++ (travl s a) -- immer links
			binary s a b = node s ++ (travl s a) ++ (travr s b)
			travl s x    = travNodes x (2*i+1) 
			travr s x    = travNodes x (2*i+2)
			node s       = 
				"\"" ++ s ++ show (i+1) ++ "\" [label=\"" ++ s ++ "\"]\n"
