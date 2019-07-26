{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings, BlockArguments #-}

-- | Using the SAX parser, provide a simple tree interface.

module Xeno.Tree where

import Control.DeepSeq
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import GHC.Generics
import Xeno.Types
import Xeno.SAX

data Node
  = Element !ByteString ![(ByteString, ByteString)] ![Node]
  | Text !ByteString
  deriving (Show, Generic, NFData)

data State
  = Start
  | Nodes [Node]
  | Open ByteString [(ByteString,ByteString)] [Node] State
  deriving (Show, Generic, NFData)

parse :: ByteString -> Either XenoException [Node]
parse i =
  case run i of
    Left e -> Left e
    Right r -> Right $ case r of
        Start -> []
        Nodes ns -> ns
        Open {} -> error "Missing closing tag."
  where
    run =
      fold
        do \s name -> Open name [] [] s
        do \s key value ->
            case s of
                 Open name attrs cs s' -> Open name ((key, value) : attrs) cs s'
                 _ -> error "Unexpected attributes."
        do \s _ -> s
        do \s text ->
            let node = Text text
                in case s of
                        Start -> Nodes [node]
                        Nodes ns -> Nodes (ns ++ [node])
                        Open name attrs cs s' -> Open name attrs (cs ++ [node]) s'
        do \s name0 -> case s of
             Open name attrs cs s'
               | name == name0 ->
                 let node = Element name attrs cs
                 in case s' of
                      Start -> Nodes [node]
                      Nodes cs' -> Nodes (cs' ++ [node])
                      Open name' attrs' cs' s'' ->
                        Open name' attrs' (cs' ++ [node]) s''
             _ -> error "Unexpected closing attribute."
        do \s _ -> s
        Start

indent :: Int -> ByteString -> ByteString
indent c = mappend $ "\n" <> B.replicate c (toEnum $ fromEnum ' ')

render :: [Node] -> ByteString
render = mconcat . map (go 0)
  where
    go c (Text t) = indent c  t
    go c (Element name attrs children) =
        indent c ("<" <> name <> mconcat (map (\(key, val) -> " " <> key <> "=\"" <> val <> "\"") attrs) <> ">")
        <>
        mconcat (map (go $ c + 2) children) 
        <>
        indent c ("</" <> name <> ">")
