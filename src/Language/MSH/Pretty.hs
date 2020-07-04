module Language.MSH.Pretty (
    module Text.PrettyPrint,

    Pretty(..),
    conditionally,
    optionally
) where

import Text.PrettyPrint

class Pretty a where
    pp :: a -> Doc

optionally :: Maybe a -> (a -> Doc) -> Doc
optionally Nothing f = empty
optionally (Just x) f = f x

conditionally :: Bool -> Doc -> Doc 
conditionally False _ = empty
conditionally True f = f
