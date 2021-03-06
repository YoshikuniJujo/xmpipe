{-# LANGUAGE OverloadedStrings, TupleSections, TypeFamilies, PackageImports #-}

module Tools (
	SHandle(..),
	fromHandleLike, toHandleLike,
	hlpDebug, voidM, nullQ, myFromJust,
	) where

import "monads-tf" Control.Monad.State
import Data.Pipe
import Data.HandleLike
import Text.XML.Pipe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

fromHandleLike :: HandleLike h => h -> Pipe () BS.ByteString (HandleMonad h) ()
fromHandleLike h = lift (hlGetContent h) >>= ((>> fromHandleLike h) . yield)

toHandleLike :: HandleLike h => h -> Pipe BS.ByteString () (HandleMonad h) ()
toHandleLike h = await >>= maybe (return ()) ((>> toHandleLike h) . lift . hlPut h)

hlpDebug :: (HandleLike h, Show a) => h -> Pipe a a (HandleMonad h) ()
hlpDebug h = await >>= maybe (return ())
	(\x -> lift (hlPrint h x) >> yield x >> hlpDebug h)

hlPrint :: (HandleLike h, Show a) => h -> a -> HandleMonad h ()
hlPrint h = hlDebug h "critical" . BSC.pack . (++ "\n") . show

data SHandle s h = SHandle h

instance HandleLike h => HandleLike (SHandle s h) where
	type HandleMonad (SHandle s h) = StateT s (HandleMonad h)
	type DebugLevel (SHandle s h) = DebugLevel h
	hlPut (SHandle h) = lift . hlPut h
	hlGet (SHandle h) = lift . hlGet h
	hlClose (SHandle h) = lift $ hlClose h
	hlDebug (SHandle h) = (lift .) . hlDebug h

voidM :: Monad m => m a -> m ()
voidM = (>> return ())

myFromJust :: String -> Maybe a -> a
myFromJust _ (Just x) = x
myFromJust msg _ = error msg
