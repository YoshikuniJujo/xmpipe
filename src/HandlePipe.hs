{-# LANGUAGE OverloadedStrings, PackageImports #-}

module HandlePipe (
	fromHandleLike, fromHandleLikeT,
	hlpDebug, hlpDebugT,
	) where

import "monads-tf" Control.Monad.Trans
import Data.Pipe
import Data.HandleLike

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

fromHandleLike :: HandleLike h => h -> Pipe () BS.ByteString (HandleMonad h) ()
fromHandleLike h = lift (hlGetContent h) >>= ((>> fromHandleLike h) . yield)

fromHandleLikeT :: (HandleLike h, MonadTrans t, Monad (t (HandleMonad h))) =>
	h -> Pipe () BS.ByteString (t (HandleMonad h)) ()
fromHandleLikeT h =
	lift (lift $ hlGetContent h) >>= ((>> fromHandleLikeT h) . yield)

hlpDebug :: (HandleLike h, Show a) => h -> Pipe a a (HandleMonad h) ()
hlpDebug h = await >>= maybe (return ())
	(\x -> lift (hlPrint h x) >> yield x >> hlpDebug h)

hlpDebugT :: (HandleLike h, Show a, MonadTrans t, Monad (t (HandleMonad h))) => 
	h -> Pipe a a (t (HandleMonad h)) ()
hlpDebugT h = await >>= maybe (return ())
	(\x -> lift (lift $ hlPrint h x) >> yield x >> hlpDebugT h)

hlPrint :: (HandleLike h, Show a) => h -> a -> HandleMonad h ()
hlPrint h = hlDebug h "critical" . BSC.pack . (++ "\n") . show
