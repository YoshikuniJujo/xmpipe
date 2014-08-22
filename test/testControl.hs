{-# LANGUAGE PackageImports, FlexibleContexts #-}

import "monads-tf" Control.Monad.State
import Control.Monad.Trans.Control
import qualified GHC.Event as E

registerTimeout :: MonadBaseControl IO m =>
	E.EventManager -> Int -> m () -> m E.TimeoutKey
registerTimeout ev i cb = control $ \runInIO ->
	E.registerTimeout ev i (runInIO cb >> return ())
