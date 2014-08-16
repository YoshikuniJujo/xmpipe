{-# LANGUAGE PackageImports #-}

import "monads-tf" Control.Monad.Error

-- failure :: ErrorT String IO ()
failure :: Monad m => m ()
failure = do
	[] <- return "hello"
	return ()
