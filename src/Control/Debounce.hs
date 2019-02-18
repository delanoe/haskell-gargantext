{- This file is part of json-state.
 - Imported in haskell-gargantext.
 -
 - Written in 2015 by fr33domlover <fr33domlover@rel4tion.org>.
 -
 - ♡ Copying is an act of love. Please copy, reuse and share.
 -
 - The author(s) have dedicated all copyright and related and neighboring
 - rights to this software to the public domain worldwide. This software is
 - distributed without any warranty.
 -
 - You should have received a copy of the CC0 Public Domain Dedication along
 - with this software. If not, see
 - <http://creativecommons.org/publicdomain/zero/1.0/>.
 -}

-- | This is similar to the same module from "auto-update" package, except here
-- the caller can pass a parameter to the debounced action. Also, the returned
-- action comes in 2 versions.
--
-- The first is non-blocking at the cost of a small chance a parameter isn't
-- passed and is instead discarded. This can happen if the action is called
-- from different threads simultanously. One empties the 'MVar', and the other
-- happens to fill it first, and then the parameter the former thread passed is
-- discarded. If you run the action from a single thread, there is no problem,
-- or if missing at a hopefully small chance isn't a problem.
--
-- The second is blocking, but only in the small chance described above.
-- Otherwise it doesn't block in practice.
--
-- Also, exceptions aren't handled. This includes async exceptions and any
-- exceptions thrown by the given action.
module Control.Debounce
    ( mkDebounce
    )
where

import Control.Monad (forever, void)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Data.Time.Units

mkDebounce :: TimeUnit t
           => t               -- ^ Time delay between calls to the action
           -> (a -> IO ())    -- ^ Action to perform
           -> IO ( a -> IO () --   Never-blocking version
                 , a -> IO () --   Possibly-blocking version
                 )
mkDebounce interval action = do
    paramVar <- newEmptyMVar
    let run = void $ forkIO $ forever $ do
            param <- takeMVar paramVar
            action param
            threadDelay $ fromInteger $ toMicroseconds interval
        actNB param = do
            void $ tryTakeMVar paramVar
            void $ tryPutMVar paramVar param
        actPB param = do
            void $ tryTakeMVar paramVar
            putMVar paramVar param
    run
    return (actNB, actPB)
