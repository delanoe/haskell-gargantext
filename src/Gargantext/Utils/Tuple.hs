module Gargantext.Utils.Tuple where

import Protolude



uncurryMaybe :: (Maybe a, Maybe b) -> Maybe (a, b)
uncurryMaybe (Nothing, _) = Nothing
uncurryMaybe (_, Nothing) = Nothing
uncurryMaybe (Just a, Just b) = Just (a, b)

uncurryMaybeSecond :: (a, Maybe b) -> Maybe (a, b)
uncurryMaybeSecond (_, Nothing) = Nothing
uncurryMaybeSecond (a, Just b) = Just (a, b)
