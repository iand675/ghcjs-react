{-# LANGUAGE OverloadedStrings #-}
module React.Addons where

import qualified Data.Foldable as F
import Data.Maybe (catMaybes)
import Data.JSString (JSString)
import GHCJS.Types
import React
import React.DOM (className_)

foreign import javascript unsafe "React.addons.CSSTransitionGroup" js_cssTransitionGroup :: ReactClass OnlyAttributes

cssTransition :: ReactProps OnlyAttributes -> Maybe (Array ReactNode) -> ReactNode
cssTransition = runFactory' fact
  where
    fact = createFactory js_cssTransitionGroup
{-# NOINLINE cssTransition #-}

cssTransitionGroup :: (Foldable elems) => JSString -> TransitionGroup -> elems ReactNode -> ReactNode
cssTransitionGroup cl g es = cssTransition builtProps $ if Prelude.null es
  then Nothing
  else Just $ array $ F.toList es
  where
    builtProps = buildProps $ catMaybes
      [ Just (className_ cl)
      , Just (PropName "transitionName" .: (either jsval (jsval . buildProps . map (uncurry (\k v -> PropName k .: v))) $ transitionName g))
      , (\x -> PropName "transitionEnterTimeout" .: x) <$> transitionEnterTimeout g
      , (\x -> PropName "transitionLeaveTimeout" .: x) <$> transitionLeaveTimeout g
      , (\x -> PropName "transitionEnter" .: x) <$> transitionEnter g
      , (\x -> PropName "transitionLeave" .: x) <$> transitionLeave g
      , (\x -> PropName "transitionAppear" .: x) <$> transitionAppear g
      ]

data TransitionGroup = TransitionGroup
  { transitionName         :: Either JSString [(JSString, JSString)]
  , transitionEnterTimeout :: Maybe Int
  , transitionLeaveTimeout :: Maybe Int
  , transitionEnter        :: Maybe Bool
  , transitionLeave        :: Maybe Bool
  , transitionAppear       :: Maybe Bool
  }

simpleTransition :: JSString -> TransitionGroup
simpleTransition str = TransitionGroup (Left str) Nothing Nothing Nothing Nothing Nothing

complexTransition :: [(JSString, JSString)] -> TransitionGroup
complexTransition strs = TransitionGroup (Right strs) Nothing Nothing Nothing Nothing Nothing
