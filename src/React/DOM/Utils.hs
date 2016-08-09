module React.DOM.Utils
  ( eventFormValue
  ) where

import Control.Monad.Trans
import GHCJS.DOM.EventTarget
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.Types
import Unsafe.Coerce

-- | Form targets all return strings, so make it simpler to get at the string without having
-- to reimport the numerous GHCJS.DOM modules
eventFormValue :: (MonadIO m, FromJSString string) => EventTarget -> m (Maybe string)
eventFormValue t = liftIO $ do
  let el = unsafeCoerce t
  getValue (el :: HTMLInputElement)
