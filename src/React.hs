{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StandaloneDeriving         #-}
module React where
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (traverse_)
import qualified Data.Foldable as F
import Data.Text (Text)
import Data.JSString.Text
import Data.Traversable
import Data.IORef
import qualified Data.JSString as JSString
import Data.String (IsString(..))
import qualified Data.Text      as S
import qualified Data.Text.Lazy as L
import Data.Typeable
import qualified GHCJS.DOM.AnimationEvent   as Animation
import qualified GHCJS.DOM.CompositionEvent as Composition
import qualified GHCJS.DOM.KeyboardEvent    as Keyboard
import qualified GHCJS.DOM.FocusEvent       as Focus
import qualified GHCJS.DOM.MouseEvent       as Mouse
import qualified GHCJS.DOM.TouchEvent       as Touch
import qualified GHCJS.DOM.TransitionEvent  as Transition
import GHCJS.DOM.DataTransfer
import GHCJS.DOM.Element
import GHCJS.DOM.EventTarget (EventTarget)
import GHCJS.DOM.TouchList
import GHCJS.DOM.Types (GObject(..), IsGObject(..), IsEventTarget(..))
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import GHCJS.Foreign.Export
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Nullable (Nullable(..), nullableToMaybe)
import GHCJS.Types
import qualified JavaScript.Object as Object
import qualified JavaScript.Object.Internal as OI
import JavaScript.Array
import Language.Haskell.TH
import System.IO.Unsafe
import Unsafe.Coerce

class ElementType e where
  reactVal :: e -> JSVal

instance ElementType JSString where
  reactVal = jsval

instance ElementType (ReactClass ps) where
  reactVal (ReactClass c) = c

newtype ReactClass ps = ReactClass JSVal

newtype ReactElement = ReactElement { fromReactElement :: JSVal } -- Either function for stateless or component

newtype ReactInstance ps st = ReactInstance { fromReactInstance :: JSVal }

instanceThis :: ReactInstance ps st -> This ps st
instanceThis = This . fromReactInstance

instanceElement :: ReactInstance ps st -> Element
instanceElement = unsafeCastGObject . GObject . fromReactInstance

instance PToJSVal ReactElement where
  pToJSVal = fromReactElement

instance ToJSVal ReactElement where
  toJSVal = return . fromReactElement

instance FromJSVal ReactElement where
  fromJSVal = return . Just . ReactElement

instance IsString ReactElement where
  fromString = ReactElement . pToJSVal . JSString.pack

-- TODO these should probably be conditonally included only for GHCJS
instance IsGObject ReactElement where
  toGObject (ReactElement val) = GObject val
  unsafeCastGObject (GObject val) = ReactElement val

instance IsEventTarget ReactElement

newtype PropName v = PropName JSString

data EventHandler ps st a = HsEventHandler (Callback (JSVal -> JSVal -> IO ())) JSVal
                          | JSEventHandler JSVal

instance PToJSVal (EventHandler ps st a) where
  pToJSVal (HsEventHandler _ v) = v
  pToJSVal (JSEventHandler v) = v

eventHandler :: (e -> ReactM ps st ()) -> RendererM (EventHandler ps st e)
eventHandler f = RendererM $ do
  cb <- liftIO $ syncCallback2 ThrowWouldBlock (\this e -> runReaderT (fromReactM $ f $ unsafeCoerce e) $ This this)
  let cleaner = (>> releaseCallback cb)
  modify cleaner
  return $ HsEventHandler cb (jsval cb)

jsEventHandler :: JSVal -> EventHandler ps st a
jsEventHandler = JSEventHandler

callback :: (Typeable a, Typeable b, Show a, Show b) => (a -> b) -> RendererM (Callback (Export a -> IO (Export b)))
callback f = RendererM $ do
  cb <- liftIO $ syncCallback1' $ \exp -> do
    v <- derefExport (unsafeCoerce exp :: Export a)
    case v of
      Nothing -> Prelude.error "Mismatched type or freed export value"
      Just x -> do
        print x
        let res = f x
        print res
        jsval <$> export res
  let cleaner = (>> releaseCallback cb)
  modify cleaner
  return $ unsafeCoerce cb

data Prop = Prop {-# UNPACK #-} !JSString {-# UNPACK #-} !JSVal

newtype Props ps = Props Object.Object
              deriving (IsJSVal)

instance ToJSVal (Props ps) where
  toJSVal (Props o) = return $ unsafeCoerce o
instance FromJSVal (Props ps) where
  fromJSVal = return . Just . unsafeCoerce

foreign import javascript "Object.assign.apply(null, $1)" assign :: JSArray -> IO Object.Object

instance Monoid (Props ps) where
  mempty = Props $ unsafePerformIO Object.create
  mappend (Props o1) (Props o2) = unsafePerformIO $ do
    o <- Object.create
    oo <- fromListIO [jsval o, jsval o1, jsval o2]
    res <- assign oo
    return $ Props res

newtype SanitizedHtml = SanitizedHtml JSString
  deriving (Eq, Show)

instance PToJSVal SanitizedHtml where
  pToJSVal (SanitizedHtml h) = unsafePerformIO $ do
    o <- Object.create
    Object.setProp "__html" (jsval h) o
    return $ jsval o

instance FromJSVal SanitizedHtml where
  fromJSVal r = case jsonTypeOf r of
    JSONObject -> do
      let o = OI.Object r
      str <- Object.getProp "__html" o
      fmap SanitizedHtml <$> fromJSVal str
    _ -> return Nothing
  {-# INLINE fromJSVal #-}

instance ToJSVal SanitizedHtml where
  toJSVal = return . pToJSVal
  {-# INLINE toJSVal #-}

children :: Props st -> Maybe (Array ReactElement)
children (Props o) =
  let p = unsafePerformIO (Object.getProp "children" o)
  in if isUndefined p then Nothing else Just (unsafeCoerce p)



newtype Array a = Array JSArray
                deriving (IsJSVal)

instance PToJSVal (Array a) where
  pToJSVal = unsafeCoerce

array :: PToJSVal a => [a] -> Array a
array = Array . fromList . map pToJSVal

foreign import javascript unsafe "captureThis($1)" captureThis :: Callback a -> JSVal
foreign import javascript unsafe "captureThis($1, $2)" captureThis' :: JSVal -> This ps st -> JSVal

foreign import javascript unsafe "console.log($1)" js_printWhatever :: JSVal -> IO ()

printWhatever :: a -> IO ()
printWhatever = js_printWhatever . unsafeCoerce

foreign import javascript unsafe "React.createClass($1)" createClass :: ReactSpec ps st -> ReactClass ps

foreign import javascript unsafe "React.createElement($1, $2, $3)" js_createElement :: JSVal -> Props ps -> JSVal -> ReactElement

nullish :: IsJSVal j => Maybe j -> JSVal
nullish ma = case ma of
  Nothing -> jsNull
  Just a -> jsval a

createElement :: (Applicative t, Foldable t, Foldable elems) => ReactClass ps -> t Prop -> elems ReactElement -> ReactElement
createElement t ps es = createElement' t (buildProps ps) (if Prelude.null es then Nothing else Just $ array $ F.toList es)

createElement' :: ReactClass ps -> Props ps -> Maybe (Array ReactElement) -> ReactElement
createElement' t ps ma = js_createElement (reactVal t) ps $ case ma of
  Nothing -> jsNull
  Just (Array a) -> jsval a

newtype Factory ps = Factory {factoryVal :: JSVal}

foreign import javascript "$1($2,$3)" fromFactory :: Factory ps -> Props ps -> JSVal -> ReactElement

foreign import javascript unsafe "React.createFactory($1)" createFactory :: ReactClass ps -> Factory ps

runFactory :: (Applicative t, Foldable t, Foldable elems) => Factory ps -> t Prop -> elems ReactElement -> ReactElement
runFactory f ps es = runFactory' f (buildProps ps) (if Prelude.null es then Nothing else Just $ array $ F.toList es)

runFactory' :: Factory ps -> Props ps -> Maybe (Array ReactElement) -> ReactElement
runFactory' f ps ma = (fromFactory f) ps $ case ma of
  Nothing -> jsNull
  Just (Array a) -> jsval a

foreign import javascript unsafe "React.cloneElement($1, $2, $3)" cloneElement :: ReactElement -> Props ps -> Array ReactElement -> ReactElement

foreign import javascript unsafe "React.isValidElement($1)" isValidElement :: JSVal -> IO Bool

{-
-- React.PropTypes
-- React.Children
-}
-- ReactDOM

foreign import javascript unsafe "ReactDOM.render($1, $2, $3)" js_render :: ReactElement -> Element -> JSVal -> IO ()

render :: IsElement e => ReactElement -> e -> Maybe (This ps st -> IO ()) -> IO ()
render re e mf = case mf of
  Nothing -> js_render re (toElement e) jsUndefined
  Just f -> do
    selfCleaner <- newIORef undefined
    cb <- syncCallback1 ContinueAsync $ \ref -> do
      f $ This ref
      cb <- readIORef selfCleaner
      releaseCallback cb
    writeIORef selfCleaner cb
    js_render re (toElement e) (captureThis cb)

foreign import javascript unsafe "ReactDOM.unmountComponentAtNode($1)" js_unmountComponentAtNode :: Element -> IO Bool

unmountComponentAtNode :: IsElement e => e -> IO Bool
unmountComponentAtNode = js_unmountComponentAtNode . toElement

foreign import javascript unsafe "ReactDOM.findDOMNode($1)" js_findDOMNode :: ReactInstance ps st -> IO (Nullable Element)

findDOMNode :: MonadIO m => ReactInstance ps st -> m (Maybe Element)
findDOMNode c = liftIO (nullableToMaybe <$> js_findDOMNode c)

{-
-- React.Component
-}

foreign import javascript unsafe "$1['props']" js_getProps :: This ps st -> IO (Props ps)
getProps :: ReactM ps st (Props ps)
getProps = do
  v <- ask
  liftIO $ js_getProps v

getProperties :: FromJSVal ps => ReactM ps st (Properties ps)
getProperties = getProps >>= liftIO . readProperties . unsafeCoerce

foreign import javascript unsafe "$1['state']" js_getState :: This ps st -> IO JSVal
getState :: FromJSVal st => ReactM ps st st
getState = do
  v <- ask
  r <- liftIO $ js_getState v
  liftIO $ readState r

foreign import javascript unsafe "$1['setState']($2)" js_setState :: This ps st -> JSVal -> IO ()

setState :: ToJSVal st => st -> ReactM ps st ()
setState x = do
  v <- ask
  xv <- liftIO $ toJSVal x
  liftIO $ js_setState v xv

setState' :: (ToJSVal st, FromJSVal st) => (st -> Props ps -> st) -> ReactM ps st ()
setState' f = do
  cb <- liftIO $ syncCallback2' $ \st ps -> do
    stv <- readState st
    liftIO $ toJSVal $ f stv (Props $ unsafeCoerce ps)
  v <- ask
  liftIO $ js_setState v (jsval cb)
  liftIO $ releaseCallback cb

modifyState :: (ToJSVal st, FromJSVal st) => (st -> st) -> ReactM ps st ()
modifyState f = fmap f getState >>= setState

-- forceUpdate :: f -> IO ()

-- Component Specs and Lifecycle
{-
newtype ReactM propPerms statePerms refPerms a = ReactM (ReaderT ReactThis IO a)
-}

data OnlyAttributes = OnlyAttributes
  deriving (Show, Eq)

instance ToJSVal OnlyAttributes where
  toJSVal _ = unsafeCoerce <$> Object.create

instance FromJSVal OnlyAttributes where
  fromJSVal _ = return $ Just OnlyAttributes
  fromJSValUnchecked _ = return OnlyAttributes

newtype ReactM ps st a = ReactM {fromReactM :: ReaderT (This ps st) IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runReactM :: MonadIO m => This ps st -> ReactM ps st a -> m a
runReactM t m = liftIO $ runReaderT (fromReactM m) t

deriving instance MonadBase IO (ReactM ps st)
instance MonadBaseControl IO (ReactM ps st) where
  type StM (ReactM ps st) a = a
  liftBaseWith f = ReactM $ liftBaseWith $ \q -> f (q . fromReactM)
  restoreM = ReactM . restoreM

instance MonadReader (This ps st) (ReactM ps st) where
  ask = ReactM ask
  local f = ReactM . local f . fromReactM

newtype RendererM a = RendererM {fromRendererM :: StateT (IO ()) IO a }
  deriving (Functor, Applicative, Monad)

data Spec ps st = Spec
            { renderSpec                :: RendererM (ReactM ps st ReactElement)
            , getInitialState           :: Maybe (ReactM ps st st)
            , getDefaultProps           :: Maybe (IO (Props ps))
            -- , propTypes                 :: Maybe PropTypechecker
            -- , mixins                    :: Maybe (Array Mixin)
            , statics                   :: Maybe Object.Object
            , displayName               :: Maybe JSString
            , componentWillMount        :: Maybe (ReactM ps st ())
            -- ^ Invoked once, both on the client and server, immediately before the initial rendering occurs. If you call setState within this method, render() will see the updated state and will be executed only once despite the state change.
            , componentDidMount         :: Maybe (ReactM ps st ())
            -- ^ Invoked once, only on the client (not on the server), immediately after the initial rendering occurs. At this point in the lifecycle, you can access any refs to your children (e.g., to access the underlying DOM representation). The componentDidMount() method of child components is invoked before that of parent components.
            -- If you want to integrate with other JavaScript frameworks, set timers using setTimeout or setInterval, or send AJAX requests, perform those operations in this method.
            , componentWillReceiveProps :: Maybe (Properties ps -> ReactM ps st ())
            -- ^ Invoked when a component is receiving new props. This method is not called for the initial render.
            -- Use this as an opportunity to react to a prop transition before render() is called by updating the state using this.setState(). The old props can be accessed via this.props. Calling this.setState() within this function will not trigger an additional render.

            , shouldComponentUpdate     :: Maybe (Properties ps -> st -> ReactM ps st Bool)
            , componentWillUpdate       :: Maybe (Properties ps -> st -> ReactM ps st ())
            , componentDidUpdate        :: Maybe (Properties ps -> st -> ReactM ps st ())
            , componentWillUnmount      :: Maybe (ReactM ps st ())
            }

spec :: (ToJSVal ps, FromJSVal ps) => RendererM (ReactM ps OnlyAttributes ReactElement) -> Spec ps OnlyAttributes
spec f = Spec f Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

statefulSpec :: (ToJSVal ps, FromJSVal ps, ToJSVal st, FromJSVal st) => ReactM ps st st -> RendererM (ReactM ps st ReactElement) -> Spec ps st
statefulSpec st f = Spec f (Just st) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data SpecMaintenance = SpecMaintenance { specMaintenanceFinalize :: IO () }

newtype ReactSpec ps st = ReactSpec Object.Object -- [object Object], haha

maybeSetProp :: JSString -> Maybe JSVal -> Object.Object -> IO ()
maybeSetProp propName mval o = case mval of
  Nothing -> return ()
  Just val -> Object.setProp propName val o
{-# INLINE maybeSetProp #-}

data Properties ps = Properties
  { propVal  :: ps
  , allProps :: Props ps
  }

readProperties :: FromJSVal ps => JSVal -> IO (Properties ps)
readProperties v = do
  ps <- fromJSVal v
  case ps of
    Nothing -> printWhatever v >> Prelude.error "Failed to parse props"
    Just r -> return (Properties r $ unsafeCoerce v)

readState :: FromJSVal st => JSVal -> IO st
readState v = do
  st <- fromJSVal v
  case st of
    Nothing -> printWhatever v >> Prelude.error "Failed to parse state"
    Just r -> return r

buildSpec :: (ToJSVal ps, FromJSVal ps, ToJSVal st, FromJSVal st) => Spec ps st -> IO (ReactSpec ps st, SpecMaintenance)
buildSpec s = do
  (renderer, cleanup) <- runStateT (fromRendererM $ renderSpec s) (return ())
  o <- Object.create
  renderCb <- syncCallback1' $ \this -> do
    fromReactElement <$> runReaderT (fromReactM renderer) (This this)

  Object.setProp "render" (captureThis renderCb) o

  mGetInitialStateCb <- for (getInitialState s) $ \f -> do
    getInitialStateCb <- syncCallback1' ((runReaderT (fromReactM f) . This) >=> toJSVal)
    Object.setProp "getInitialState" (captureThis getInitialStateCb) o
    return getInitialStateCb

  mGetDefaultPropsCb <- for (getDefaultProps s) $ \m -> do
    getDefaultPropsCb <- syncCallback' (toJSVal =<< m)
    Object.setProp "getDefaultProps" (jsval getDefaultPropsCb) o
    return getDefaultPropsCb

  maybeSetProp "statics" (unsafeCoerce <$> statics s) o
  maybeSetProp "displayName" (jsval <$> displayName s) o

  -- TODO refcount increment
  mWillMountCb <- for (componentWillMount s) $ \f -> do
    willMountCb <- syncCallback1 ThrowWouldBlock (runReaderT (fromReactM f) . This)
    Object.setProp "componentWillMount" (captureThis willMountCb) o
    return willMountCb

  mDidMountCb <- for (componentDidMount s) $ \f -> do
    didMountCb <- syncCallback1 ThrowWouldBlock (runReaderT (fromReactM f) . This)
    Object.setProp "componentDidMount" (captureThis didMountCb) o
    return didMountCb

  mWillReceiveProps <- for (componentWillReceiveProps s) $ \f -> do
    willReceivePropsCb <- syncCallback2 ThrowWouldBlock $ \t ps -> do
      ps' <- readProperties ps
      runReaderT (fromReactM $ f ps') $ This t
    Object.setProp "componentWillReceiveProps" (captureThis willReceivePropsCb) o
    return willReceivePropsCb

  mShouldComponentUpdate <- for (shouldComponentUpdate s) $ \f -> do
    cb <- syncCallback3' $ \t ps st -> do
      st' <- readState st
      ps' <- readProperties ps
      res <- runReaderT (fromReactM (f ps' st')) $ This t
      return $ pToJSVal res

    Object.setProp "shouldComponentUpdate" (captureThis cb) o
    return cb

  mWillUpdate <- for (componentWillUpdate s) $ \f -> do
    cb <- syncCallback3 ThrowWouldBlock $ \t ps st -> do
      st' <- readState st
      ps' <- readProperties ps
      runReaderT (fromReactM $ f ps' st') $ This t
    Object.setProp "componentWillUpdate" (captureThis cb) o
    return cb

  mDidUpdate <- for (componentDidUpdate s) $ \f -> do
    cb <- syncCallback3 ThrowWouldBlock $ \t ps st -> do
      st' <- readState st
      ps' <- readProperties ps
      runReaderT (fromReactM $ f ps' st') $ This t
    Object.setProp "componentDidUpdate" (captureThis cb) o
    return cb

  -- TODO refcount decrement
  mWillUnmount <- for (componentWillUnmount s) $ \f -> do
    cb <- syncCallback1 ThrowWouldBlock (runReaderT (fromReactM f) . This)
    Object.setProp "componentWillUnmount" (captureThis cb) o
    return cb

  let finalizer = do
        -- TODO return failure instead of freeing if refcount /= 0
        releaseCallback renderCb
        traverse_ releaseCallback mGetInitialStateCb
        traverse_ releaseCallback mGetDefaultPropsCb
        traverse_ releaseCallback mWillMountCb
        traverse_ releaseCallback mDidMountCb
        traverse_ releaseCallback mWillReceiveProps
        traverse_ releaseCallback mShouldComponentUpdate
        traverse_ releaseCallback mWillUpdate
        traverse_ releaseCallback mDidUpdate
        traverse_ releaseCallback mWillUnmount
        cleanup

  return (ReactSpec o, SpecMaintenance finalizer)

suppressContentEditableWarning :: PropName Bool
suppressContentEditableWarning = PropName "suppressContentEditableWarning"

dangerouslySetInnerHTML :: PropName SanitizedHtml
dangerouslySetInnerHTML = PropName "dangerouslySetInnerHTML"

key :: PropName JSString
key = PropName "key"

-- TODO support callback version?
ref :: PropName JSString
ref = PropName "ref"

(.:) :: (PToJSVal a) => PropName a -> a -> Prop
(PropName k) .: v = Prop k (pToJSVal v)

buildProps :: (Applicative t, Foldable t) => t Prop -> Props ps
buildProps ps = unsafePerformIO $ do
  o <- Object.create
  traverse_ (\(Prop k v) -> Object.setProp k v o) ps
  return $ Props o

props :: (PToJSVal ps, Applicative t, Foldable t) => ps -> t Prop -> Props ps
props pv ps = unsafePerformIO $ do
  eo <- Object.create
  o <- Object.create
  traverse_ (\(Prop k v) -> Object.setProp k v o) ps
  l <- toJSValListOf [unsafeCoerce eo, unsafeCoerce o, pToJSVal pv]
  merged <- assign $ unsafeCoerce l
  return $ unsafeCoerce merged

unsafeProps :: (ToJSVal ps, Applicative t, Foldable t) => ps -> t Prop -> Props ps
unsafeProps ps rest = unsafePerformIO $ do
  eo <- Object.create
  o <- Object.create
  traverse_ (\(Prop k v) -> Object.setProp k v o) rest
  pjv <- toJSVal ps
  l <- toJSValListOf [unsafeCoerce eo, unsafeCoerce o, pjv]
  merged <- assign $ unsafeCoerce l
  return $ unsafeCoerce merged

type EventProp ps st e = This ps st -> EventHandler ps st e -> Prop

eventProp :: JSString -> This ps st -> EventHandler ps st e -> Prop
eventProp n t h = PropName n .: case h of
  HsEventHandler _ f -> captureThis' f t
  JSEventHandler f -> captureThis' f t

onCopy, onCut, onPaste :: EventProp ps st ClipboardEvent
onCopy = eventProp "onCopy"
onCut = eventProp "onCut"
onPaste = eventProp "onPaste"

onCompositionEnd, onCompositionStart, onCompositionUpdate :: EventProp ps st CompositionEvent
onCompositionEnd = eventProp "onCompositionEnd"
onCompositionStart = eventProp "onCompositionStart"
onCompositionUpdate = eventProp "onCompositionUpdate"

onKeyDown, onKeyPress, onKeyUp :: EventProp ps st KeyboardEvent
onKeyDown = eventProp "onKeyDown"
onKeyPress = eventProp "onKeyPress"
onKeyUp = eventProp "onKeyUp"

onFocus, onBlur :: EventProp ps st FocusEvent
onFocus = eventProp "onFocus"
onBlur = eventProp "onBlur"

onChange, onInput, onSubmit :: EventProp ps st FormEvent
onChange = eventProp "onChange"
onInput = eventProp "onInput"
onSubmit = eventProp "onSubmit"

onClick, onContextMenu, onDoubleClick, onDrag, onDragEnd, onDragEnter, onDragExit, onDragLeave, onDragOver, onDragStart, onDrop, onMouseDown, onMouseEnter, onMouseLeave, onMouseMove, onMouseOut, onMouseOver, onMouseUp :: EventProp ps st MouseEvent
onClick = eventProp "onClick"
onContextMenu = eventProp "onContextMenu"
onDoubleClick = eventProp "onDoubleClick"
onDrag = eventProp "onDrag"
onDragEnd = eventProp "onDragEnd"
onDragEnter = eventProp "onDragEnter"
onDragExit = eventProp "onDragExit"
onDragLeave = eventProp "onDragLeave"
onDragOver = eventProp "onDragOver"
onDragStart = eventProp "onDragStart"
onDrop = eventProp "onDrop"
onMouseDown = eventProp "onMouseDown"
onMouseEnter = eventProp "onMouseEnter"
onMouseLeave = eventProp "onMouseLeave"
onMouseMove = eventProp "onMouseMove"
onMouseOut = eventProp "onMouseOut"
onMouseOver = eventProp "onMouseOver"
onMouseUp = eventProp "onMouseUp"

onSelect :: EventProp ps st SelectionEvent
onSelect = eventProp "onSelect"

onTouchCancel, onTouchEnd, onTouchMove, onTouchStart :: EventProp ps st TouchEvent
onTouchCancel = eventProp "onTouchCancel"
onTouchEnd = eventProp "onTouchEnd"
onTouchMove = eventProp "onTouchMove"
onTouchStart = eventProp "onTouchStart"

onScroll :: EventProp ps st UIEvent
onScroll = eventProp "onScroll"

onWheel :: EventProp ps st WheelEvent
onWheel = eventProp "onWheel"

onAbort, onCanPlay, onCanPlayThrough, onDurationChange, onEmptied, onEncrypted, onEnded, onError, onLoadedData, onLoadedMetadata, onLoadStart, onPause, onPlay, onPlaying, onProgress, onRateChange, onSeeked, onSeeking, onStalled, onSuspend, onTimeUpdate, onVolumeChange, onWaiting :: EventProp ps st MediaEvent
onAbort = eventProp "onAbort"
onCanPlay = eventProp "onCanPlay"
onCanPlayThrough = eventProp "onCanPlayThrough"
onDurationChange = eventProp "onDurationChange"
onEmptied = eventProp "onEmptied"
onEncrypted = eventProp "onEncrypted"
onEnded = eventProp "onEnded"
-- TODO different event data when used for image event instead of media event
onError = eventProp "onError"
onLoadedData = eventProp "onLoadedData"
onLoadedMetadata = eventProp "onLoadedMetadata"
onLoadStart = eventProp "onLoadStart"
onPause = eventProp "onPause"
onPlay = eventProp "onPlay"
onPlaying = eventProp "onPlaying"
onProgress = eventProp "onProgress"
onRateChange = eventProp "onRateChange"
onSeeked = eventProp "onSeeked"
onSeeking = eventProp "onSeeking"
onStalled = eventProp "onStalled"
onSuspend = eventProp "onSuspend"
onTimeUpdate = eventProp "onTimeUpdate"
onVolumeChange = eventProp "onVolumeChange"
onWaiting = eventProp "onWaiting"

onLoad :: EventProp ps st ImageEvent
onLoad = eventProp "onLoad"
-- onError

onAnimationStart, onAnimationEnd, onAnimationIteration :: EventProp ps st AnimationEvent
onAnimationStart = eventProp "onAnimationStart"
onAnimationEnd = eventProp "onAnimationEnd"
onAnimationIteration = eventProp "onAnimationIteration"

onTransitionEnd :: EventProp ps st TransitionEvent
onTransitionEnd = eventProp "onTransitionEnd"

foreign import javascript "$1['persist']()" js_persist :: Object.Object -> IO Object.Object
foreign import javascript "$1['bubbles']" js_bubbles :: Object.Object -> IO Bool
foreign import javascript "$1['cancelable']" js_cancelable :: Object.Object -> IO Bool
foreign import javascript "$1['nativeEvent']" js_getNative :: Object.Object -> IO JSVal
foreign import javascript "$1['target']" js_target :: Object.Object -> IO EventTarget
foreign import javascript "$1['currentTarget']" js_currentTarget :: Object.Object -> IO EventTarget
foreign import javascript "$1['defaultPrevented']" js_defaultPrevented :: Object.Object -> IO Bool
foreign import javascript "$1['eventPhase']" js_eventPhase :: Object.Object -> IO Int
foreign import javascript "$1['isTrusted']" js_isTrusted :: Object.Object -> IO Bool
foreign import javascript "$1['preventDefault']()" js_preventDefault :: Object.Object -> IO ()
foreign import javascript "$1['isDefaultPrevented']()" js_isDefaultPrevented :: Object.Object -> IO Bool
foreign import javascript "$1['stopPropagation']()" js_stopPropagation :: Object.Object -> IO ()
foreign import javascript "$1['isPropagationStopped']()" js_isPropagationStopped :: Object.Object -> IO Bool
foreign import javascript "$1['timeStamp']" js_timeStamp :: Object.Object -> IO Int
foreign import javascript "$1['type']" js_type :: Object.Object -> IO JSString

class SyntheticEvent e where
  type NativeEvent e
  eventVal :: e -> Object.Object
  eventVal = unsafeCoerce

  persist :: MonadIO m => e -> m e
  persist = fmap unsafeCoerce . liftIO . js_persist . eventVal

  bubbles :: MonadIO m => e -> m Bool
  bubbles = liftIO . js_bubbles . eventVal

  cancelable :: MonadIO m => e -> m Bool
  cancelable = liftIO . js_cancelable . eventVal

  currentTarget :: MonadIO m => e -> m EventTarget
  currentTarget = liftIO . js_currentTarget . eventVal

  defaultPrevented :: MonadIO m => e -> m Bool
  defaultPrevented = liftIO . js_defaultPrevented . eventVal

  eventPhase :: MonadIO m => e -> m Int
  eventPhase = liftIO . js_eventPhase . eventVal

  isTrusted :: MonadIO m => e -> m Bool
  isTrusted = liftIO . js_isTrusted . eventVal

  nativeEvent :: MonadIO m => e -> m (NativeEvent e)
  nativeEvent = fmap unsafeCoerce . liftIO . js_getNative . eventVal

  preventDefault :: MonadIO m => e -> m ()
  preventDefault = liftIO . js_preventDefault . eventVal

  isDefaultPrevented :: MonadIO m => e -> m Bool
  isDefaultPrevented = liftIO . js_isDefaultPrevented . eventVal

  stopPropagation :: MonadIO m => e -> m ()
  stopPropagation = liftIO . js_stopPropagation . eventVal

  isPropagationStopped :: MonadIO m => e -> m Bool
  isPropagationStopped = liftIO . js_isPropagationStopped . eventVal

  target :: MonadIO m => e -> m EventTarget
  target = liftIO . js_target . eventVal

  timeStamp :: MonadIO m => e -> m Int
  timeStamp = liftIO . js_timeStamp . eventVal

  eventType_ :: MonadIO m => e -> m JSString
  eventType_ = liftIO . js_type . eventVal

{-
class ErrorEvent e where {}
-}

newtype ClipboardEvent = ClipboardEvent Object.Object
-- clipboardData :: ClipboardEvent -> m DataTransfer

newtype CompositionEvent = CompositionEvent Object.Object
instance SyntheticEvent CompositionEvent where
  type NativeEvent CompositionEvent = JSVal
-- data_ :: e -> m JSString

newtype KeyboardEvent = KeyboardEvent Object.Object
instance SyntheticEvent KeyboardEvent where
  type NativeEvent KeyboardEvent = Keyboard.KeyboardEvent
{-
altKey :: e -> m Bool
charCode :: e -> m Int
ctrlKey :: e -> m Bool
getModifierState :: Int -> m Bool
key :: e -> m JSString
keyCode :: e -> m Int
locale :: e -> m JSString
location :: e -> m Int
metaKey :: e -> m Bool
repeat :: e -> m Bool
shiftKey :: e -> m Bool
which :: e -> m Int
-}

newtype FocusEvent = FocusEvent Object.Object
instance SyntheticEvent FocusEvent where
  type NativeEvent FocusEvent = Focus.FocusEvent
-- relatedTarget :: e -> m EventTarget

newtype FormEvent = FormEvent Object.Object
instance SyntheticEvent FormEvent where
  type NativeEvent FormEvent = JSVal

newtype MouseEvent = MouseEvent Object.Object
instance SyntheticEvent MouseEvent where
  type NativeEvent MouseEvent = Mouse.MouseEvent
{-
-- altKey
button :: e -> m Int
buttons :: e -> m Int
clientX :: e -> m Int
clientY :: e -> m Int
-- ctrlKey
-- getModifierState
-- metaKey
pageX :: e -> m Int
pageY :: e -> m Int
-- relatedTarget
screenX :: e -> m Int
screenY :: e -> m Int
-- shiftKey
-}

newtype SelectionEvent = SelectionEvent Object.Object
instance SyntheticEvent SelectionEvent where
  type NativeEvent SelectionEvent = JSVal

newtype TouchEvent = TouchEvent Object.Object
instance SyntheticEvent TouchEvent where
  type NativeEvent TouchEvent = Touch.TouchEvent
-- altKey
-- changedTouches :: e -> m (Maybe TouchList)


newtype UIEvent = UIEvent Object.Object
instance SyntheticEvent UIEvent where
  type NativeEvent UIEvent = JSVal

newtype WheelEvent = WheelEvent Object.Object
instance SyntheticEvent WheelEvent where
  type NativeEvent WheelEvent = JSVal

newtype MediaEvent = MediaEvent Object.Object
instance SyntheticEvent MediaEvent where
  type NativeEvent MediaEvent = JSVal

newtype ImageEvent = ImageEvent Object.Object
instance SyntheticEvent ImageEvent where
  type NativeEvent ImageEvent = JSVal

newtype AnimationEvent = AnimationEvent Object.Object
instance SyntheticEvent AnimationEvent where
  type NativeEvent AnimationEvent = Animation.AnimationEvent

newtype TransitionEvent = TransitionEvent Object.Object
instance SyntheticEvent TransitionEvent where
  type NativeEvent TransitionEvent = Transition.TransitionEvent

reactText :: JSString -> ReactElement
reactText = ReactElement . jsval

-- Type constrained in order to avoid ambiguous types in DOM structures
(//) :: (Maybe (Array ReactElement) -> ReactElement) -> [ReactElement] -> ReactElement
(//) lhs rhs = lhs $ Just $ array rhs

noProps :: [Prop]
noProps = []

newtype This ps st = This { fromThis :: JSVal }

foreign import javascript unsafe "$1['refs'][$2]" js_getRef :: This ps st -> JSString -> JSVal

-- | There's no checking that the returned ReactComponent actually uses the @ps@ and @st@ types
-- for its props and state, or that it conforms to the @t@ phantom type either, so a lot of safety
-- goes out the window here.
unsafeGetRef :: JSString -> ReactM ps st (Maybe (ReactInstance ps' st'))
unsafeGetRef str = do
  this <- ask
  let ref = js_getRef this str
  return $! if isTruthy ref
    then Just $ ReactInstance ref
    else Nothing

makeClass :: String -> ExpQ -> DecsQ
makeClass newName n = do
  ds <- funD newName'
        [ clause [] (normalB [e| createClass . fst . unsafePerformIO . buildSpec $ $(n)|]) []
        ]
  return $ [ds, noInlineFun]
  where
    newName' = mkName newName
    noInlineFun = PragmaD $ InlineP newName' NoInline FunLike AllPhases

foreign import javascript unsafe "($1 ? $1.props[$2] : null)" js_getProp :: This ps st -> JSString -> IO JSVal
foreign import javascript unsafe "$1[$2]" js_deref :: Props ps -> JSString -> IO JSVal

class GetProp a where
  getProp :: FromJSVal p => a -> PropName p -> Maybe p
  unsafeGetProp :: a -> PropName p -> Maybe p

instance GetProp (This ps st) where
  getProp this (PropName p) = unsafePerformIO $ do
    p <- js_getProp this p
    if isTruthy p
      then fromJSVal p
      else return Nothing

  unsafeGetProp this (PropName p) = unsafeCoerce $ unsafePerformIO $ do
    p <- js_getProp this p
    return $ if isTruthy p
      then Just $ unsafeCoerce p
      else Nothing

instance GetProp (Props a) where
  getProp allProps (PropName p) = unsafePerformIO $ do
    p <- js_deref allProps p
    if isTruthy p
      then fromJSVal p
      else return Nothing

  unsafeGetProp allProps (PropName p) = unsafeCoerce $ unsafePerformIO $ do
    p <- js_deref allProps p
    return $ if isTruthy p
      then Just $ unsafeCoerce p
      else Nothing

instance GetProp (Properties a) where
  getProp (Properties _ allProps) = getProp allProps
  unsafeGetProp (Properties _ allProps) = unsafeGetProp allProps


inheritProp :: This ps st -> PropName p -> Prop
inheritProp this (PropName p) = inheritProp' this p

inheritProp' :: This ps st -> JSString -> Prop
inheritProp' this str = unsafePerformIO $ do
  p <- js_getProp this str
  return $ Prop str p

class ToReactElement a where
  toReactElement :: a -> ReactElement

instance ToReactElement S.Text where
  toReactElement = text . textToJSString
  {-# INLINE toReactElement #-}

instance ToReactElement L.Text where
  toReactElement = text . lazyTextToJSString
  {-# INLINE toReactElement #-}

instance ToReactElement Bool where
  toReactElement True = text ("true" :: JSString)
  toReactElement False = text ("false" :: JSString)
  {-# INLINE toReactElement #-}

instance ToReactElement JSString where
  toReactElement = text

text' :: JSString -> ReactElement
text' = text
{-# INLINE text' #-}

class ReactText a where
  text :: a -> ReactElement

instance ReactText JSString where
  text = ReactElement . pToJSVal

instance ReactText Text where
  text = ReactElement . pToJSVal

instance ReactText String where
  text = ReactElement . pToJSVal

{-

A react element tree could have an intermediate representation that allows inline handlers -> pull them out from the IR when building the class

-}
