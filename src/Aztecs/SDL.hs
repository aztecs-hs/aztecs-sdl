{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Aztecs.SDL
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.SDL
  ( -- * Window components
    WindowRenderer (..),

    -- * Camera components
    Camera (..),
    CameraTarget (..),

    -- * Surface components
    Surface (..),
    SurfaceTarget (..),
    SurfaceTexture (..),

    -- * Input

    -- ** Keyboard input
    fromSDLKeycode,
    toSDLKeycode,

    -- ** Mouse input
    mouseButtonFromSDL,
    mouseButtonToSDL,
    motionFromSDL,
    motionToSDL,

    -- * Systems
    setup,
    update,
    draw,

    -- ** Primitive systems
    addWindows,
    buildTextures,
    addSurfaceTargets,
    handleInput,
    updateTime,
  )
where

import Aztecs
import Aztecs.Camera
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Aztecs.Input
import Control.Arrow
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as T
import GHC.Generics
import SDL hiding (InputMotion (..), MouseButton (..), Surface, Texture, Window, windowTitle)
import qualified SDL

-- | Window renderer component.
--
-- @since 0.5
data WindowRenderer = WindowRenderer
  { -- | SDL window.
    --
    -- @since 0.5
    windowRendererRaw :: !SDL.Window,
    -- | SDL renderer.
    --
    -- @since 0.5
    windowRenderer :: !Renderer
  }
  deriving (Show, Generic)

-- | @since 0.5
instance Component WindowRenderer

-- | @since 0.5
instance NFData WindowRenderer where
  rnf = rwhnf

-- | Setup SDL
--
-- @since 0.5
setup :: (MonadAccess b m, MonadIO m) => m ()
setup = do
  liftIO initializeAll
  A.spawn_ . bundle $ Time 0
  A.spawn_ $ bundle keyboardInput
  A.spawn_ $ bundle mouseInput

-- | Update SDL windows
--
-- @since 0.5
update ::
  ( ArrowQueryReader qr,
    ArrowDynamicQueryReader qr,
    ArrowQuery m q,
    MonadSystem q s,
    MonadReaderSystem qr s,
    MonadIO s,
    MonadAccess b s,
    MonadIO m
  ) =>
  s ()
update = do
  updateTime
  join addWindows
  join addCameraTargets
  join addSurfaceTargets
  join buildTextures
  handleInput

-- | Setup new windows.
--
-- @since 0.5
addWindows ::
  ( ArrowQueryReader q,
    ArrowDynamicQueryReader q,
    MonadReaderSystem q s,
    MonadIO s,
    MonadAccess b ma
  ) =>
  s (ma ())
addWindows = do
  newWindows <- S.filter () (Q.entity &&& Q.fetch @_ @Window) (without @WindowRenderer)
  newWindows' <- liftIO $ mapM createWindowRenderer newWindows
  return $ mapM_ insertWindowRenderer newWindows'
  where
    createWindowRenderer (eId, window) = do
      sdlWindow <- createWindow (T.pack $ windowTitle window) defaultWindow
      renderer <- createRenderer sdlWindow (-1) defaultRenderer
      return (eId, sdlWindow, renderer)
    insertWindowRenderer (eId, window, renderer) = A.insert eId . bundle $ WindowRenderer window renderer

-- | Surface texture component.
--
-- @since 0.5
newtype SurfaceTexture = SurfaceTexture
  { -- | SDL texture.
    --
    -- @since 0.5
    unSurfaceTexture :: SDL.Texture
  }
  deriving (Generic)

-- | @since 0.5
instance Component SurfaceTexture

-- | @since 0.5
instance NFData SurfaceTexture where
  rnf = rwhnf

-- | Query all window textures.
--
-- @since 0.5
allWindowTextures ::
  (ArrowQueryReader q, ArrowDynamicQueryReader q, MonadReaderSystem q s) =>
  s [(WindowRenderer, [(EntityID, Surface, Transform2D, Maybe SurfaceTexture)])]
allWindowTextures =
  map (second (concatMap snd))
    <$> allWindowDraws
      (arr (const ()))
      ( proc () -> do
          e <- Q.entity -< ()
          surface <- Q.fetch -< ()
          transform <- Q.fetch -< ()
          texture <- Q.fetchMaybe -< ()
          returnA -< (e, surface, transform, texture)
      )

-- | Build textures from surfaces in preparation for `drawTextures`.
--
-- @since 0.5
buildTextures ::
  ( ArrowQueryReader q,
    ArrowDynamicQueryReader q,
    MonadReaderSystem q s,
    MonadAccess b m,
    MonadIO m
  ) =>
  s (m ())
buildTextures = do
  windowTextures <- allWindowTextures
  let go =
        mapM_
          ( \(window, cameraDraws) -> do
              let renderer = windowRenderer window
              mapM_
                ( \(eId, surface, transform, maybeTexture) -> do
                    sdlTexture <- SDL.createTextureFromSurface renderer $ sdlSurface surface
                    textureDesc <- queryTexture sdlTexture
                    case maybeTexture of
                      Just (SurfaceTexture lastTexture) -> destroyTexture lastTexture
                      Nothing -> return ()
                    A.insert eId . bundle $ SurfaceTexture sdlTexture
                    A.insert
                      eId
                      ( bundle . Size $
                          transformScale transform
                            * maybe (fromIntegral <$> V2 (textureWidth textureDesc) (textureHeight textureDesc)) (\(Rectangle _ s) -> fmap fromIntegral s) (surfaceBounds surface)
                      )
                )
                cameraDraws
          )
          windowTextures
  return go

-- | Draw all textures to their targetted windows.
--
-- @since 0.5
draw :: (ArrowQueryReader q, ArrowDynamicQueryReader q, MonadReaderSystem q s, MonadIO s) => s ()
draw = do
  cameraSurfaces <- allCameraSurfaces
  mapM_
    ( \(window, cameraDraws) -> do
        let renderer = windowRenderer window
        clear renderer
        mapM_
          ( \((camera, cameraTransform), cameraDraws') -> do
              rendererScale renderer $= fmap realToFrac (cameraScale camera)
              rendererViewport renderer
                $= Just
                  ( Rectangle
                      (P (fromIntegral <$> transformTranslation cameraTransform))
                      (fromIntegral <$> cameraViewport camera)
                  )
              mapM_
                ( \(surface, transform, texture) -> do
                    textureDesc <- queryTexture $ unSurfaceTexture texture
                    copyEx
                      renderer
                      (unSurfaceTexture texture)
                      (fmap fromIntegral <$> surfaceBounds surface)
                      ( Just
                          ( Rectangle
                              (fmap fromIntegral . P $ transformTranslation transform)
                              ( maybe (fromIntegral <$> V2 (textureWidth textureDesc) (textureHeight textureDesc)) (\(Rectangle _ s) -> fmap fromIntegral s) (surfaceBounds surface)
                              )
                          )
                      )
                      (realToFrac $ transformRotation transform)
                      Nothing
                      (V2 False False)
                )
                cameraDraws'
          )
          cameraDraws
        rendererDrawColor renderer $= V4 0 0 0 255
        present renderer
    )
    cameraSurfaces

-- | Query all cameras and their target surfaces.
--
-- @since 0.5
allCameraSurfaces ::
  (ArrowQueryReader q, ArrowDynamicQueryReader q, MonadReaderSystem q s) =>
  s [(WindowRenderer, [((Camera, Transform2D), [(Surface, Transform2D, SurfaceTexture)])])]
allCameraSurfaces =
  allWindowDraws
    (Q.fetch &&& Q.fetch)
    ( proc () -> do
        surface <- Q.fetch -< ()
        transform <- Q.fetch -< ()
        texture <- Q.fetch -< ()
        returnA -< (surface, transform, texture)
    )

-- | Query all windows and drawable surfaces.
--
-- @since 0.5
allWindowDraws ::
  (ArrowQueryReader q, ArrowDynamicQueryReader q, MonadReaderSystem q s) =>
  q () a ->
  q () b ->
  s [(WindowRenderer, [(a, [b])])]
allWindowDraws qA qB = do
  cameras <-
    S.all
      ()
      ( proc () -> do
          eId <- Q.entity -< ()
          cameraTarget <- Q.fetch @_ @CameraTarget -< ()
          a <- qA -< ()
          returnA -< (eId, cameraTarget, a)
      )

  windows <- S.all () (Q.entity &&& Q.fetch @_ @WindowRenderer)
  draws <-
    S.all
      ()
      ( proc () -> do
          t <- Q.fetch @_ @SurfaceTarget -< ()
          a <- qB -< ()
          returnA -< (t, a)
      )
  let cameraDraws =
        map
          ( \(eId, cameraTarget, b) ->
              ( cameraTarget,
                b,
                mapMaybe
                  ( \(surfaceTarget, a) ->
                      if drawTargetCamera surfaceTarget == eId
                        then Just a
                        else Nothing
                  )
                  draws
              )
          )
          cameras
      windowDraws =
        map
          ( \(eId, window) ->
              ( window,
                map (\(_, a, b) -> (a, b)) $
                  filter (\(cameraTarget, _, _) -> cameraTargetWindow cameraTarget == eId) cameraDraws
              )
          )
          windows
  return windowDraws

-- | Surface target component.
-- This component can be used to specify which `Camera` to draw a `Surface` to.
--
-- @since 0.5
newtype SurfaceTarget = SurfaceTarget {drawTargetCamera :: EntityID}
  deriving (Eq, Show, Generic, NFData)

instance Component SurfaceTarget

-- | Surface component.
-- This component can be used to draw to a window.
--
-- @since 0.5
data Surface = Surface
  { sdlSurface :: !SDL.Surface,
    surfaceBounds :: !(Maybe (Rectangle Int))
  }

-- | @since 0.5
instance Component Surface

-- | @since 0.5
instance NFData Surface where
  rnf = rwhnf

-- | Add `SurfaceTarget` components to entities with a new `Surface` component.
--
-- @since 0.5
addSurfaceTargets ::
  ( ArrowQueryReader q,
    ArrowDynamicQueryReader q,
    MonadReaderSystem q s,
    MonadAccess b m
  ) =>
  s (m ())
addSurfaceTargets = do
  cameras <- S.all () (Q.entity &&& Q.fetch @_ @Camera)
  newDraws <- S.filter () (Q.entity &&& Q.fetch @_ @Surface) (without @SurfaceTarget)
  let go = case cameras of
        (cameraEId, _) : _ -> mapM_ (\(eId, _) -> A.insert eId . bundle $ SurfaceTarget cameraEId) newDraws
        _ -> return ()
  return go

-- | Update the current `Time`.
--
-- @since 0.5
updateTime ::
  ( ArrowQuery m q,
    MonadSystem q s,
    MonadIO m
  ) =>
  s ()
updateTime = void . S.mapSingle () $ Q.adjustM (\_ _ -> Time <$> SDL.ticks)

-- | Keyboard input system.
--
-- @since 0.5
handleInput ::
  ( ArrowQueryReader qr,
    MonadReaderSystem qr s,
    ArrowQuery m q,
    MonadSystem q s,
    MonadIO s
  ) =>
  s ()
handleInput = liftIO pollEvents >>= handleInput'

-- | Keyboard input system.
--
-- @since 0.5
handleInput' ::
  (ArrowQueryReader qr, MonadReaderSystem qr s, ArrowQuery m q, MonadSystem q s) =>
  [Event] ->
  s ()
handleInput' events = do
  let go (kbAcc, mouseAcc) event = case eventPayload event of
        KeyboardEvent keyboardEvent ->
          ( case fromSDLKeycode $ keysymKeycode $ keyboardEventKeysym keyboardEvent of
              Just key -> handleKeyboardEvent key (motionFromSDL $ keyboardEventKeyMotion keyboardEvent) . kbAcc
              Nothing -> kbAcc,
            mouseAcc
          )
        MouseMotionEvent mouseMotionEvent ->
          ( kbAcc,
            \m ->
              (mouseAcc m)
                { mousePosition = fromIntegral <$> mouseMotionEventPos mouseMotionEvent,
                  mouseOffset = fromIntegral <$> mouseMotionEventRelMotion mouseMotionEvent,
                  mouseButtons = mouseButtons m
                }
          )
        MouseButtonEvent mouseButtonEvent ->
          ( kbAcc,
            \m ->
              (mouseAcc m)
                { mousePosition = fromIntegral <$> mouseButtonEventPos mouseButtonEvent,
                  mouseButtons =
                    Map.insert
                      (mouseButtonFromSDL $ mouseButtonEventButton mouseButtonEvent)
                      (motionFromSDL $ mouseButtonEventMotion mouseButtonEvent)
                      (mouseButtons m)
                }
          )
        _ -> (kbAcc, mouseAcc)
      (updateKb, updateMouse) = foldl' go (id, id) events
  _ <- S.map () . Q.adjust $ const updateKb
  _ <- S.map () . Q.adjust $ const updateMouse
  return ()

-- | Convert an SDL mouse button to `MouseButton`.
--
-- @since 0.5
mouseButtonFromSDL :: SDL.MouseButton -> MouseButton
mouseButtonFromSDL b = case b of
  SDL.ButtonLeft -> ButtonLeft
  SDL.ButtonMiddle -> ButtonMiddle
  SDL.ButtonRight -> ButtonRight
  SDL.ButtonX1 -> ButtonX1
  SDL.ButtonX2 -> ButtonX2
  SDL.ButtonExtra i -> ButtonExtra i

-- | Convert a `MouseButton` to an SDL mouse button.
--
-- @since 0.5
mouseButtonToSDL :: MouseButton -> SDL.MouseButton
mouseButtonToSDL b = case b of
  ButtonLeft -> SDL.ButtonLeft
  ButtonMiddle -> SDL.ButtonMiddle
  ButtonRight -> SDL.ButtonRight
  ButtonX1 -> SDL.ButtonX1
  ButtonX2 -> SDL.ButtonX2
  ButtonExtra i -> SDL.ButtonExtra i

-- | Convert an SDL input motion to `InputMotion`.
--
-- @since 0.5
motionFromSDL :: SDL.InputMotion -> InputMotion
motionFromSDL m = case m of
  SDL.Pressed -> Pressed
  SDL.Released -> Released

-- | Convert an `InputMotion` to an SDL input motion.
--
-- @since 0.5
motionToSDL :: InputMotion -> SDL.InputMotion
motionToSDL m = case m of
  Pressed -> SDL.Pressed
  Released -> SDL.Released

-- | Convert an SDL key code to `Key`.
--
-- @since 0.5
toSDLKeycode :: Key -> SDL.Keycode
toSDLKeycode key = case key of
  KeyA -> SDL.KeycodeA
  KeyB -> SDL.KeycodeB
  KeyC -> SDL.KeycodeC
  KeyD -> SDL.KeycodeD
  KeyE -> SDL.KeycodeE
  KeyF -> SDL.KeycodeF
  KeyG -> SDL.KeycodeG
  KeyH -> SDL.KeycodeH
  KeyI -> SDL.KeycodeI
  KeyJ -> SDL.KeycodeJ
  KeyK -> SDL.KeycodeK
  KeyL -> SDL.KeycodeL
  KeyM -> SDL.KeycodeM
  KeyN -> SDL.KeycodeN
  KeyO -> SDL.KeycodeO
  KeyP -> SDL.KeycodeP
  KeyQ -> SDL.KeycodeQ
  KeyR -> SDL.KeycodeR
  KeyS -> SDL.KeycodeS
  KeyT -> SDL.KeycodeT
  KeyU -> SDL.KeycodeU
  KeyV -> SDL.KeycodeV
  KeyW -> SDL.KeycodeW
  KeyX -> SDL.KeycodeX
  KeyY -> SDL.KeycodeY
  KeyZ -> SDL.KeycodeZ
  Key0 -> SDL.Keycode0
  Key1 -> SDL.Keycode1
  Key2 -> SDL.Keycode2
  Key3 -> SDL.Keycode3
  Key4 -> SDL.Keycode4
  Key5 -> SDL.Keycode5
  Key6 -> SDL.Keycode6
  Key7 -> SDL.Keycode7
  Key8 -> SDL.Keycode8
  Key9 -> SDL.Keycode9
  KeyF1 -> SDL.KeycodeF1
  KeyF2 -> SDL.KeycodeF2
  KeyF3 -> SDL.KeycodeF3
  KeyF4 -> SDL.KeycodeF4
  KeyF5 -> SDL.KeycodeF5
  KeyF6 -> SDL.KeycodeF6
  KeyF7 -> SDL.KeycodeF7
  KeyF8 -> SDL.KeycodeF8
  KeyF9 -> SDL.KeycodeF9
  KeyF10 -> SDL.KeycodeF10
  KeyF11 -> SDL.KeycodeF11
  KeyF12 -> SDL.KeycodeF12
  KeyEscape -> SDL.KeycodeEscape
  KeyEnter -> SDL.KeycodeReturn
  KeySpace -> SDL.KeycodeSpace
  KeyBackspace -> SDL.KeycodeBackspace
  KeyTab -> SDL.KeycodeTab
  KeyCapsLock -> SDL.KeycodeCapsLock
  KeyShift -> SDL.KeycodeLShift
  KeyCtrl -> SDL.KeycodeLCtrl
  KeyAlt -> SDL.KeycodeLAlt
  KeyLeft -> SDL.KeycodeLeft
  KeyRight -> SDL.KeycodeRight
  KeyUp -> SDL.KeycodeUp
  KeyDown -> SDL.KeycodeDown
  KeyHome -> SDL.KeycodeHome
  KeyEnd -> SDL.KeycodeEnd
  KeyPageUp -> SDL.KeycodePageUp
  KeyPageDown -> SDL.KeycodePageDown
  KeyInsert -> SDL.KeycodeInsert
  KeyDelete -> SDL.KeycodeDelete
  KeyMinus -> SDL.KeycodeMinus
  KeyEquals -> SDL.KeycodeEquals
  KeyBracketLeft -> SDL.KeycodeLeftBracket
  KeyBracketRight -> SDL.KeycodeRightBracket
  KeyBackslash -> SDL.KeycodeBackslash
  KeySemicolon -> SDL.KeycodeSemicolon
  KeyComma -> SDL.KeycodeComma
  KeyPeriod -> SDL.KeycodePeriod
  KeySlash -> SDL.KeycodeSlash
  KeyNumLock -> SDL.KeycodeNumLockClear
  KeyNumpad0 -> SDL.KeycodeKP0
  KeyNumpad1 -> SDL.KeycodeKP1
  KeyNumpad2 -> SDL.KeycodeKP2
  KeyNumpad3 -> SDL.KeycodeKP3
  KeyNumpad4 -> SDL.KeycodeKP4
  KeyNumpad5 -> SDL.KeycodeKP5
  KeyNumpad6 -> SDL.KeycodeKP6
  KeyNumpad7 -> SDL.KeycodeKP7
  KeyNumpad8 -> SDL.KeycodeKP8
  KeyNumpad9 -> SDL.KeycodeKP9
  KeyNumpadDivide -> SDL.KeycodeKPDivide
  KeyNumpadMultiply -> SDL.KeycodeKPMultiply
  KeyNumpadMinus -> SDL.KeycodeKPMinus
  KeyNumpadPlus -> SDL.KeycodeKPPlus
  KeyNumpadEnter -> SDL.KeycodeKPEnter
  KeyNumpadPeriod -> SDL.KeycodeKPPeriod
  KeySuper -> SDL.KeycodeLGUI
  KeyMenu -> SDL.KeycodeMenu

-- | Convert a `Key` to an SDL key code.
--
-- @since 0.5
fromSDLKeycode :: SDL.Keycode -> Maybe Key
fromSDLKeycode keycode = case keycode of
  SDL.KeycodeA -> Just KeyA
  SDL.KeycodeB -> Just KeyB
  SDL.KeycodeC -> Just KeyC
  SDL.KeycodeD -> Just KeyD
  SDL.KeycodeE -> Just KeyE
  SDL.KeycodeF -> Just KeyF
  SDL.KeycodeG -> Just KeyG
  SDL.KeycodeH -> Just KeyH
  SDL.KeycodeI -> Just KeyI
  SDL.KeycodeJ -> Just KeyJ
  SDL.KeycodeK -> Just KeyK
  SDL.KeycodeL -> Just KeyL
  SDL.KeycodeM -> Just KeyM
  SDL.KeycodeN -> Just KeyN
  SDL.KeycodeO -> Just KeyO
  SDL.KeycodeP -> Just KeyP
  SDL.KeycodeQ -> Just KeyQ
  SDL.KeycodeR -> Just KeyR
  SDL.KeycodeS -> Just KeyS
  SDL.KeycodeT -> Just KeyT
  SDL.KeycodeU -> Just KeyU
  SDL.KeycodeV -> Just KeyV
  SDL.KeycodeW -> Just KeyW
  SDL.KeycodeX -> Just KeyX
  SDL.KeycodeY -> Just KeyY
  SDL.KeycodeZ -> Just KeyZ
  SDL.Keycode0 -> Just Key0
  SDL.Keycode1 -> Just Key1
  SDL.Keycode2 -> Just Key2
  SDL.Keycode3 -> Just Key3
  SDL.Keycode4 -> Just Key4
  SDL.Keycode5 -> Just Key5
  SDL.Keycode6 -> Just Key6
  SDL.Keycode7 -> Just Key7
  SDL.Keycode8 -> Just Key8
  SDL.Keycode9 -> Just Key9
  SDL.KeycodeF1 -> Just KeyF1
  SDL.KeycodeF2 -> Just KeyF2
  SDL.KeycodeF3 -> Just KeyF3
  SDL.KeycodeF4 -> Just KeyF4
  SDL.KeycodeF5 -> Just KeyF5
  SDL.KeycodeF6 -> Just KeyF6
  SDL.KeycodeF7 -> Just KeyF7
  SDL.KeycodeF8 -> Just KeyF8
  SDL.KeycodeF9 -> Just KeyF9
  SDL.KeycodeF10 -> Just KeyF10
  SDL.KeycodeF11 -> Just KeyF11
  SDL.KeycodeF12 -> Just KeyF12
  SDL.KeycodeEscape -> Just KeyEscape
  SDL.KeycodeReturn -> Just KeyEnter
  SDL.KeycodeSpace -> Just KeySpace
  SDL.KeycodeBackspace -> Just KeyBackspace
  SDL.KeycodeTab -> Just KeyTab
  SDL.KeycodeCapsLock -> Just KeyCapsLock
  SDL.KeycodeLShift -> Just KeyShift
  SDL.KeycodeRShift -> Just KeyShift
  SDL.KeycodeLCtrl -> Just KeyCtrl
  SDL.KeycodeRCtrl -> Just KeyCtrl
  SDL.KeycodeLAlt -> Just KeyAlt
  SDL.KeycodeRAlt -> Just KeyAlt
  SDL.KeycodeLeft -> Just KeyLeft
  SDL.KeycodeRight -> Just KeyRight
  SDL.KeycodeUp -> Just KeyUp
  SDL.KeycodeDown -> Just KeyDown
  SDL.KeycodeHome -> Just KeyHome
  SDL.KeycodeEnd -> Just KeyEnd
  SDL.KeycodePageUp -> Just KeyPageUp
  SDL.KeycodePageDown -> Just KeyPageDown
  SDL.KeycodeInsert -> Just KeyInsert
  SDL.KeycodeDelete -> Just KeyDelete
  SDL.KeycodeMinus -> Just KeyMinus
  SDL.KeycodeEquals -> Just KeyEquals
  SDL.KeycodeLeftBracket -> Just KeyBracketLeft
  SDL.KeycodeRightBracket -> Just KeyBracketRight
  SDL.KeycodeBackslash -> Just KeyBackslash
  SDL.KeycodeSemicolon -> Just KeySemicolon
  SDL.KeycodeComma -> Just KeyComma
  SDL.KeycodePeriod -> Just KeyPeriod
  SDL.KeycodeSlash -> Just KeySlash
  SDL.KeycodeNumLockClear -> Just KeyNumLock
  SDL.KeycodeKP0 -> Just KeyNumpad0
  SDL.KeycodeKP1 -> Just KeyNumpad1
  SDL.KeycodeKP2 -> Just KeyNumpad2
  SDL.KeycodeKP3 -> Just KeyNumpad3
  SDL.KeycodeKP4 -> Just KeyNumpad4
  SDL.KeycodeKP5 -> Just KeyNumpad5
  SDL.KeycodeKP6 -> Just KeyNumpad6
  SDL.KeycodeKP7 -> Just KeyNumpad7
  SDL.KeycodeKP8 -> Just KeyNumpad8
  SDL.KeycodeKP9 -> Just KeyNumpad9
  SDL.KeycodeKPDivide -> Just KeyNumpadDivide
  SDL.KeycodeKPMultiply -> Just KeyNumpadMultiply
  SDL.KeycodeKPMinus -> Just KeyNumpadMinus
  SDL.KeycodeKPPlus -> Just KeyNumpadPlus
  SDL.KeycodeKPEnter -> Just KeyNumpadEnter
  SDL.KeycodeKPPeriod -> Just KeyNumpadPeriod
  SDL.KeycodeLGUI -> Just KeySuper
  SDL.KeycodeRGUI -> Just KeySuper
  SDL.KeycodeMenu -> Just KeyMenu
  _ -> Nothing
