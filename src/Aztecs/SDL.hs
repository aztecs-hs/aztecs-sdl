-- |
-- Module      : Aztecs.SDL
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.SDL
  ( -- * Components
    Surface (..),
    Transform (..),
    Size (..),

    -- * Systems
    buildTextures,
    drawTextures,

    -- * Internal
    SurfaceTexture (..),
    drawTexture,
  )
where

import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import Control.Monad.IO.Class
import Linear
import Linear.Affine
import qualified SDL

data Surface = Surface
  { sdlSurface :: SDL.Surface,
    surfaceBounds :: Maybe (SDL.Rectangle Int)
  }

instance Component Surface

data Transform = Transform
  { transformTranslation :: V2 Int,
    transformScale :: V2 Float,
    transformRotation :: Float
  }

instance Component Transform

newtype Size = Size {unSize :: V2 Float}

instance Component Size

buildTextures :: SDL.Renderer -> AccessT IO ()
buildTextures renderer = do
  surfaces <- system $ readQuery $ (,,,) <$> entity <*> fetch <*> fetch <*> fetchMaybe
  mapM_
    ( \(eId, surface, transform, maybeTexture) -> do
        sdlTexture <- SDL.createTextureFromSurface renderer $ sdlSurface surface
        textureDesc <- SDL.queryTexture sdlTexture
        case maybeTexture of
          Just (SurfaceTexture lastTexture) -> SDL.destroyTexture lastTexture
          Nothing -> return ()
        A.insert eId . bundle $ SurfaceTexture sdlTexture
        A.insert
          eId
          ( bundle . Size $
              transformScale transform
                * maybe
                  (fromIntegral <$> V2 (SDL.textureWidth textureDesc) (SDL.textureHeight textureDesc))
                  (\(SDL.Rectangle _ s) -> fmap fromIntegral s)
                  (surfaceBounds surface)
          )
    )
    surfaces

drawTextures :: SDL.Renderer -> AccessT IO ()
drawTextures renderer = do
  surfaces <- system $ readQuery $ (,,) <$> fetch <*> fetch <*> fetch
  mapM_
    (\(surface, transform, texture) -> liftIO $ drawTexture surface texture transform renderer)
    surfaces

drawTexture :: Surface -> SurfaceTexture -> Transform -> SDL.Renderer -> IO ()
drawTexture surface texture transform renderer = do
  textureDesc <- SDL.queryTexture $ unSurfaceTexture texture
  SDL.copyEx
    renderer
    (unSurfaceTexture texture)
    (fmap fromIntegral <$> surfaceBounds surface)
    ( Just
        ( SDL.Rectangle
            (fmap fromIntegral . P $ transformTranslation transform)
            ( maybe
                (fromIntegral <$> V2 (SDL.textureWidth textureDesc) (SDL.textureHeight textureDesc))
                (\(SDL.Rectangle _ s) -> fmap fromIntegral s)
                (surfaceBounds surface)
            )
        )
    )
    (realToFrac $ transformRotation transform)
    Nothing
    (V2 False False)

newtype SurfaceTexture = SurfaceTexture {unSurfaceTexture :: SDL.Texture}

instance Component SurfaceTexture
