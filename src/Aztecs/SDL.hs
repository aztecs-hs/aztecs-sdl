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
    drawSurfaces,

    -- * Internal
    SurfaceTexture (..),
  )
where

import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import Linear
import qualified SDL

data Surface = Surface
  { sdlSurface :: SDL.Surface,
    surfaceBounds :: Maybe (SDL.Rectangle Int)
  }

instance Component Surface

data Transform = Transform
  { transformTranslation :: V2 Float,
    transformScale :: V2 Float,
    transformRotation :: Float
  }

instance Component Transform

newtype Size = Size {unSize :: V2 Float}

instance Component Size

drawSurfaces :: SDL.Renderer -> AccessT IO ()
drawSurfaces renderer = do
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

newtype SurfaceTexture = SurfaceTexture {unSurfaceTexture :: SDL.Texture}

instance Component SurfaceTexture
