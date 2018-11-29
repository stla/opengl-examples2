module CyclideColor
  (main)
  where
import           Control.Concurrent                (threadDelay)
import           Control.Monad                     (when)
import           Cyclide.DataColor
import qualified Data.ByteString                   as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           System.Directory                  (doesDirectoryExist)
import           System.IO.Unsafe
import           Text.Printf

n_u,n_v :: Int
n_u = 150
n_v = 150

cyclide :: Double -> Double -> Double -> [(NTriangle,NTriangle)]
cyclide = allTriangles (n_u,n_v)

data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextTriangles :: IORef [(NTriangle,NTriangle)]
    }

white,black :: Color4 GLfloat
white    = Color4    1    1    1 1
black    = Color4    0    0    0 1

display :: Context -> IORef GLdouble -> IORef GLfloat -> DisplayCallback
display context zoom alpha = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  triangles <- get (contextTriangles context)
  let (lowerTriangles,upperTriangles) = unzip triangles
  z <- get zoom
  alpha' <- get alpha
  loadIdentity
  (_, size) <- get viewport
  resize z size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  rotate alpha' $ Vector3 1 1 1
  renderPrimitive Triangles $ mapM_ drawTriangle lowerTriangles
  renderPrimitive Triangles $ mapM_ drawTriangle upperTriangles
  swapBuffers
  where
    drawTriangle ((v1,n1,col1),(v2,n2,col2),(v3,n3,col3)) = do
      materialDiffuse Front $= col1
      normal n1
      vertex v1
      materialDiffuse Front $= col2
      normal n2
      vertex v2
      materialDiffuse Front $= col3
      normal n3
      vertex v3

resize :: GLdouble -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (5+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef GLdouble -- zoom
         -> IORef Double -- a
         -> IORef Double -- c
         -> IORef Double -- d
         -> IORef Bool -- animation
         -> IORef Bool -- save
         -> IORef Int -- delay
         -> IORef [(NTriangle,NTriangle)]
         -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom a c d anim save delay triangles char _ = do
  case char of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+2)
    'm' -> zoom $~! (+0.1)
    'l' -> zoom $~! subtract 0.1
    'd' -> do
      a $~! (+0.1)
      a' <- get a
      c' <- get c
      d' <- get d
      writeIORef triangles (cyclide a' c' d')
    'c' -> do
      a $~! (subtract 0.1)
      a' <- get a
      c' <- get c
      d' <- get d
      writeIORef triangles (cyclide a' c' d')
    'f' -> do
      c $~! (+0.1)
      a' <- get a
      c' <- get c
      d' <- get d
      writeIORef triangles (cyclide a' c' d')
    'v' -> do
      c $~! (subtract 0.1)
      a' <- get a
      c' <- get c
      d' <- get d
      writeIORef triangles (cyclide a' c' d')
    'g' -> do
      d $~! (+0.1)
      a' <- get a
      c' <- get c
      d' <- get d
      writeIORef triangles (cyclide a' c' d')
    'b' -> do
      d $~! (subtract 0.1)
      a' <- get a
      c' <- get c
      d' <- get d
      writeIORef triangles (cyclide a' c' d')
    'a' -> anim $~! not
    's' -> save $~! not
    'o' -> delay $~! (+10000)
    'p' -> delay $~! (\x -> if x==0 then 0 else x-10000)
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing

ppmExists :: Bool
{-# NOINLINE ppmExists #-}
ppmExists = unsafePerformIO $ doesDirectoryExist "./ppm"

idle :: IORef Bool -> IORef Bool -> IORef Int -> IORef Int -> IORef GLfloat 
     -> IdleCallback
idle anim save delay snapshots alpha = do
  a <- get anim
  snapshot <- get snapshots
  s <- get save
  when a $ do
    d <- get delay
    when (s && ppmExists && snapshot < 360) $ do
      let ppm = printf "ppm/cyclide%04d.ppm" snapshot
      (>>=) capturePPM (B.writeFile ppm)
      print snapshot
      snapshots $~! (+1)
    alpha $~! (+1)
    _ <- threadDelay d
    postRedisplay Nothing
  return ()

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Ring cyclide"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= black
  materialSpecular Front $= white
  materialShininess Front $= 30
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  alpha <- newIORef 0.0
  let a = 0.94
      c = 0.34
      d = 0.56
      triangles = cyclide a c d
  a' <- newIORef a
  c' <- newIORef c
  d' <- newIORef d
  triangles' <- newIORef triangles
  anim <- newIORef False
  save <- newIORef False
  delay <- newIORef 0
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextTriangles = triangles'}
                             zoom alpha
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= 
    Just (keyboard rot1 rot2 rot3 zoom a' c' d' anim save delay triangles')
  snapshot <- newIORef 0
  idleCallback $= Just (idle anim save delay snapshot alpha)
  putStrLn "*** Ring cyclide ***\n\
        \    To quit, press q.\n\
        \    Scene rotation: e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease parameters: d, c, f, v, g, b\n\
        \    Animation: a\n\
        \    Animation speed: o, p\n\
        \    Save animation: s\n\
        \"
  mainLoop