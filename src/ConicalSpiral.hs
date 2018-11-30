module ConicalSpiral where
import           ConicalSpiral.Data
import           Control.Concurrent                (threadDelay)
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           System.Directory                  (doesDirectoryExist)
import           System.IO.Unsafe
import           Text.Printf

nu,nv :: Int
nu = 100
nv = 100

type NPoint = (Vertex3 GLfloat, Normal3 GLfloat, Color4 GLfloat)
type NTriangle = (NPoint, NPoint, NPoint)
type PairedTriangles = (NTriangle,NTriangle)

white,black :: Color4 GLfloat
white      = Color4    1    1    1    1
black      = Color4    0    0    0    1

data Context = Context
    {
      contextRot1      :: IORef GLfloat
    , contextRot2      :: IORef GLfloat
    , contextRot3      :: IORef GLfloat
    , contextZoom      :: IORef GLdouble
    , contextTriangles :: IORef [PairedTriangles]
    }

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  zoom <- get (contextZoom context)
  triangles <- get (contextTriangles context)
  let (lowerTriangles, upperTriangles) = unzip triangles
  loadIdentity
  (_, size) <- get viewport
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
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

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-20+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat
         -> IORef GLfloat -> IORef GLfloat 
         -> IORef GLfloat -> IORef GLfloat
         -> IORef GLdouble -> IORef [PairedTriangles]
         -> IORef Bool -> IORef Int -> IORef Bool -> KeyboardCallback
keyboard rot1 rot2 rot3 alpha beta gamma n zoom triangles anim delay save c _ = do
  case c of
    'c' -> do 
      alpha $~! subtract 0.1
      alpha' <- get alpha
      beta' <- get beta
      gamma' <- get gamma
      n' <- get n 
      writeIORef triangles (allTriangles' alpha' beta' gamma' n' (nu,nv))
    'd' -> do
      alpha $~! (+ 0.1)
      alpha' <- get alpha
      beta' <- get beta
      gamma' <- get gamma
      n' <- get n 
      writeIORef triangles (allTriangles' alpha' beta' gamma' n' (nu,nv))
    'v' -> do
      beta $~! subtract 0.1
      alpha' <- get alpha
      beta' <- get beta
      gamma' <- get gamma
      n' <- get n 
      writeIORef triangles (allTriangles' alpha' beta' gamma' n' (nu,nv))
    'f' -> do
      beta $~! (+ 0.1)
      alpha' <- get alpha
      beta' <- get beta
      gamma' <- get gamma
      n' <- get n 
      writeIORef triangles (allTriangles' alpha' beta' gamma' n' (nu,nv))
    'b' -> do
      gamma $~! subtract 0.1
      alpha' <- get alpha
      beta' <- get beta
      gamma' <- get gamma
      n' <- get n 
      writeIORef triangles (allTriangles' alpha' beta' gamma' n' (nu,nv))
    'g' -> do
      gamma $~! (+ 0.1)
      alpha' <- get alpha
      beta' <- get beta
      gamma' <- get gamma
      n' <- get n 
      writeIORef triangles (allTriangles' alpha' beta' gamma' n' (nu,nv))
    'n' -> do
      n $~! subtract 0.1
      alpha' <- get alpha
      beta' <- get beta
      gamma' <- get gamma
      n' <- get n 
      writeIORef triangles (allTriangles' alpha' beta' gamma' n' (nu,nv))
    'h' -> do
      n $~! (+ 0.1)
      alpha' <- get alpha
      beta' <- get beta
      gamma' <- get gamma
      n' <- get n 
      writeIORef triangles (allTriangles' alpha' beta' gamma' n' (nu,nv))
    'e' -> rot1 $~! subtract 1
    'r' -> rot1 $~! (+1)
    't' -> rot2 $~! subtract 1
    'y' -> rot2 $~! (+1)
    'u' -> rot3 $~! subtract 1
    'i' -> rot3 $~! (+1)
    'm' -> zoom $~! (+1)
    'l' -> zoom $~! subtract 1
    'a' -> anim $~! not
    'o' -> delay $~! (+10000)
    'p' -> delay $~! (\d -> if d==0 then 0 else d-10000)
    's' -> save $~! not
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing

ppmExists :: Bool
{-# NOINLINE ppmExists #-}
ppmExists = unsafePerformIO $ doesDirectoryExist "./ppm"

idle :: IORef Bool -> IORef Int -> IORef Bool -> IORef Int -> IORef GLfloat
     -> IdleCallback
idle anim delay save snapshots rot2 = do
    a <- get anim
    snapshot <- get snapshots
    s <- get save
    when a $ do
      d <- get delay
      when (s && ppmExists && snapshot < 360) $ do
        let ppm = printf "ppm/conicalspiral%04d.ppm" snapshot
        (>>=) capturePPM (B.writeFile ppm)
        print snapshot
        snapshots $~! (+1)
      rot2 $~! (+1)
      _ <- threadDelay d
      postRedisplay Nothing
    return ()

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Conical Spiral"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= black
  materialShininess FrontAndBack $= 50
  materialSpecular Front $= white
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  let alpha = 1.0
      beta = 8.0
      gamma = 0.2
      n = 5.0
      spiral = allTriangles' alpha beta gamma n (nu,nv)
  alpha' <- newIORef alpha
  beta' <- newIORef beta
  gamma' <- newIORef gamma
  n' <- newIORef n
  spiral' <- newIORef spiral
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  anim <- newIORef False
  delay <- newIORef 0
  save <- newIORef False
  snapshots <- newIORef 0
  displayCallback $= display Context {
    contextRot1 = rot1,
    contextRot2 = rot2,
    contextRot3 = rot3,
    contextZoom = zoom,
    contextTriangles = spiral'
  }   
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= 
    Just (keyboard rot1 rot2 rot3 alpha' beta' gamma' n' zoom spiral' anim delay save)
  idleCallback $= Just (idle anim delay save snapshots rot2)
  putStrLn "*** Conical Spiral ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Parameters: d, f, g, h, c, b, v, n \n\
        \    Animation: a\n\
        \    Animation speed: o, p\n\
        \    Save animation: s\n\
        \"
  mainLoop
