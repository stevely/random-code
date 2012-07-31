{-
  SpinningShapes.hs
  By Steven Smith
-}

import Control.Applicative
import Control.Monad
import Data.IORef
import System.Exit
import Graphics.UI.GLUT

infixl 6 $+, $-
($+), ($-) :: (Applicative t, Num a) => t a -> t a -> t a
($+) = liftA2 (+)
($-) = liftA2 (-)

data State = State {
    currentAngleOfRotation :: IORef GLfloat,
    rotationRatePerSecond  :: IORef GLfloat,
    framesPerSecond        :: IORef Int,
    rotateX                :: IORef Int,
    rotateY                :: IORef Int,
    modifiers              :: IORef Modifiers
    }

makeState :: IO State
makeState = do
    car <- newIORef 1
    rps <- newIORef 6
    fps <- newIORef 30
    rtx <- newIORef 0
    rty <- newIORef 0
    mod <- newIORef (Modifiers Up Up Up)
    return $ State {
        currentAngleOfRotation = car,
        rotationRatePerSecond  = rps,
        framesPerSecond        = fps,
        rotateX                = rtx,
        rotateY                = rty,
        modifiers              = mod
        }

drawFace :: Normal3 GLfloat
         -> (Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat)
         -> IO ()
drawFace norm (a, b, c, d) = do
    normal norm
    vertex a
    vertex b
    vertex c
    vertex d

drawCube :: IO ()
drawCube = do
    let size = 20
        indices = [Vertex3 (-size) (-size) (-size),
                   Vertex3 (-size) (-size) ( size),
                   Vertex3 (-size) ( size) ( size),
                   Vertex3 (-size) ( size) (-size),
                   Vertex3 ( size) ( size) (-size),
                   Vertex3 ( size) (-size) (-size),
                   Vertex3 ( size) (-size) ( size),
                   Vertex3 ( size) ( size) ( size)]
        f = (indices !!)
        pick a b c d = (f a, f b, f c, f d)
        faces = [pick 0 1 2 3,
                 pick 7 6 5 4,
                 pick 5 6 1 0,
                 pick 7 4 3 2,
                 pick 1 2 7 6,
                 pick 3 4 5 0]
        normals = [Normal3 (-1)   0   0,
                   Normal3   1    0   0,
                   Normal3   0  (-1)  0,
                   Normal3   0    1   0,
                   Normal3   0    0   1,
                   Normal3   0    0 (-1)]
    renderPrimitive Quads $ zipWithM_ drawFace normals faces

reshape :: ReshapeCallback
reshape size@(Size w h) = do
    let aspect = fromIntegral w / fromIntegral h
        l      = -50.0 :: GLdouble
        r      =  50.0 :: GLdouble
    viewport   $= (Position 0 0, size)
    matrixMode $= Projection
    loadIdentity
    if w <= h
    then ortho l r (l / aspect) (r / aspect) l r
    else ortho (l * aspect) (r * aspect) l r l r
    matrixMode $= Modelview 0

display :: State -> DisplayCallback
display state = do
    clear [ ColorBuffer, DepthBuffer ]
    matrixMode $= Modelview 0
    loadIdentity
    rotX         <- get (rotateX state)
    rotY         <- get (rotateY state)
    currentAngle <- get (currentAngleOfRotation state )
    rotate (fromIntegral rotX :: GLfloat) (Vector3 1 0 0)
    rotate (fromIntegral rotY :: GLfloat) (Vector3 0 1 0)
    rotate currentAngle (Vector3 1 1 1)
    color (Color3 1.0 1.0 (1.0 :: GLfloat))

    drawCube

    flush
    swapBuffers

timer :: State -> TimerCallback
timer state = do
    rotPerSec <- get (rotationRatePerSecond  state)
    fps       <- get (framesPerSecond        state)
    let angle = currentAngleOfRotation state
    angle $~ (+ rotPerSec * (1 / (fromIntegral fps)))
    a <- get angle
    when (a > 360) $ do
        angle $~ (`subtract` 360)
    postRedisplay Nothing
    addTimerCallback (fromIntegral $ 1000 `div` fps) (timer state)

keyboard :: State -> KeyboardMouseCallback
keyboard state key keyState mods _ = do
    modifiers state $= mods
    postRedisplay Nothing
    case (key, keyState) of
        (Char 'q', Down) -> exitWith ExitSuccess
        (Char '\27', Down) -> exitWith ExitSuccess
        otherwise -> return ()

main :: IO ()
main = do
    getArgsAndInitialize
    initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]
    initialWindowPosition $= Position 80 80
    initialWindowSize $= Size 500 500
    createWindow "Spinning Shapes"

    state <- makeState
    displayCallback $= display state
    keyboardMouseCallback $= Just (keyboard state)
    reshapeCallback $= Just reshape
    fps <- get (framesPerSecond state)
    addTimerCallback (fromIntegral $ 1000 `div` fps) (timer state)
    depthFunc $= Just Less

    materialDiffuse Front $= Color4 1 0 0 1
    materialDiffuse Front $= Color4 0.3 0.3 0.3 1
    materialShininess Front $= 16
    lighting $= Enabled
    position (Light 0) $= Vertex4 0 0 50 0
    light (Light 0) $= Enabled

    mainLoop
