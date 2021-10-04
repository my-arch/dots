{-# LANGUAGE FlexibleContexts, DeriveDataTypeable
  , UndecidableInstances, FlexibleInstances, MultiParamTypeClasses
  , PatternGuards, Rank2Types, TypeSynonymInstances, ScopedTypeVariables #-}

module Local.MC where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Stack
import XMonad.Layout (splitHorizontallyBy, splitVerticallyBy)
import Data.Maybe
import Data.List (intercalate, transpose, findIndex)
import Control.Arrow (first, second, (&&&))
import Control.Applicative ((<$>))
import qualified Data.Map.Strict as M
import qualified Debug.Trace as D
import XMonad.Util.Types (Direction2D (..))
import Graphics.X11.Xlib.Extras (getWindowAttributes,
                                 WindowAttributes (..))

import Graphics.X11.Xlib.Misc (warpPointer)
import XMonad.Actions.RotSlaves (rotAll')
import qualified XMonad.Actions.TagWindows as T
import XMonad.Hooks.DebugEvents

import Data.Monoid

data MC l a = MC
  { cells :: [(Rational, [Rational])]
  , lastCells :: [(Rational, [Rational])]
  , overflow :: l a
  , coords :: M.Map a (Int, Int)
  , mirror :: Bool
  , lastRect :: Rectangle
  , overflowFocus :: Int
  , overflowSize :: Int
  , workspaceId :: WorkspaceId
  } deriving (Read, Show)

mc :: l a -> [(Rational, [Rational])] -> MC l a
mc il c0 = MC { cells = c0
              , overflow = il
              , coords = M.empty
              , mirror = False
              , lastCells = []
              , lastRect = Rectangle 0 0 10 10
              , overflowFocus = 0
              , overflowSize = 0
              , workspaceId = "" }

data MCMsg a =
  SetCells [(Rational, [Rational])] |
  ResizeCell Rational Rational a |
  SetEdge Direction2D Position a |
  ChangeCells (Maybe (Int, Int) -> [(Rational, [Rational])] -> [(Rational, [Rational])]) a |
  Flip |
  OnOverflow ([Window] -> [Window]) |
  WithOverflowFocus (Window -> X ()) |
  WithOverflowFocusIndex (Int -> Int) |
  OverflowFocusMaster |
  FocusCell Int |
  OverflowFocusWindow a

instance Typeable a => Message (MCMsg a)

flipR :: Rectangle -> Rectangle
flipR (Rectangle sx sy sw sh) = Rectangle sy sx sh sw

a /? 0 = a
a /? b = a / b

cellCount :: MC l a -> Int
cellCount state = foldl (+) 0 $ map (length . snd) $ cells state

instance (LayoutClass l Window) => LayoutClass (MC l) Window where
  description (MC { cells = cs, mirror = m }) =
    concat [ if m then "M" else ""
           , intercalate "|" (map (show . length . snd) cs) ]

  runLayout wspa@(W.Workspace wid state stack) rect' = do
    let mirr = if mirror state then flipR else id
        rect = mirr rect'
        ws = W.integrate' stack
        capacity = cellCount state
        demand = length ws

        cutV (Rectangle sx sy sw sh) (l, r) =
          let wx = sx + ((round $ (l * fromIntegral sw)) :: Position)
              wr = sx + ((round $ (r * fromIntegral sw)) :: Position)
              ww = (fromIntegral wr - fromIntegral wx) :: Dimension
          in Rectangle wx sy ww sh
        cutH r bs = flipR $ cutV (flipR r) bs

        explode cut rect rats =
          let t = sum rats
              parts = map (flip (/?) t) $ scanl (+) 0 rats
              bounds = zip parts (drop 1 parts)
          in map (cut rect) bounds

        limit0 :: Int -> [(Rational, [Rational])] -> [(Rational, [Rational])]
        limit0 _ [] = []
        limit0 n [x] = snd $ limit n [x]
        limit0 n ((cw, rws):xs) =
          let (n', res) = limit (n - 1) xs in
            (cw, take (n - n') rws):res

        limit :: Int -> [(Rational, [Rational])] -> (Int, [(Rational, [Rational])])
        limit 0 _ = (0, [])
        limit _ [] = (0, [])
        limit n ((cw, rws):rest)
          | n < 0 = (0, [])
          | length rws >= n = (n, [(cw, take n rws)])
          | otherwise = let (n', l') = (limit (n - length rws) rest) in
              (n' + length rws, (cw, rws):l')

        divide :: [(Rational, [Rational])] -> [(Rectangle, (Int, Int))]
        divide cs = let cols = explode cutV rect $ map fst cs
                        rows = map (uncurry (explode cutH)) $ zip cols $ map snd cs
                   in concatMap (\(c, rs) -> zip rs (map ((,) c) [0 :: Int ..])) $ zip [0 :: Int ..] rows


    mapM_ (T.delTag "overflow") ws
    if capacity >= demand
      then do let cs = limit0 demand $ cells state
                  rs = divide cs
                  rects = map fst rs
              o' <- handleMessage (overflow state) $ SomeMessage Hide
              let state' = state { overflow = fromMaybe (overflow state) o'
                                 , coords = M.fromList (zip ws $ map snd rs)
                                 , lastCells = cs
                                 , lastRect = rect
                                 , overflowFocus = 0
                                 , overflowSize = 0
                                 , workspaceId = wid }
              return $ (zip ws (map mirr rects), Just $ state')
      else do let rs = divide $ cells state
                  rects = map fst rs
                  (main, extra) = splitAt (capacity - 1) ws
                  nextra = length extra
                  fIndex = ((maybe 0 (length . W.up) stack) - (capacity - 1))
                  odex
                    | fIndex < 0 = (min (nextra - (overflowSize state) + overflowFocus state) (nextra - 1))
                    | otherwise = fIndex
                  ostack = fromIndex extra odex
                  orect = last rects
                  ocoord = (length (cells state) - 1,
                            length (snd $ last $ cells state) - 1)
              whenJust ostack $ \st -> T.addTag "overflow" (W.focus st)
              (owrs, o') <- runLayout wspa { W.stack = ostack
                                           , W.layout = (overflow state) } (mirr orect)
              let state' = state { overflow = fromMaybe (overflow state) o'
                                 , coords = M.fromList (zip main $ map snd rs) `M.union` M.fromList (zip extra $ repeat ocoord)
                                 , lastCells = (cells state)
                                 , lastRect = rect
                                 , overflowFocus = odex
                                 , overflowSize = nextra
                                 , workspaceId = wid }
              return $ ((zip main (map mirr rects)) ++ owrs, Just $ state')

  handleMessage state sm
    | Just (ButtonEvent {}) <- fromMessage sm = overflowHandle state sm
    | Just (PropertyEvent {}) <- fromMessage sm = overflowHandle state sm
    | Just (ExposeEvent {}) <- fromMessage sm = overflowHandle state sm
    | Just (Hide) <- fromMessage sm = overflowHandle state sm
    | Just (ReleaseResources) <- fromMessage sm = overflowHandle state sm
    | Just (SetCells cs :: MCMsg Window) <- fromMessage sm = if (cells state) /= cs
                                                             then return $ Just $ state { cells = cs }
                                                             else return $ Nothing
    | Just (ResizeCell dx dy a) <- fromMessage sm =
        let (dx', dy') = if mirror state then (dy, dx) else (dx, dy) in
          return $ (resizeCell (normalizeState state) (+ dx') (+ dy')) <$> (M.lookup a $ coords state)

    | Just (SetEdge e pos w) <- fromMessage sm =
        let lr = (if mirror state then flipR else id) (lastRect state)
            p :: Rational
            p = if e == L || e == R
                then (fromIntegral $ pos - (fromIntegral $ rect_x $ lr)) /?
                     (fromIntegral $ rect_width $ lr)
                else (fromIntegral $ pos - (fromIntegral $ rect_y $ lr)) /?
                     (fromIntegral $ rect_height $ lr)
        in return $ (setEdgeAbsolute (normalizeState state) (unmirror e) p) <$> (M.lookup w $ coords state)

    | Just (ChangeCells f w :: MCMsg Window) <- fromMessage sm =
        return $ Just $ state { cells = f (M.lookup w $ coords state) (cells state) }

    | Just (Flip :: MCMsg Window) <- fromMessage sm = return $ Just $ state { mirror = not (mirror state) }

    | Just (OnOverflow f :: MCMsg Window) <- fromMessage sm = do
        -- only works if we send the message to the focused workspace
        -- how do we find out which workspace we are on
        let applyF = (rotAll' $ \l -> let (u, d) = splitAt maxIndex l
                                           in u ++ f d)
            modify w@(W.Workspace t _ mst)
              | t == (workspaceId state) = w { W.stack = applyF <$> mst }
              | otherwise = w
        -- rearrange some of the windows
        windows $ W.mapWorkspace modify
        return Nothing

    | Just (WithOverflowFocus f :: MCMsg Window) <- fromMessage sm = do
        wsm <- gets (listToMaybe .
                     (filter ((== (workspaceId state)) . W.tag)) .
                      W.workspaces . windowset)
        whenJust wsm $ \ws -> do
          let offset = maxIndex + (overflowFocus state)
              theWindow = take 1 $ drop offset $ W.integrate' $ W.stack ws
          case theWindow of
            [] -> return ()
            (x:_) -> f x
        return Nothing

    | Just (WithOverflowFocusIndex f :: MCMsg Window) <- fromMessage sm =
        -- if we change the focus while we are looking at it, we need to focus down or up a bit
        do let oldFocus = overflowFocus state
               newFocus = (f oldFocus) `mod` (overflowSize state)
           if newFocus == oldFocus then
             (return Nothing)
             else
             do st <- gets (W.stack . W.workspace . W.current . windowset)
                let up = maybe 0 (length . W.up) st
                if up >= maxIndex then
                  whenJust (listToMaybe $ drop (maxIndex + newFocus) $ W.integrate' st) $ \w -> windows (W.focusWindow w)
                  else
                  return ()
                return $ Just $ state {overflowFocus = newFocus}

    | Just (OverflowFocusMaster :: MCMsg Window) <- fromMessage sm =
        return $ Just $ state { overflowFocus = 0 }

    | Just (OverflowFocusWindow w :: MCMsg Window) <- fromMessage sm = do
        wsm <- gets (listToMaybe .
                     (filter ((== (workspaceId state)) . W.tag)) .
                      W.workspaces . windowset)
        focusm <- gets (W.peek . windowset)
        case wsm of
          (Just ws) ->
            let overflow = drop maxIndex $ W.integrate' $ W.stack ws
                windex = findIndex (== w) overflow
                findex = fromMaybe False $ (flip elem overflow <$> focusm)
            in case (findex, windex) of
              (_, Nothing) -> return Nothing
              (True, Just _) -> ((windows $ W.focusWindow w) >> return Nothing)
              (False, Just i) -> return $ Just state { overflowFocus = i }
          _ -> return Nothing

    | Just (FocusCell i :: MCMsg Window) <- fromMessage sm =
        let focusNth n = windows $ foldr (.) W.focusMaster (take n $ repeat W.focusDown)
            action
              | i < maxIndex = focusNth i
              | otherwise = focusNth (maxIndex + overflowFocus state)
        in action >> return Nothing

    | otherwise = return Nothing
    where
      capacity = cellCount state
      maxIndex = capacity - 1
      unmirror e
        | mirror state = case e of
            L -> U
            R -> D
            U -> L
            D -> R
        | otherwise = e
overflowHandle state sm = do o' <- handleMessage (overflow state) sm
                             return $ fmap (\x -> state {overflow = x}) o'

-- sometimes emacs requests to map a window that xmonad thinks is already mapped
-- but the layout did not give it a rectangle, or something. not quite sure.
mappingEventHook :: Event -> X All
mappingEventHook (MapRequestEvent {ev_window = w}) = do
  ws <- gets windowset
  let tm = W.findTag w ws
  whenJust tm $ \t ->
    sendMessageWithNoRefresh (OverflowFocusWindow w) $ head $ filter ((== t) . W.tag) $ W.workspaces ws
  refresh
  return (All True)
mappingEventHook (ClientMessageEvent {ev_message_type = mt, ev_data = d, ev_window = w}) = do
  a_aw <- getAtom "_NET_ACTIVE_WINDOW"
  if mt == a_aw then do
    refresh
    else return ()
  return (All True)
mappingEventHook _ = return (All True)

normalizeState state =
  let cells0 = cells state
      cells1 = lastCells state
      ctotal = sum $ map fst cells1
      rtotals = (map (sum . snd) cells1) ++ repeat 0
  in state { cells =
             flip map (zip cells0 rtotals) $
             \((c, rs), rtotal) -> (c /? ctotal, map (flip (/?) rtotal) rs)
           }

setAbsolute _ _ _ [] = []
setAbsolute 0 p psf (t:(n:rs)) =
  let t' = min (n + t - 0.05) $ max 0.05 (p - psf)
      n' = n + (t - t')
  in t':n':rs
setAbsolute 0 p psf (t:[]) = [t]
setAbsolute n p psf (x:xs) = x:(setAbsolute (n - 1) p (psf + x) xs)

setEdgeAbsolute :: MC l a -> Direction2D -> Rational -> (Int, Int) -> MC l a
setEdgeAbsolute state e p (c, r)
  | c < 0 = state
  | r < 0 = state
  | e == L = setEdgeAbsolute state R p (c - 1, r)
  | e == U = setEdgeAbsolute state D p (c, r - 1)
  | e == R = let cps = setAbsolute c p 0 $ (map fst $ cells state)
             in state { cells = zip cps $ map snd $ cells state }
  | e == D = state { cells = toNth (second $ (setAbsolute r p 0)) c (cells state) }
  | otherwise = state

resizeCell state tx ty (ci, ri) = state
  { cells = toNth (second (toNth ty ri) . first tx) ci (cells state) }

toNth _ _ [] = []
toNth f 0 (x:xs) = (f x):xs
toNth f n (x:xs) = x:(toNth f (n-1) xs)


-- This has a bug in that it uses the whole screen rect when the
-- layout sees a reduced rect because of e.g. struts
mouseResizeTile :: Rational -> (Window -> X ())  -> Window -> X ()
mouseResizeTile border fallback w =
  whenX (isClient w) $
  withDisplay $ \dpy -> do
  wa <- io $ getWindowAttributes dpy w
  (_, _, _, ox', oy', _, _, _) <- io $ queryPointer dpy w

  let wx = fromIntegral $ wa_x wa
      wy = fromIntegral $ wa_y wa
      ww = fromIntegral $ wa_width wa
      wh = fromIntegral $ wa_height wa
      ox = fromIntegral ox'
      oy = fromIntegral oy'

      drag mouse wpos wdim e1 e2
        | mouse - wpos < (wdim * border) =
            (True, 0, \px -> sendMessage $ SetEdge e1 px w)
        | (wpos + wdim) - mouse < (wdim * border) =
            (True, wdim, \px -> sendMessage $ SetEdge e2 px w)
        | otherwise =
            (False, mouse - wpos, \px -> return ())

      (hitX, warpx, dragX) = drag ox wx ww L R
      (hitY, warpy, dragY) = drag oy wy wh U D

      dragHandler x y = do
        dragX x
        dragY y
      stopHandler = return ()
  if hitX || hitY
    -- warp pointer here
    then do io $ warpPointer dpy none w 0 0 0 0 (floor warpx) (floor warpy)
            mouseDrag dragHandler stopHandler
    else fallback w
