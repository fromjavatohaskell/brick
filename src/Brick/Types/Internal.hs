{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Brick.Types.Internal
  ( ScrollRequest(..)
  , VisibilityRequest(..)
  , vrPositionL
  , vrSizeL
  , Location(..)
  , locL
  , origin
  , TerminalLocation(..)
  , Viewport(..)
  , ViewportType(..)
  , RenderState(..)
  , Direction(..)
  , CursorLocation(..)
  , cursorLocationL
  , cursorLocationNameL
  , Context(..)
  , EventState(..)
  , EventRO(..)
  , Next(..)
  , Result(..)
  , Extent(..)
  , Edges(..)
  , eTopL, eBottomL, eRightL, eLeftL
  , BorderSegment(..)
  , bsAcceptL, bsOfferL, bsDrawL
  , DynBorder(..)
  , dbStyleL, dbAttrL, dbSegmentsL
  , CacheInvalidateRequest(..)
  , BrickEvent(..)

  , rsScrollRequestsL
  , viewportMapL
  , clickableNamesL
  , renderCacheL
  , observedNamesL
  , vpSize
  , vpLeft
  , vpTop
  , imageL
  , cursorsL
  , extentsL
  , bordersL
  , visibilityRequestsL
  , emptyResult
  )
where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

import Lens.Micro (_1, _2, Lens')
import qualified Data.Set as S
import qualified Data.Map as M
import Graphics.Vty (Vty, Event, Button, Modifier, DisplayRegion, Image, Attr, emptyImage)
import GHC.Generics
import Control.DeepSeq (NFData)

import Brick.BorderMap (BorderMap)
import qualified Brick.BorderMap as BM
import Brick.Types.Common
import Brick.AttrMap (AttrName, AttrMap)
import Brick.Widgets.Border.Style (BorderStyle)

data ScrollRequest = HScrollBy Int
                   | HScrollPage Direction
                   | HScrollToBeginning
                   | HScrollToEnd
                   | VScrollBy Int
                   | VScrollPage Direction
                   | VScrollToBeginning
                   | VScrollToEnd
                   | SetTop Int
                   | SetLeft Int
                   deriving (Read, Show, Generic, NFData)

data VisibilityRequest =
    VR { vrPosition :: Location
       , vrSize :: DisplayRegion
       }
       deriving (Show, Eq, Read, Generic, NFData)

vrPositionL :: Lens' VisibilityRequest Location
vrPositionL f x = fmap (\y -> x{vrPosition = y}) (f $ vrPosition x)
{-# INLINE vrPositionL #-}

vrSizeL :: Lens' VisibilityRequest DisplayRegion
vrSizeL f x = fmap (\y -> x{vrSize = y}) (f $ vrSize x)
{-# INLINE vrSizeL #-}

-- | Describes the state of a viewport as it appears as its most recent
-- rendering.
data Viewport =
    VP { _vpLeft :: Int
       -- ^ The column offset of left side of the viewport.
       , _vpTop :: Int
       -- ^ The row offset of the top of the viewport.
       , _vpSize :: DisplayRegion
       -- ^ The size of the viewport.
       }
       deriving (Show, Read, Generic, NFData)

vpLeft :: Lens' Viewport Int
vpLeft f x = fmap (\y -> x{_vpLeft = y}) (f $ _vpLeft x)
{-# INLINE vpLeft #-}

vpTop :: Lens' Viewport Int
vpTop f x = fmap (\y -> x{_vpTop = y}) (f $ _vpTop x)
{-# INLINE vpTop #-}

vpSize :: Lens' Viewport DisplayRegion
vpSize f x = fmap (\y -> x{_vpSize = y}) (f $ _vpSize x)
{-# INLINE vpSize #-}

-- | The type of viewports that indicates the direction(s) in which a
-- viewport is scrollable.
data ViewportType = Vertical
                  -- ^ Viewports of this type are scrollable only vertically.
                  | Horizontal
                  -- ^ Viewports of this type are scrollable only horizontally.
                  | Both
                  -- ^ Viewports of this type are scrollable vertically and horizontally.
                  deriving (Show, Eq)

data CacheInvalidateRequest n =
    InvalidateSingle n
    | InvalidateEntire
    deriving (Ord, Eq)

data EventState n = ES { esScrollRequests :: [(n, ScrollRequest)]
                       , cacheInvalidateRequests :: S.Set (CacheInvalidateRequest n)
                       }

-- | An extent of a named area: its size, location, and origin.
data Extent n = Extent { extentName      :: n
                       , extentUpperLeft :: Location
                       , extentSize      :: (Int, Int)
                       , extentOffset    :: Location
                       }
                       deriving (Show, Read, Generic, NFData)

-- | The type of actions to take upon completion of an event handler.
data Next a = Continue a
            | SuspendAndResume (IO a)
            | Halt a
            deriving Functor

-- | Scrolling direction.
data Direction = Up
               -- ^ Up/left
               | Down
               -- ^ Down/right
               deriving (Show, Eq, Read, Generic, NFData)

-- | The class of types that behave like terminal locations.
class TerminalLocation a where
    -- | Get the column out of the value
    locationColumnL :: Lens' a Int
    locationColumn :: a -> Int

    -- | Get the row out of the value
    locationRowL :: Lens' a Int
    locationRow :: a -> Int

instance TerminalLocation Location where
    locationColumnL = _1
    locationColumn (Location t) = fst t
    locationRowL = _2
    locationRow (Location t) = snd t

-- | A cursor location.  These are returned by the rendering process.
data CursorLocation n =
    CursorLocation { cursorLocation :: !Location
                   -- ^ The location
                   , cursorLocationName :: !(Maybe n)
                   -- ^ The name of the widget associated with the location
                   }
                   deriving (Read, Show, Generic, NFData)

cursorLocationL :: Lens' (CursorLocation n) Location
cursorLocationL f x = fmap (\y -> x{cursorLocation = y}) (f $ cursorLocation x)
{-# INLINE cursorLocationL #-}

cursorLocationNameL :: Lens' (CursorLocation n) (Maybe n)
cursorLocationNameL f x = fmap (\y -> x{cursorLocationName = y}) (f $ cursorLocationName x)
{-# INLINE cursorLocationNameL #-}

-- | A border character has four segments, one extending in each direction
-- (horizontally and vertically) from the center of the character.
data BorderSegment = BorderSegment
    { bsAccept :: Bool
    -- ^ Would this segment be willing to be drawn if a neighbor wanted to
    -- connect to it?
    , bsOffer :: Bool
    -- ^ Does this segment want to connect to its neighbor?
    , bsDraw :: Bool
    -- ^ Should this segment be represented visually?
    } deriving (Eq, Ord, Read, Show, Generic, NFData)

bsAcceptL :: Lens' BorderSegment Bool
bsAcceptL f x = fmap (\y -> x{bsAccept = y}) (f $ bsAccept x)
{-# INLINE bsAcceptL #-}

bsOfferL :: Lens' BorderSegment Bool
bsOfferL f x = fmap (\y -> x{bsOffer = y}) (f $ bsOffer x)
{-# INLINE bsOfferL #-}

bsDrawL :: Lens' BorderSegment Bool
bsDrawL f x = fmap (\y -> x{bsDraw = y}) (f $ bsDraw x)
{-# INLINE bsDrawL #-}

-- | Information about how to redraw a dynamic border character when it abuts
-- another dynamic border character.
data DynBorder = DynBorder
    { dbStyle :: BorderStyle
    -- ^ The 'Char's to use when redrawing the border. Also used to filter
    -- connections: only dynamic borders with equal 'BorderStyle's will connect
    -- to each other.
    , dbAttr :: Attr
    -- ^ What 'Attr' to use to redraw the border character. Also used to filter
    -- connections: only dynamic borders with equal 'Attr's will connect to
    -- each other.
    , dbSegments :: Edges BorderSegment
    } deriving (Eq, Read, Show, Generic, NFData)

dbStyleL :: Lens' DynBorder BorderStyle
dbStyleL f x = fmap (\y -> x{dbStyle = y}) (f $ dbStyle x)
{-# INLINE dbStyleL #-}

dbAttrL :: Lens' DynBorder Attr
dbAttrL f x = fmap (\y -> x{dbAttr = y}) (f $ dbAttr x)
{-# INLINE dbAttrL #-}

dbSegmentsL :: Lens' DynBorder (Edges BorderSegment)
dbSegmentsL f x = fmap (\y -> x{dbSegments = y}) (f $ dbSegments x)
{-# INLINE dbSegmentsL #-}

-- | The type of result returned by a widget's rendering function. The
-- result provides the image, cursor positions, and visibility requests
-- that resulted from the rendering process.
data Result n =
    Result { image :: Image
           -- ^ The final rendered image for a widget
           , cursors :: [CursorLocation n]
           -- ^ The list of reported cursor positions for the
           -- application to choose from
           , visibilityRequests :: [VisibilityRequest]
           -- ^ The list of visibility requests made by widgets rendered
           -- while rendering this one (used by viewports)
           , extents :: [Extent n]
           -- Programmer's note: we don't try to maintain the invariant that
           -- the size of the borders closely matches the size of the 'image'
           -- field. Most widgets don't need to care about borders, and so they
           -- use the empty 'BorderMap' that has a degenerate rectangle. Only
           -- border-drawing widgets and the hbox/vbox stuff try to set this
           -- carefully. Even then, in the boxes, we only make sure that the
           -- 'BorderMap' is no larger than the entire concatenation of boxes,
           -- and it's certainly possible for it to be smaller. (Resizing
           -- 'BorderMap's is lossy, so we try to do it as little as possible.)
           -- If you're writing a widget, this should make it easier for you to
           -- do so; but beware this lack of invariant if you are consuming
           -- widgets.
           , borders :: BorderMap DynBorder
           -- ^ Places where we may rewrite the edge of the image when
           -- placing this widget next to another one.
           }
           deriving (Show, Read, Generic, NFData)

imageL :: Lens' (Result n) Image
imageL f x = fmap (\y -> x{image = y}) (f $ image x)
{-# INLINE imageL #-}

cursorsL :: Lens' (Result n) [CursorLocation n]
cursorsL f x = fmap (\y -> x{cursors = y}) (f $ cursors x)
{-# INLINE cursorsL #-}

visibilityRequestsL :: Lens' (Result n) [VisibilityRequest]
visibilityRequestsL f x = fmap (\y -> x{visibilityRequests = y}) (f $ visibilityRequests x)
{-# INLINE visibilityRequestsL #-}

extentsL :: Lens' (Result n) [Extent n]
extentsL f x = fmap (\y -> x{extents = y}) (f $ extents x)
{-# INLINE extentsL #-}

bordersL :: Lens' (Result n) (BorderMap DynBorder)
bordersL f x = fmap (\y -> x{borders = y}) (f $ borders x)
{-# INLINE bordersL #-}

emptyResult :: Result n
emptyResult = Result emptyImage [] [] [] BM.empty

-- | The type of events.
data BrickEvent n e = VtyEvent Event
                    -- ^ The event was a Vty event.
                    | AppEvent e
                    -- ^ The event was an application event.
                    | MouseDown n Button [Modifier] Location
                    -- ^ A mouse-down event on the specified region was
                    -- received. The 'n' value is the resource name of
                    -- the clicked widget (see 'clickable').
                    | MouseUp n (Maybe Button) Location
                    -- ^ A mouse-up event on the specified region was
                    -- received. The 'n' value is the resource name of
                    -- the clicked widget (see 'clickable').
                    deriving (Show, Eq, Ord)

data RenderState n =
    RS { viewportMap :: M.Map n Viewport
       , rsScrollRequests :: [(n, ScrollRequest)]
       , observedNames :: !(S.Set n)
       , renderCache :: M.Map n (Result n)
       , clickableNames :: [n]
       } deriving (Read, Show, Generic, NFData)

viewportMapL :: Lens' (RenderState n) (M.Map n Viewport)
viewportMapL f x = fmap (\y -> x{viewportMap = y}) (f $ viewportMap x)
{-# INLINE viewportMapL #-}

rsScrollRequestsL :: Lens' (RenderState n) [(n, ScrollRequest)] 
rsScrollRequestsL f x = fmap (\y -> x{rsScrollRequests = y}) (f $ rsScrollRequests x)
{-# INLINE rsScrollRequestsL #-}

observedNamesL :: Lens' (RenderState n) (S.Set n)
observedNamesL f x = fmap (\y -> x{observedNames = y}) (f $ observedNames x)
{-# INLINE observedNamesL #-}

renderCacheL :: Lens' (RenderState n) (M.Map n (Result n))
renderCacheL f x = fmap (\y -> x{renderCache = y}) (f $ renderCache x)
{-# INLINE renderCacheL #-}

clickableNamesL :: Lens' (RenderState n) [n]
clickableNamesL f x = fmap (\y -> x{clickableNames = y}) (f $ clickableNames x)
{-# INLINE clickableNamesL #-}

data EventRO n = EventRO { eventViewportMap :: M.Map n Viewport
                         , eventVtyHandle :: Vty
                         , latestExtents :: [Extent n]
                         , oldState :: RenderState n
                         }

-- | The rendering context. This tells widgets how to render: how much
-- space they have in which to render, which attribute they should use
-- to render, which bordering style should be used, and the attribute map
-- available for rendering.
data Context =
    Context { ctxAttrName :: AttrName
            , availWidth :: Int
            , availHeight :: Int
            , windowWidth :: Int
            , windowHeight :: Int
            , ctxBorderStyle :: BorderStyle
            , ctxAttrMap :: AttrMap
            , ctxDynBorders :: Bool
            }
            deriving Show

