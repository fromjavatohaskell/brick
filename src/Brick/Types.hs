-- | Basic types used by this library.
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Brick.Types
  ( -- * The Widget type
    Widget(..)

    -- * Location types and lenses
  , Location(..)
  , locL
  , TerminalLocation(..)
  , CursorLocation(..)
  , cursorLocationL
  , cursorLocationNameL

  -- * Viewports
  , Viewport(..)
  , ViewportType(..)
  , vpSize
  , vpTop
  , vpLeft

  -- * Event-handling types
  , EventM(..)
  , Next
  , BrickEvent(..)
  , handleEventLensed

  -- * Rendering infrastructure
  , RenderM
  , getContext

  -- ** The rendering context
  , Context(ctxAttrName, availWidth, availHeight, windowWidth, windowHeight, ctxBorderStyle, ctxAttrMap, ctxDynBorders)
  , attrL
  , availWidthL
  , availHeightL
  , windowWidthL
  , windowHeightL
  , ctxAttrMapL
  , ctxAttrNameL
  , ctxBorderStyleL
  , ctxDynBordersL

  -- ** Rendering results
  , Result(..)
  , emptyResult
  , lookupAttrName
  , Extent(..)

  -- ** Rendering result lenses
  , imageL
  , cursorsL
  , visibilityRequestsL
  , extentsL

  -- ** Visibility requests
  , VisibilityRequest(..)
  , vrPositionL
  , vrSizeL

  -- * Dynamic borders
  , bordersL
  , DynBorder(..)
  , dbStyleL, dbAttrL, dbSegmentsL
  , BorderSegment(..)
  , bsAcceptL, bsOfferL, bsDrawL
  , Edges(..)
  , eTopL, eBottomL, eRightL, eLeftL

  -- * Miscellaneous
  , Size(..)
  , Padding(..)
  , Direction(..)

  -- * Renderer internals (for benchmarking)
  , RenderState
  )
where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Monoid (Monoid(..))
#endif

import Lens.Micro (_1, _2, to, (^.), (&), (.~), Lens')
import Lens.Micro.Type (Getting)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Brick.Widgets.Border.Style (BorderStyle)
import Brick.AttrMap (AttrMap)
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Graphics.Vty (Attr)
import Control.Monad.IO.Class

import Brick.Types.Internal
import Brick.AttrMap (AttrName, attrMapLookup)

-- | The type of padding.
data Padding = Pad Int
             -- ^ Pad by the specified number of rows or columns.
             | Max
             -- ^ Pad up to the number of available rows or columns.

-- | A convenience function for handling events intended for values
-- that are targets of lenses in your application state. This function
-- obtains the target value of the specified lens, invokes 'handleEvent'
-- on it, and stores the resulting transformed value back in the state
-- using the lens.
handleEventLensed :: a
                  -- ^ The state value.
                  -> Lens' a b
                  -- ^ The lens to use to extract and store the target
                  -- of the event.
                  -> (e -> b -> EventM n b)
                  -- ^ The event handler.
                  -> e
                  -- ^ The event to handle.
                  -> EventM n a
handleEventLensed v target handleEvent ev = do
    newB <- handleEvent ev (v^.target)
    return $ v & target .~ newB

-- | The monad in which event handlers run. Although it may be tempting
-- to dig into the reader value yourself, just use
-- 'Brick.Main.lookupViewport'.
newtype EventM n a =
    EventM { runEventM :: ReaderT (EventRO n) (StateT (EventState n) IO) a
           }
           deriving ( Functor, Applicative, Monad, MonadIO
                    , MonadThrow, MonadCatch, MonadMask, MonadFail
                    )

-- | Widget size policies. These policies communicate how a widget uses
-- space when being rendered. These policies influence rendering order
-- and space allocation in the box layout algorithm for 'hBox' and
-- 'vBox'.
data Size = Fixed
          -- ^ Widgets advertising this size policy should take up the
          -- same amount of space no matter how much they are given,
          -- i.e. their size depends on their contents alone rather than
          -- on the size of the rendering area.
          | Greedy
          -- ^ Widgets advertising this size policy must take up all the
          -- space they are given.
          deriving (Show, Eq, Ord)

-- | The type of widgets.
data Widget n =
    Widget { hSize :: Size
           -- ^ This widget's horizontal growth policy
           , vSize :: Size
           -- ^ This widget's vertical growth policy
           , render :: RenderM n (Result n)
           -- ^ This widget's rendering function
           }

-- | The type of the rendering monad. This monad is used by the
-- library's rendering routines to manage rendering state and
-- communicate rendering parameters to widgets' rendering functions.
type RenderM n a = ReaderT Context (State (RenderState n)) a

-- | Get the current rendering context.
getContext :: RenderM n Context
getContext = ask

-- | The rendering context's current drawing attribute.
attrL :: forall r. Getting r Context Attr
attrL = to (\c -> attrMapLookup (c^.ctxAttrNameL) (c^.ctxAttrMapL))

instance TerminalLocation (CursorLocation n) where
    locationColumnL = cursorLocationL._1
    locationColumn = locationColumn . cursorLocation
    locationRowL = cursorLocationL._2
    locationRow = locationRow . cursorLocation

-- | Given an attribute name, obtain the attribute for the attribute
-- name by consulting the context's attribute map.
lookupAttrName :: AttrName -> RenderM n Attr
lookupAttrName n = do
    c <- getContext
    return $ attrMapLookup n (c^.ctxAttrMapL)

ctxAttrNameL :: Lens' Context AttrName
ctxAttrNameL f x = fmap (\y -> x{ctxAttrName = y}) (f $ ctxAttrName x)
{-# INLINE ctxAttrNameL #-}

availWidthL :: Lens' Context Int
availWidthL f x = fmap (\y -> x{availWidth = y}) (f $ availWidth x)
{-# INLINE availWidthL #-}

availHeightL :: Lens' Context Int
availHeightL f x = fmap (\y -> x{availHeight = y}) (f $ availHeight x)
{-# INLINE availHeightL #-}

windowWidthL :: Lens' Context Int
windowWidthL f x = fmap (\y -> x{windowWidth = y}) (f $ windowWidth x)
{-# INLINE windowWidthL #-}

windowHeightL :: Lens' Context Int
windowHeightL f x = fmap (\y -> x{windowHeight = y}) (f $ windowHeight x)
{-# INLINE windowHeightL #-}

ctxBorderStyleL :: Lens' Context BorderStyle
ctxBorderStyleL f x = fmap (\y -> x{ctxBorderStyle = y}) (f $ ctxBorderStyle x)
{-# INLINE ctxBorderStyleL #-}

ctxAttrMapL :: Lens' Context AttrMap
ctxAttrMapL f x = fmap (\y -> x{ctxAttrMap = y}) (f $ ctxAttrMap x)
{-# INLINE ctxAttrMapL #-}

ctxDynBordersL :: Lens' Context Bool
ctxDynBordersL f x = fmap (\y -> x{ctxDynBorders = y}) (f $ ctxDynBorders x)
{-# INLINE ctxDynBordersL #-}
