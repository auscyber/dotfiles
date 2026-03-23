module StackSetLens (currentLens, visibleLens, hiddenLens, floatingLens, tagLens, layoutLens, stackLens, focusLens, upLens, downLens, workspaceLens, screenLens, screenDetailLens) where

import qualified Data.Map as M
import Lens.Micro
import XMonad
import qualified XMonad.StackSet as W

-- StackSet lenses

currentLens :: Lens' (W.StackSet i l a sid sd) (W.Screen i l a sid sd)
currentLens f s@W.StackSet{W.current = current'} = (\x -> s{W.current = x}) <$> f current'

visibleLens :: Lens' (W.StackSet i l a sid sd) [W.Screen i l a sid sd]
visibleLens f s@W.StackSet{W.visible = visible'} = (\x -> s{W.visible = x}) <$> f visible'

hiddenLens :: Lens' (W.StackSet i l a sid sd) [W.Workspace i l a]
hiddenLens f s@W.StackSet{W.hidden = hidden'} = (\x -> s{W.hidden = x}) <$> f hidden'

floatingLens :: Lens' (W.StackSet i l a sid sd) (M.Map a W.RationalRect)
floatingLens f s@W.StackSet{W.floating = floating'} = (\x -> s{W.floating = x}) <$> f floating'

-- Screen Lens
workspaceLens :: Lens' (W.Screen i l a sid sd) (W.Workspace i l a)
workspaceLens f s@W.Screen{W.workspace = workspace'} = (\x -> s{W.workspace = x}) <$> f workspace'

screenLens :: Lens' (W.Screen i l a sid sd) sid
screenLens f s@W.Screen{W.screen = screen'} = (\x -> s{W.screen = x}) <$> f screen'

screenDetailLens :: Lens' (W.Screen i l a sid sd) sd
screenDetailLens f s@W.Screen{W.screenDetail = screenDetail'} = (\x -> s{W.screenDetail = x}) <$> f screenDetail'

-- Workspace lenses

tagLens :: Lens' (W.Workspace i l a) i
tagLens f w@W.Workspace{W.tag = tag'} = (\x -> w{W.tag = x}) <$> f tag'

layoutLens :: Lens' (W.Workspace i l a) l
layoutLens f w@W.Workspace{W.layout = layout'} = (\x -> w{W.layout = x}) <$> f layout'

stackLens :: Lens' (W.Workspace i l a) (Maybe (W.Stack a))
stackLens f w@W.Workspace{W.stack = stack'} = (\x -> w{W.stack = x}) <$> f stack'

-- Stack Lens

focusLens :: Lens' (W.Stack a) a
focusLens f s@W.Stack{W.focus = focus'} = (\x -> s{W.focus = x}) <$> f focus'

upLens :: Lens' (W.Stack a) [a]
upLens f s@W.Stack{W.up = up'} = (\x -> s{W.up = x}) <$> f up'

downLens :: Lens' (W.Stack a) [a]
downLens f s@W.Stack{W.down = down'} = (\x -> s{W.down = x}) <$> f down'
