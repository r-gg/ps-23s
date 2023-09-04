{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Redundant brace" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module TextEditor where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util (fg, on)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( hLimit,
    str,
    vLimit,
    viewport,
  )
import qualified Brick.Widgets.Core as C
import Control.Lens (element)
import Data.List (find, findIndices, isPrefixOf, tails)
import Data.Maybe (isJust)
import qualified Data.Text.Zipper as Z hiding (textZipper)
import qualified Data.Text.Zipper.Generic as Z
import qualified Data.Text.Zipper.Generic.Words as Z
import Data.Tuple (swap)
import Graphics.Vty (Event (..), Key (..), Modifier (..))
import qualified Graphics.Vty as V
import Lens.Micro (to, (%~), (&), (^.), (^?), _1, _2)
import Lens.Micro.Mtl (zoom)
import Lens.Micro.TH (makeLenses)

data Name = Edit | SyntaxHighlight deriving (Ord, Show, Eq)

data Editor t n = Editor
  { editContents :: Z.TextZipper t,
    editorName :: n
  }

instance C.Named (Editor t n) n where
  getName = editorName

newtype State = State {_editorState :: Editor String Name}

Lens.Micro.TH.makeLenses ''State
T.suffixLenses ''Editor

-- Opens the editor
open :: State -> IO State
open = M.defaultMain theApp

-- Construct an editor
editor :: Z.GenericTextZipper a => n -> Maybe Int -> a -> Editor a n
editor name limit s = Editor (Z.textZipper (Z.lines s) limit) name

-- Define the app and its attributes
theApp :: M.App State e Name
theApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = const $ M.showCursorNamed Edit,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

-- Look of the editor
theMap :: A.AttrMap
theMap = A.attrMap V.defAttr [(editAttr, V.white `on` V.black), (braceAttr, fg V.red), (msgAttr, fg V.blue), (markAttr, fg V.green)]

-- Event handler of the editor
appEvent :: T.BrickEvent Name e -> T.EventM Name State ()
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
appEvent ev = zoom editorState $ handleEditorEvent ev

-- handles the events of the editor
handleEditorEvent ::
  (Eq n, Eq t, Z.GenericTextZipper t) =>
  T.BrickEvent n e ->
  T.EventM n (Editor t n) ()
handleEditorEvent e = do
  ed <- T.get
  let f = case e of
        T.VtyEvent ev -> handleVtyEvent ev
        T.MouseDown n _ _ (T.Location pos) | n == C.getName ed -> Z.moveCursorClosest (swap pos)
        _ -> id

      handleVtyEvent ev = case ev of
        EvKey (KChar 'a') [MCtrl] -> Z.gotoBOL
        EvKey (KChar 'e') [MCtrl] -> Z.gotoEOL
        EvKey (KChar 'd') [MCtrl] -> Z.deleteChar
        EvKey (KChar 'd') [MMeta] -> Z.deleteWord
        EvKey (KChar 'k') [MCtrl] -> Z.killToEOL
        EvKey (KChar 'u') [MCtrl] -> Z.killToBOL
        EvKey KEnter [] -> Z.breakLine
        EvKey KDel [] -> Z.deleteChar
        EvKey (KChar c) [] | c /= '\t' -> Z.insertChar c
        EvKey KUp [] -> Z.moveUp
        EvKey KDown [] -> Z.moveDown
        EvKey KLeft [] -> Z.moveLeft
        EvKey KRight [] -> Z.moveRight
        EvKey (KChar 'b') [MCtrl] -> Z.moveLeft
        EvKey (KChar 'f') [MCtrl] -> Z.moveRight
        EvKey (KChar 'b') [MMeta] -> Z.moveWordLeft
        EvKey (KChar 'f') [MMeta] -> Z.moveWordRight
        EvKey KBS [] -> Z.deletePrevChar
        EvKey (KChar 't') [MCtrl] -> Z.transposeChars
        EvKey KHome [] -> Z.gotoBOL
        EvKey KEnd [] -> Z.gotoEOL
        EvKey (KChar '<') [MMeta] -> Z.gotoBOF
        EvKey (KChar '>') [MMeta] -> Z.gotoEOF
        _ -> id

  T.put $ applyEdit f ed

-- rendering of the editor
drawUI :: State -> [T.Widget Name]
drawUI st = [ui]
  where
    ui = C.center $ hLimit 80 $ vLimit 24 e
    e = render (st ^. editorState)

-- calls the renderEditor function
render :: Editor String Name -> T.Widget Name
render e
  | isJust (lookup (-1) (bracePairs (unlines (getEditContents e)))) = C.vBox [messagesViewport "Unbalanced braces", renderEditor (syntaxHighlight e) True e]
  | otherwise = C.vBox [messagesViewport "", renderEditor (syntaxHighlight e) True e]
  where
    messagesViewport msg = vLimit 1 $ viewport SyntaxHighlight T.Vertical (body msg)
    body msg = C.withDefAttr msgAttr $ C.hBox [errorWidget msg]
    errorWidget msg = str $ show msg

-- Turn an editor state value into a widget.
renderEditor :: (Ord n, Show n) => ([String] -> T.Widget n) -> Bool -> Editor String n -> T.Widget n
renderEditor draw foc e =
  C.withAttr (if foc then editFocusedAttr else editAttr) $
    limit $
      viewport (e ^. editorNameL) T.Both $
        (if foc then C.showCursor (e ^. editorNameL) cursorLoc else id) $
          C.visibleRegion cursorLoc (atCharWidth, 1) $
            draw $
              getEditContents e
  where
    cp = Z.cursorPosition z
    z = e ^. editContentsL
    toLeft = Z.take (cp ^. _2) (Z.currentLine z)
    cursorLoc = T.Location (C.textWidth toLeft, cp ^. _1)
    limit = maybe id vLimit (e ^. editContentsL . to Z.getLineLimit)
    atChar = charAtCursor $ e ^. editContentsL
    atCharWidth = maybe 1 C.textWidth atChar

-- add syntax highlighting
syntaxHighlight :: Editor String Name -> [String] -> T.Widget n
syntaxHighlight e = syntaxHighlight' e 0

-- add syntax highlighting to a row
syntaxHighlight' :: Editor String Name -> Int -> [String] -> T.Widget n
syntaxHighlight' e rowPos (s : ss) = syntaxHighlight'' e rowPos 0 s C.<=> syntaxHighlight' e (rowPos + 1) ss
syntaxHighlight' _ _ [] = str "\n"

-- add syntax highlighting to a column
syntaxHighlight'' :: Editor String Name -> Int -> Int -> String -> T.Widget n
syntaxHighlight'' e rowPos colPos (c : cs) = syntaxHighlight''' e rowPos colPos c C.<+> syntaxHighlight'' e rowPos (colPos + 1) cs
syntaxHighlight'' _ _ _ [] = str "\n"

-- add syntax highlighting to a char
syntaxHighlight''' :: Editor String Name -> Int -> Int -> Char -> T.Widget n
syntaxHighlight''' e rowPos colPos c
  | positionToStringPosition e (getCursorPosition e) == matchingOpeningbraceForPosition e rowPos colPos = C.withAttr braceAttr (str [c])
  | positionToStringPosition e (getCursorPosition e) == matchingClosingbraceForPosition e rowPos colPos = C.withAttr braceAttr (str [c])
  | isPartOfMatchingWords e rowPos colPos = C.withAttr markAttr (str [c])
  | otherwise = str [c]

-- count of rows
rows :: Editor String Name -> Int
rows e = length (Z.getText $ e ^. editContentsL)

-- count of columns
cols :: Editor String Name -> Int -> Int
cols e row = length (currentPositionRow e row)

-- convert the position (i,j) to a position in the flattened string
positionToStringPosition :: Editor String Name -> (Int, Int) -> Int
positionToStringPosition e (i, j) = sum [cols e r + 1 | r <- [0 .. i - 1]] + j

-- is the currentposition the cursor position
isCurrentCursorPosition :: Editor String Name -> Int -> Int -> Bool
isCurrentCursorPosition e rowPos colPos = fst (getCursorPosition e) == rowPos && snd (getCursorPosition e) == colPos

-- get row string with a index
currentPositionRowMaybe :: Editor String Name -> Int -> Maybe String
currentPositionRowMaybe e rowPos = Z.getText (e ^. editContentsL) ^? element rowPos

-- convert the maybe datatype
currentPositionRow :: Editor String Name -> Int -> String
currentPositionRow e rowPos =
  case currentPositionRowMaybe e rowPos of
    (Just r) -> r
    Nothing -> []

-- search in braces list for the matching opening brace
matchingOpeningbraceForPosition :: Editor String Name -> Int -> Int -> Int
matchingOpeningbraceForPosition e rowPos colPos =
  case lookup (positionToStringPosition e (rowPos, colPos)) (bracePairs (unlines (getEditContents e))) of
    (Just p) -> p
    Nothing -> -1

-- search in braces list for the matching closing brace
matchingClosingbraceForPosition :: Editor String Name -> Int -> Int -> Int
matchingClosingbraceForPosition e rowPos colPos =
  case searchSndElement (positionToStringPosition e (rowPos, colPos)) (bracePairs (unlines (getEditContents e))) of
    (Just p) -> p
    Nothing -> -1
  where
    searchSndElement :: Eq b => b -> [(a, b)] -> Maybe a
    searchSndElement a = fmap fst . find ((== a) . snd)

-- pairs of corresponding braces
-- unbalanced braces are marked with (-1, -1)
bracePairs :: String -> [(Int, Int)]
bracePairs = go 0 []
  where
    go _ (_ : _) [] = [(-1, -1)]
    go j acc ('(' : cs) = go (j + 1) (j : acc) cs
    go _ [] (')' : _) = [(-1, -1)]
    go j (i : is) (')' : cs) = (i, j) : go (j + 1) is cs
    go j acc (_ : cs) = go (j + 1) acc cs
    go _ _ [] = []

-- check if row col position is part of matching words
isPartOfMatchingWords :: Editor String Name -> Int -> Int -> Bool
isPartOfMatchingWords e r c = or [positionToStringPosition e (r, c) <? p | p <- matchingWordIndices e]

-- start and end indices of matching words
matchingWordIndices :: Editor String Name -> [(Int, Int)]
matchingWordIndices e = [(p, p + length wordToFind - 1) | p <- findPositionOfWord (unlines (getEditContents e)) wordToFind]
  where
    wordToFind = currentWord (unlines (getEditContents e)) (positionToStringPosition e (getCursorPosition e))

-- get word at current position
currentWord :: String -> Int -> String
currentWord s i
  | take 1 (drop i s) == [' '] = [' ']
  | take 1 (drop i s) == ['\n'] = ['\n']
  | take 1 (drop i s) == ['('] = ['(']
  | take 1 (drop i s) == [')'] = [')']
  | otherwise = charBefore s i ++ charAfter s i

-- get chars before position
charBefore :: String -> Int -> String
charBefore s i = reverse (takeWhile (\c -> (c /= ' ') && (c /= '\n') && (c /= '(') && (c /= ')')) (reverse (take i s)))

-- get chars after position
charAfter :: String -> Int -> String
charAfter s i = takeWhile (\c -> (c /= ' ') && (c /= '\n') && (c /= '(') && (c /= ')')) (take (length s) (drop i s))

-- gets the position of a word
findPositionOfWord :: String -> String -> [Int]
findPositionOfWord s w = findIndices (w `isPrefixOf`) (tails s)

-- Apply an editing operation to the editor's contents
applyEdit :: (Z.TextZipper t -> Z.TextZipper t) -> Editor t n -> Editor t n
applyEdit f e = e & editContentsL %~ f

-- The attribute assigned to the editor when it does not have focus
editAttr :: A.AttrName
editAttr = A.attrName "edit"

-- The attribute assigned to the editor when it has focus
editFocusedAttr :: A.AttrName
editFocusedAttr = editAttr <> A.attrName "focused"

-- The attribute assigned to a characted when it is ( or )
braceAttr :: A.AttrName
braceAttr = A.attrName "brace"

-- The attribute assigned to a characted when it is msg
msgAttr :: A.AttrName
msgAttr = A.attrName "msg"

-- The attribute assigned to a characted when it is marked
markAttr :: A.AttrName
markAttr = A.attrName "mark"

-- Get the contents of the editor.
getEditContents :: Monoid t => Editor t n -> [t]
getEditContents e = Z.getText $ e ^. editContentsL

-- Get the cursor position of the editor (row, column)
getCursorPosition :: Editor t n -> (Int, Int)
getCursorPosition e = Z.cursorPosition $ e ^. editContentsL

-- Get character at the cursor
charAtCursor :: (Z.GenericTextZipper t) => Z.TextZipper t -> Maybe t
charAtCursor z =
  let col = snd $ Z.cursorPosition z
      curLine = Z.currentLine z
      toRight = Z.drop col curLine
   in if Z.length toRight > 0
        then Just $ Z.take 1 toRight
        else Nothing

-- check if number is between a pair of numbers
(<?) :: Ord a => a -> (a, a) -> Bool
(<?) x (mi, ma) = x >= mi && x <= ma