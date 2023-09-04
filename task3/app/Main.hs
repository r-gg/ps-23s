module Main where

import Lens.Micro ((^.))
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath.Posix (takeDirectory)
import TextEditor
  ( Name (Edit),
    State (State),
    editor,
    editorState,
    getEditContents,
    open,
  )

main :: IO ()
main = do
  -- get filepath as input
  args <- getArgs

  case args of
    -- if filepath is empty --> open with initial state
    [] ->
      do
        editedFile <- TextEditor.open (TextEditor.State (editor Edit Nothing ""))
        save "document.txt" editedFile

    -- if filepath was given --> open with file content
    _ ->
      do
        let fileName = head args
        file <- readFile fileName
        editedFile <- TextEditor.open (TextEditor.State (editor Edit Nothing file))
        save fileName editedFile

-- save edited document
save :: String -> TextEditor.State -> IO ()
save filePath state = do
  createDirectoryIfMissing True $ takeDirectory filePath
  writeFile filePath (unlines (TextEditor.getEditContents (state ^. TextEditor.editorState)))