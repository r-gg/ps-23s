module Main where

import Lens.Micro
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)
import TextEditor

main :: IO ()
main = do
  -- get filepath as input
  putStrLn "Please Input Filepath:"
  fileName <- getLine

  case fileName of
    -- if filepath is empty --> open with initial state
    [] ->
      do
        editedFile <- TextEditor.open (TextEditor.State (editor Edit Nothing ""))
        save "document.txt" editedFile

    -- if filepath was given --> open with file content
    _ ->
      do
        file <- readFile fileName
        editedFile <- TextEditor.open (TextEditor.State (editor Edit Nothing file))
        save fileName editedFile

-- save edited document
save :: String -> TextEditor.State -> IO ()
save filePath state = do
  createDirectoryIfMissing True $ takeDirectory filePath
  writeFile filePath (head (TextEditor.getEditContents (state ^. TextEditor.editorState)))