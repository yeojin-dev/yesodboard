{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Home where

import Import
import System.Directory
import System.FilePath (joinPath)
import System.IO.Unsafe
import Text.Julius (RawJS (..))
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

-- Define our data that will be used for creating the form.
data FileForm = FileForm
  { fileInfo :: FileInfo,
    fileDescription :: Text
  }

getStaticHtmlFilePath :: String -> IO FilePath
getStaticHtmlFilePath filePath =
  getCurrentDirectory >>= (\basePath -> return (System.FilePath.joinPath [basePath ++ "/templates" ++ filePath]))

getHomeR :: Handler Html
getHomeR = do
  let indexFullFilePath = getStaticHtmlFilePath "/index.html"
  sendFile "text/html" (unsafePerformIO indexFullFilePath)

postHomeR :: Handler Html
postHomeR = do
  ((result, formWidget), formEnctype) <- runFormPost sampleForm
  let handlerName = "postHomeR" :: Text
      submission = case result of
        FormSuccess res -> Just res
        _ -> Nothing

  defaultLayout $ do
    let (commentFormId, commentTextareaId, commentListId) = commentIds
    aDomId <- newIdent
    setTitle "Welcome To Yesod!"
    $(widgetFile "homepage")

sampleForm :: Form FileForm
sampleForm =
  renderBootstrap3 BootstrapBasicForm $
    FileForm
      <$> fileAFormReq "Choose a file"
      <*> areq textField textSettings Nothing
  where
    -- Add attributes like the placeholder and CSS classes.
    textSettings =
      FieldSettings
        { fsLabel = "What's on the file?",
          fsTooltip = Nothing,
          fsId = Nothing,
          fsName = Nothing,
          fsAttrs =
            [ ("class", "form-control"),
              ("placeholder", "File description")
            ]
        }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
