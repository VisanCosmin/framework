{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Home where

import Foundation
import Yesod 
import Database.Persist.Sql
import Data.Aeson hiding (Result)
import GHC.Generics
import Text.Hamlet (shamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)


data Request = Request {
    requestState :: HomeState,
    reqeustAction :: HomeAction
} deriving (Generic, Show)

instance ToJSON Request
instance FromJSON Request



-----

data HomeState = HomeState Int deriving (Show,Read,Generic)
data HomeAction = HomeIncrement | HomeDecrement deriving (Show,Read,Generic)

instance ToJSON HomeState
instance FromJSON HomeState

instance ToJSON HomeAction
instance FromJSON HomeAction

updateState :: HomeState -> HomeAction -> Handler HomeState 
updateState (HomeState n) HomeIncrement = return (HomeState $ n+1)
updateState (HomeState n) HomeDecrement = return (HomeState $ n-1)

viewState :: HomeState -> Html 
viewState (HomeState n) = [shamlet|
        <button onClick="sendRequest('HomeIncrement')" class="sadasda">Increment
        <p id="Dasdada">Counter : #{show n}
        <button onClick="sendRequest('HomeDecrement')">Decrement
    |]

-----

getHomeR :: Handler Html
getHomeR = do 
    let initialState = HomeState 0 

    let initialWidget :: Widget
        initialWidget = toWidget $ viewState initialState

    defaultLayout $ do
        setTitle "Home"
        [whamlet|
            ^{initialWidget}
        |]
        toWidgetBody [julius|
            window.data = 0;
        |]


----
data Result = Result {
    resultState :: HomeState,
    resultHtml :: String
} deriving (Generic, Show)

instance ToJSON Result
instance FromJSON Result

----

postHomeR :: Handler Value
postHomeR = do 
    (Request state action) <- requireInsecureJsonBody
    newState <- updateState state action
    let newHtml = renderHtml $ viewState newState

    returnJson $ Result newState newHtml