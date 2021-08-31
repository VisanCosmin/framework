{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Foundation where

import Yesod 
import Database.Persist.Sqlite
import Data.Text
import Data.Aeson
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

|]


data App = App ConnectionPool
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        App pool <- getYesod
        runSqlPool action pool

mkYesodData "App" $(parseRoutesFile "routes.yesodroutes")

instance Yesod App where
    yesodMiddleware handler = do 
        defaultYesodMiddleware handler
    makeSessionBackend _ =
        laxSameSiteSessions (fmap Just $ defaultClientSessionBackend 60 "client_session_key.aes")

    defaultLayout contents = do
        PageContent title headTags bodyTags <- widgetToPageContent contents
        mmsg <- getMessage
        withUrlRenderer [hamlet|
            $doctype 5
            <html>
                <head>
                    <title>#{title}
                    ^{headTags}
                <body>
                    <div id="app">
                        ^{bodyTags}

                    <script type="module">
                        function sendRequest(action){
                            fetch('/',{
                                method: 'POST',
                                headers: {
                                    'Accept': 'application/json, text/plain, */*',
                                    'Content-Type': 'application/json'
                                },
                                body: JSON.stringify({ 
                                    requestState: window.data, 
                                    reqeustAction: action
                                }) 
                            })
                            .then(function(res){ return res.json(); })
                            .then(function(res){
                                window.data = res.resultState;
                                document.getElementById('app').innerHTML = res.resultHtml;
                            });
                        }
                        window.sendRequest = sendRequest;
        |]


--                         import { innerHTML } from 'https://diffhtml.org/es';
--                         innerHTML(document.getElementById('app'), res.resultHtml);

