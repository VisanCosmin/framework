{-# LANGUAGE OverloadedStrings          #-}

import Application ()
import Foundation
import Yesod
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Network.Wai.Middleware.Cors


openConnectionCount = 10

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "forum-db.db3" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
    warp 3000 $ App pool
