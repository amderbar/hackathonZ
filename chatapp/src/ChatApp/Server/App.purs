module ChatApp.Server.App where

import Prelude
import Data.Maybe                           (Maybe(..), fromMaybe)
import Effect.Exception                     (Error)
import Effect.Exception                   as Error
import Node.Express.App                     (App)
import Node.Express.App                   as App
import Node.Express.Handler                 (Handler)
import Node.Express.Handler               as Handler
import Node.Express.Middleware.CookieParser (cookieParser)
import Node.Express.Middleware.Static       (static)
import Node.Express.Response              as Response
import Node.BodyParser                    as BodyParser
import Node.Morgan                        as Morgan
import Simple.JSON                        as Json

error' :: forall a. Json.WriteForeign a => a -> Error
error' = Error.error <<< Json.writeJSON

message' :: forall a. Json.ReadForeign a => Error -> Maybe a
message' = Json.readJSON_ <<< Error.message

app :: App
app = do
  env <- App.getProp "env"
  App.useExternal (Morgan.logger "dev")
  App.useExternal BodyParser.json
  App.use         cookieParser
  App.use         (static "public")
  App.use         notFoundErrorHandler
  App.useOnError case ((==) "development") <$> env of
    (Just true) -> errorHandlerDev
    _           -> errorHandlerProd
  where
    notFoundErrorHandler :: Handler
    notFoundErrorHandler = do
      Handler.nextThrow $ error'
        { message: "Not Found"
        , status: 404
        }

    errorHandlerDev :: Error -> Handler
    errorHandlerDev err = do
      let errMsg = errorPerser err
      Response.setStatus $ errMsg.status
      Response.sendJson
        { message: errMsg.message
        , error  : Error.stack err
        }

    errorHandlerProd :: Error -> Handler
    errorHandlerProd err = do
      let errMsg = errorPerser err
      Response.setStatus $ errMsg.status
      Response.sendJson
        { message: errMsg.message }

    errorPerser :: Error -> { status :: Int, message :: String }
    errorPerser err =
      let err500 =
            { status : 500
            , message: "Internal Server Error"
            }
       in fromMaybe err500 (message' err)
