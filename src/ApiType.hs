{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import           Data.Text
-- import           Data.Time                      ( UTCTime )
import           Data.Time.Calendar             ( Day
                                                , fromGregorian
                                                )
import           Servant.API
import           Data.Aeson
import           GHC.Generics

type RootEndpoint = Get '[JSON] User

type UserApi = "users" :> Get '[JSON] [User]

data SortBy = Age | Name

data User = User {
    name :: String
    , age :: Int
    , email :: String
    , registration_date :: Day
} deriving (Eq, Show, Generic)

instance ToJSON User

users1 :: [User]
users1 =
    [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)
    , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
    ]
