{-# LANGUAGE OverloadedStrings #-}

module W6 where

import Regexp

optionally :: Regexp -> Regexp
optionally r = r <|> one

starPlus :: Regexp -> Regexp
starPlus r = r <.> star r


