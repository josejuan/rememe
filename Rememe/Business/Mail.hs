{-# LANGUAGE OverloadedStrings #-}
module Rememe.Business.Mail (notify) where

import Data.Text
import qualified Data.Text.Lazy as Lt
import qualified Data.Text.Lazy.Encoding as Lt
import Network.Mail.Mime

addr = Address Nothing . pack


notify from to subject body = do

  let msg = Mail { mailFrom = Address Nothing from
                 , mailTo = [Address Nothing to]
                 , mailCc = []
                 , mailBcc = []
                 , mailHeaders = [("Subject", subject)]
                 , mailParts = [[Part (pack "text/plain; charset=utf-8") QuotedPrintableText Nothing [] $ Lt.encodeUtf8 $ Lt.fromStrict body]]
                 }

  renderSendMail msg
