{-|
Module      : Heist.Splices.I18n
Description : i18n splice for heist
-}
{-# LANGUAGE OverloadedStrings #-}
module Heist.Splices.I18n (
    i18nTag,
    i18nTranslate,
    i18nDumb,
    i18nGenPOT
) where

import Heist ( getParamNode, HeistT )
import qualified Heist.Interpreted as I
import Control.Monad.Trans ( MonadTrans(lift) )
import Text.XmlHtml ( childNodes, Node(TextNode), getAttribute )
import qualified Data.Text as T (Text, concat, lines, strip, empty)
import Data.Text.I18n
    ( Msgstr, gettext, localize, Msgid(..), Locale(Locale), L10n, Context, withContext, CtxMap )
import qualified Data.Map.Strict as Map
import Data.IORef ( modifyIORef, IORef, readIORef, writeIORef )
import Control.Lens ( (&), _Just, (?~), At(at) )

-- |Defult tag for i18n
i18nTag::T.Text
i18nTag = "i18n"

-- |Translate content between tag
--
-- For Example:
--
-- @
--    (l10n, _) <- getL10n "./locale"
--    i18nTag ## i18nTranslate l10n "en"
-- @
--
i18nTranslate :: L10n -> T.Text -> HeistT n IO [Node]
i18nTranslate l10n lo = do
    node <- getParamNode
    let TextNode a = head $ childNodes node
    let tempid = T.concat $ map T.strip $ T.lines a
    let result = localize l10n (Locale lo) (withContext (getAttribute "ctxt" node) (gettext tempid))
    return [TextNode result]

-- |Keep the content between tag and drop the tag
i18nDumb :: Monad n => I.Splice n
i18nDumb = do
    node <- getParamNode
    let TextNode a = head $ childNodes node
    I.runChildren
    return [TextNode a]

-- |Use content between tag as Msgid, and generate CtxMap
i18nGenPOT :: IORef CtxMap -> HeistT n IO [Node]
i18nGenPOT refCtxMap = do
    node <- getParamNode
    let TextNode a = head $ childNodes node
    let msgCtx = getAttribute "ctxt" node
    let msgId = Msgid $ T.concat $ map T.strip $ T.lines a
    lift $ do
        old_ctx_map <- readIORef refCtxMap
        if Map.member msgCtx old_ctx_map then
            -- old context
            writeIORef refCtxMap $
                    old_ctx_map & at msgCtx . _Just . at msgId ?~ []  
        else
            -- a brand new context
            writeIORef refCtxMap $
                    old_ctx_map & at msgCtx ?~ Map.fromList [(msgId, [])]
            
    return [node]

