module AusPrompt where

import XMonad
import XMonad.Prompt

newtype AusPrompt = AusPrompt String

instance XPrompt AusPrompt where
    showXPrompt (AusPrompt "launch") = "Run: "
    showXPrompt (AusPrompt _) = ""
