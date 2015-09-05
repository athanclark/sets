module Data.IntMap.Data where

import qualified Data.IntMap as I


imapTo :: Int -> I.IntMap Int
imapTo n = I.fromList $ [1..n] `zip` [1..n]

imap1 :: I.IntMap Int
imap1 = imapTo 10

imap2 :: I.IntMap Int
imap2 = imapTo 20

imap3 :: I.IntMap Int
imap3 = imapTo 30

imap4 :: I.IntMap Int
imap4 = imapTo 40

imap5 :: I.IntMap Int
imap5 = imapTo 50
