{-# LANGUAGE TemplateHaskell #-} -- ^ Aktivace jazykoveho rozsireni TH. Potrebne pro quickCheckAll

module Tests where

import Test.QuickCheck
import Test.QuickCheck.All

import Ciphers

main = $quickCheckAll >> return ()

prop_invCaesar n xs = (decryptCaesar n . encryptCaesar n) xs == xs

prop_invViginere k xs = decryptViginere k (encryptViginere k xs) == xs

