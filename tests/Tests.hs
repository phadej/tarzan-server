import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Text.Tarzan.Regex (RE)
import qualified Text.Tarzan.Regex as RE

import Control.Applicative
import Text.Tarzan.Pretty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProps]

instance Arbitrary RE where
  arbitrary = sized arbitrary'
    where arbitrary' n
            | n <= 0     = frequency [ (4, return RE.eps)
                                     , (4, return RE.anychar)
                                     , (4, RE.char <$> (arbitrary `suchThat` ((<=0xffff) . fromEnum)))
                                     , (1, return RE.empty)
                                     , (1, return RE.anything)
                                     ]
            | otherwise  = oneof [ arbitrary' 0
                                 , RE.append <$> arbitrary'' <*> arbitrary''
                                 , RE.union <$> arbitrary'' <*> arbitrary''
                                 , RE.kleene <$> arbitrary''
                                 ]
            where arbitrary'' = arbitrary' (n `div` 2)

showParseRE :: RE -> Bool
showParseRE re = Right re == RE.parseRe (pretty re)

qcProps :: TestTree
qcProps = testGroup "QuickCheck properties"
  [ QC.testProperty "pretty . parse == id" showParseRE
  ]
