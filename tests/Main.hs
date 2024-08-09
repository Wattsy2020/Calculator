{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Data.Maybe (mapMaybe)
import NaturalCalculator
import Test.Hspec
import Test.QuickCheck
import Data.Function (on)

testCases :: [(String, Int)]
testCases =
  [ ("1+3*2", 7),
    ("3*2+1", 7),
    ("1+3*2+1", 8),
    ("11+3*(2+1)+2", 22),
    ("1+3*(1+3*2)", 22),
    ("1+3*(1+3*2)+1", 23),
    ("1+3*(1+3*2+1)+1", 26),
    ("0+(11+3*(2+1+1)+2)+100", 125),
    ("(1)+1", 2),
    ("(11+3*(2+1-1)-2)+100", 115),
    ("-4*2", -8)
  ]

testPasses :: (String, Int) -> Bool
testPasses (exprStr, result) = eval exprStr == Right result

getFailedTest :: (String, Int) -> Maybe (String, Int)
getFailedTest (exprStr, result)
  | testPasses (exprStr, result) = Nothing
  | otherwise = Just (show $ fmap parseExpression (lexExpression exprStr :: Either ParseError [Token Int]), result)

instance Arbitrary Op where
  arbitrary :: Gen Op
  arbitrary = elements [ Plus, Multiply, Minus]

instance (Arbitrary a, Num a) => Arbitrary (Expression a) where
  arbitrary :: Gen (Expression a)
  arbitrary = frequency [
    (1, Value <$> (arbitrary :: Gen a)),
    (1, Expression <$> (arbitrary :: Gen (Expression a)) <*> (arbitrary :: Gen Op) <*> (arbitrary :: Gen (Expression a)))]

  shrink :: Num a => Expression a -> [Expression a]
  shrink (Value val) = map Value $ shrink val
  shrink expr@(Expression (Value leftVal) op (Value rightVal)) = [Value $ evalExpression expr]
  shrink expr@(Expression leftExpr op rightVal@(Value _)) = [
    leftExpr,
    rightVal,
    Expression evalLeft op rightVal] 
    ++ map (\shrunkExpr -> Expression shrunkExpr op rightVal) shrunkLeft
    where
      evalLeft = Value $ evalExpression leftExpr
      shrunkLeft = shrink leftExpr
  shrink expr@(Expression leftVal@(Value _) op rightExpr) = [
    leftVal,
    rightExpr,
    Expression leftVal op evalRight] 
    ++ map (Expression leftVal op) shrunkRight
    where
      evalRight = Value $ evalExpression rightExpr
      shrunkRight = shrink rightExpr
  shrink (Expression leftExpr op rightExpr) = [
      leftExpr, 
      rightExpr, 
      Expression evalLeft op evalRight,
      Expression evalLeft op rightExpr,
      Expression leftExpr op evalRight]
      ++ map (\shrunkExpr -> Expression shrunkExpr op rightExpr) shrunkLeft
      ++ map (Expression leftExpr op) shrunkRight
    where
      evalLeft = Value $ evalExpression leftExpr
      evalRight = Value $ evalExpression rightExpr
      shrunkLeft = shrink leftExpr
      shrunkRight = shrink rightExpr

prop_serializeroundtrip :: (Show a, Eq a, Num a) => Expression a -> Bool
prop_serializeroundtrip expr = case readExpression $ serializeExpression expr of
  Left _ -> False
  Right readExpr -> ((==) `on` evalExpression) expr readExpr

main :: IO ()
main = hspec $ do
  describe "Calculator" $ do
    it "Passes Manual Test cases" $ mapMaybe getFailedTest testCases `shouldBe` []
    
    it "Passes Property Test cases" $ property (prop_serializeroundtrip :: Expression Int -> Bool)
