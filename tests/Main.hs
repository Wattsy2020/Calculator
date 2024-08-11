{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Data.Maybe (mapMaybe)
import NaturalCalculator
import Expression
import Parser
import Evaluator
import Test.Hspec
import Test.QuickCheck
import Data.Function (on)
import Data.Foldable (toList)
import Data.Decimal
import Data.Word
import FunctorUtils ( ($>) )

testCases :: [(String, Double)]
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
    -- todo: fix these test cases
    -- brackets should continue to parse numbers after them
    --("1+(2)*3", 7),
    -- subtraction and addition are both left associative, this should be parsed as (1-2)+3
    --("1-2+3", 4)
  ]

testPasses :: (String, Double) -> Bool
testPasses (exprStr, result) = eval exprStr == Right (Right result)

getFailedTest :: (String, Double) -> Maybe (String, Double)
getFailedTest (exprStr, result)
  | testPasses (exprStr, result) = Nothing
  | otherwise = Just (show $ fmap parseExpression (lexExpression exprStr :: Either ParseError [Token]), result)

instance Arbitrary Op where
  arbitrary :: Gen Op
  arbitrary = elements [ Add, Subtract, Multiply, Divide ]

instance (Arbitrary a, Eq a, Fractional a) => Arbitrary (Expression a) where
  arbitrary :: Gen (Expression a)
  arbitrary = frequency [
    (1, Value <$> (arbitrary :: Gen a)),
    (1, Expression <$> (arbitrary :: Gen (Expression a)) <*> (arbitrary :: Gen Op) <*> (arbitrary :: Gen (Expression a)))]

  shrink :: Fractional a => Expression a -> [Expression a]
  shrink (Value val) = map Value $ shrink val
  shrink expr@(Expression (Value leftVal) op (Value rightVal)) = toList $ Value <$> evalExpression expr
  shrink expr@(Expression leftExpr op rightVal@(Value _)) = [
    leftExpr,
    rightVal]
    ++ toList (Expression <$> evalLeft $> op $> rightVal)
    ++ map (\shrunkExpr -> Expression shrunkExpr op rightVal) shrunkLeft
    where
      evalLeft = Value <$> evalExpression leftExpr
      shrunkLeft = shrink leftExpr
  shrink expr@(Expression leftVal@(Value _) op rightExpr) = [
    leftVal,
    rightExpr]
    ++ toList (Expression leftVal op <$> evalRight)
    ++ map (Expression leftVal op) shrunkRight
    where
      evalRight = Value <$> evalExpression rightExpr
      shrunkRight = shrink rightExpr
  shrink (Expression leftExpr op rightExpr) = [
      leftExpr, 
      rightExpr]
      ++ toList (Expression <$> evalLeft $> op <*> evalRight)
      ++ toList (Expression <$> evalLeft $> op $> rightExpr)
      ++ toList (Expression leftExpr op <$> evalRight)
      ++ map (\shrunkExpr -> Expression shrunkExpr op rightExpr) shrunkLeft
      ++ map (Expression leftExpr op) shrunkRight
    where
      evalLeft = Value <$> evalExpression leftExpr
      evalRight = Value <$> evalExpression rightExpr
      shrunkLeft = shrink leftExpr
      shrunkRight = shrink rightExpr

instance (Integral a) => Arbitrary (DecimalRaw a) where
  arbitrary :: Gen (DecimalRaw a)
  arbitrary = realFracToDecimal <$> (arbitrary :: Gen Word8) <*> (arbitrary :: Gen Double)

prop_serializeroundtrip :: (Show a, Read a, Eq a, Fractional a) => Expression a -> Bool
prop_serializeroundtrip expr = case readExpression $ serializeExpression expr of
  Left _ -> False
  Right readExpr -> ((==) `on` evalExpression) expr readExpr

main :: IO ()
main = hspec $ do
  describe "Calculator" $ do
    it "Passes Manual Test cases" $ mapMaybe getFailedTest testCases `shouldBe` []

    it "Handles incorrectly formatted numbers" $ readExpression "1.0.0" `shouldBe` Left (InvalidExpression "Number was incorrectly formatted")
    
    it "Passes Property Test cases" $ property (prop_serializeroundtrip :: Expression Double -> Bool)

    it "Can Process Decimals" $ property (prop_serializeroundtrip :: Expression Decimal -> Bool)
