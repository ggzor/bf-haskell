{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

import Control.Applicative ((<|>))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Writer
import Data.Char (chr, ord)
import Data.Function ((&))
import Data.Int
import qualified Data.IntMap.Strict as M
import Data.Maybe (fromMaybe)
import System.Environment
import System.Exit

type Program = M.IntMap Char

data BFState = BFState
  { input :: [Char]
  , dataCells :: M.IntMap Int8
  , dataPointer :: Int
  , instructionPointer :: Int
  }

type Output = String

type Imperative m =
  ( MonadReader Program m
  , MonadState BFState m
  , MonadWriter Output m
  , MonadFail m
  )

theDataPointer f s@BFState{dataPointer} = s{dataPointer = f dataPointer}
theCurrentByte f s@BFState{dataCells, dataPointer} =
  s
    { dataCells =
        M.alter
          ((<|> Just (f 0)) . fmap f)
          dataPointer
          dataCells
    }
theInstructionPointer f s@BFState{instructionPointer} = s{instructionPointer = f instructionPointer}

readByte :: Imperative m => m Int8
readByte = do
  BFState{dataCells, dataPointer} <- get
  pure . fromMaybe 0 $ M.lookup dataPointer dataCells

readInstruction :: Imperative m => m Char
readInstruction = do
  program <- ask
  BFState{instructionPointer} <- get
  Just instruction <- pure $ M.lookup instructionPointer program
  pure instruction

jumpNextBracket :: Imperative m => m ()
jumpNextBracket = go 0
 where
  go n = do
    modify (theInstructionPointer (+ 1))
    readInstruction >>= \case
      ']' | n == 0 -> pure ()
      ']' -> go (n - 1)
      '[' -> go (n + 1)
      _ -> go n

jumpPreviousBracket :: Imperative m => m ()
jumpPreviousBracket = go 0
 where
  go n = do
    modify (theInstructionPointer (+ (-1)))
    readInstruction >>= \case
      '[' | n == 0 -> pure ()
      '[' -> go (n - 1)
      ']' -> go (n + 1)
      _ -> go n

runProgram :: Imperative m => m ()
runProgram = do
  c <- readInstruction
  case c of
    '>' -> modify (theDataPointer (+ 1))
    '<' -> modify (theDataPointer (+ (-1)))
    '+' -> modify (theCurrentByte (+ 1))
    '-' -> modify (theCurrentByte (+ (-1)))
    '.' -> readByte >>= tell . (: []) . chr . fromIntegral
    ',' -> do
      (c : remaining) <- gets input
      modify (theCurrentByte (const . fromIntegral . ord $ c))
      modify (\s -> s{input = remaining})
    '[' ->
      readByte >>= \case
        0 -> jumpNextBracket
        _ -> pure ()
    ']' ->
      readByte >>= \case
        0 -> pure ()
        _ -> jumpPreviousBracket
    _ -> pure ()
  modify (theInstructionPointer (+ 1))
  runProgram

main =
  do
    input <- getContents
    args <- getArgs
    if length args /= 1
      then putStrLn "Usage: cabal run bf-haskell -- PROGRAM" >> exitFailure
      else do
        program <- getArgs >>= readFile . head
        let output =
              runProgram
                `evalStateT` ( BFState
                                { input
                                , dataCells = M.empty
                                , dataPointer = 0
                                , instructionPointer = 0
                                }
                             )
                `runReaderT` listToMap program
                & runMaybeT
                & runWriter
                & snd
        putStr output
 where
  listToMap = M.fromAscList . zip [0 ..]
