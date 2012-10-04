import Control.Monad.State
import qualified Data.IntMap.Lazy as IntMap
import Data.Bits

type Word = Int

type Pointer = Word

data Register = RegA
              | RegB
              | RegC
              | RegX
              | RegY
              | RegZ
              | RegI
              | RegJ
              | RegPC
              | RegSP
              | RegO
                deriving Show

data DCPUState = DCPUState { ram :: IntMap.IntMap Word
                           , registerA :: Word
                           , registerB :: Word
                           , registerC :: Word
                           , registerX :: Word
                           , registerY :: Word
                           , registerZ :: Word
                           , registerI :: Word
                           , registerJ :: Word
                           , registerPC :: Pointer
                           , registerSP :: Pointer
                           , registerO :: Word
                           } deriving (Show)

initialState :: DCPUState
initialState = DCPUState { ram = IntMap.empty
                             , registerA = 0
                             , registerB = 0
                             , registerC = 0
                             , registerX = 0
                             , registerY = 0
                             , registerZ = 0
                             , registerI = 0
                             , registerJ = 0
                             , registerPC = 0
                             , registerSP = 0
                             , registerO = 0
                             }

data InstrValue = Register Register
                | PointerReg Register
                | NextPlusReg Word Register
                | POP
                | PEEK
                | PUSH
                | SP
                | PC
                | O
                | Pointer Word
                | NextLiteral Word
                | Literal Word
                  deriving Show

data BasicOp = SET InstrValue InstrValue
             | ADD InstrValue InstrValue
             | SUB InstrValue InstrValue
             | MUL InstrValue InstrValue
             | DIV InstrValue InstrValue
             | MOD InstrValue InstrValue
             | SHL InstrValue InstrValue
             | SHR InstrValue InstrValue
             | AND InstrValue InstrValue
             | BOR InstrValue InstrValue
             | XOR InstrValue InstrValue
             | IFE InstrValue InstrValue BasicOp
             | IFN InstrValue InstrValue BasicOp
             | IFG InstrValue InstrValue BasicOp
             | IFB InstrValue InstrValue BasicOp
               deriving Show

getRegister :: Register -> State DCPUState Word
getRegister RegA = gets registerA
getRegister RegB = gets registerB
getRegister RegC = gets registerC
getRegister RegX = gets registerX
getRegister RegY = gets registerY
getRegister RegZ = gets registerZ
getRegister RegI = gets registerI
getRegister RegJ = gets registerJ
getRegister RegPC = gets registerPC
getRegister RegSP = gets registerSP
getRegister RegO = gets registerO

setRegister :: Register -> Word  -> State DCPUState ()
setRegister RegA w = do
  st <- get
  put $ st { registerA = w }
setRegister RegB w = do
  st <- get
  put $ st { registerB = w }
setRegister RegC w = do
  st <- get
  put $ st { registerC = w }
setRegister RegX w = do
  st <- get
  put $ st { registerX = w }
setRegister RegY w = do
  st <- get
  put $ st { registerY = w }
setRegister RegZ w = do
  st <- get
  put $ st { registerZ = w }
setRegister RegI w = do
  st <- get
  put $ st { registerI = w }
setRegister RegJ w = do
  st <- get
  put $ st { registerJ = w }
setRegister RegPC w = do
  st <- get
  put $ st { registerPC = w }
setRegister RegSP w = do
  st <- get
  put $ st { registerSP = w }
setRegister RegO w = do
  st <- get
  put $ st { registerO = w }

getWordInRam :: Pointer -> State DCPUState Word
getWordInRam p = do
  r <- gets ram
  return $ IntMap.findWithDefault 0 p r

setWordInRam :: Pointer -> Word -> State DCPUState ()
setWordInRam p w = do
  st <- get
  put $ st { ram = (IntMap.insert p w $ ram st) }

getInstrValue :: InstrValue -> State DCPUState Word
getInstrValue (Register reg) = getRegister reg
getInstrValue (PointerReg reg) = do
  p <- getRegister reg
  getWordInRam p
getInstrValue (NextPlusReg w reg) = do
  p <- getRegister reg
  getWordInRam $ p + w
getInstrValue POP = do
  p <- getRegister RegSP
  setRegister RegSP $ p + 1
  getWordInRam p
getInstrValue PEEK = do
  p <- getRegister RegSP
  getWordInRam p
getInstrValue PUSH = do
  p <- getRegister RegSP
  setRegister RegSP $ p - 1
  getWordInRam $ p - 1
getInstrValue SP = getRegister RegSP
getInstrValue PC = getRegister RegPC
getInstrValue O = getRegister RegO
getInstrValue (Pointer w) = getWordInRam w
getInstrValue (NextLiteral w) = return w
getInstrValue (Literal w) = return w

setInstrValue :: InstrValue -> Word -> State DCPUState ()
setInstrValue (Register reg) w = setRegister reg w
setInstrValue (PointerReg reg) w = do
  p <- getRegister reg
  setWordInRam p w
setInstrValue (NextPlusReg nw reg) w = do
  p <- getRegister reg
  setWordInRam (p + nw) w
setInstrValue POP w = do
  p <- getRegister RegSP
  setWordInRam p w
setInstrValue PEEK w = do
  p <- getRegister RegSP
  setWordInRam p w
setInstrValue PUSH w = do
  p <- getRegister RegSP
  setWordInRam (p - 1) w
setInstrValue SP w = setRegister RegSP w
setInstrValue PC w = setRegister RegPC w
setInstrValue O w = setRegister RegO w
setInstrValue (Pointer p) w = do
  setWordInRam p w
setInstrValue (NextLiteral _) _ = undefined
setInstrValue (Literal _) _ = undefined

interp :: BasicOp -> State DCPUState ()
interp (SET dest val) = do
  getInstrValue dest
  v <- getInstrValue val
  setInstrValue dest v
interp (ADD dest val) = do
  b <- getInstrValue val
  a <- getInstrValue dest
  if (a + b > 0x1111) then
      setRegister RegO 0x0001
  else return ()
  setInstrValue dest ((a + b) `mod` 0x10000)
interp (SUB dest val) = do
  a <- getInstrValue dest
  b <- getInstrValue val
  if (a < b) then do
               setRegister RegO 0xffff
               setInstrValue dest $ a - b + 0x10000
  else
      setInstrValue dest (a - b)
interp (MUL dest val) = do
  a <- getInstrValue dest
  b <- getInstrValue val
  setInstrValue dest $ a*b
  setRegister RegO $ (.&.) 2 2
interp (DIV dest val) = do
  a <- getInstrValue dest
  b <- getInstrValue val
  if b == 0 then do
              setInstrValue dest 0
              setRegister RegO $ (.&.) (shift (a * b) (-16)) 0xffff
  else do
    setInstrValue dest $ a `div` b
    setRegister RegO $ (.&.) ((shift a 16) `div` b) 0xffff
interp (MOD dest val) = do
  a <- getInstrValue dest
  b <- getInstrValue val
  if b == 0 then do
              setInstrValue dest 0
              setRegister RegO 0
  else setInstrValue dest $ a `mod` b
interp (SHL dest val) = do
  a <- getInstrValue dest
  b <- getInstrValue val
  setInstrValue dest $ shift a b
  setRegister RegO $ (.&.) (shift (shift a b) (-16)) 0xffff
interp (SHR dest val) = do
  a <- getInstrValue dest
  b <- getInstrValue val
  setInstrValue dest $ shift a (-b)
  setRegister RegO $ (.&.) (shift (shift a 16) (-b)) 0xffff
interp (AND dest val) = do
  a <- getInstrValue dest
  b <- getInstrValue val
  setInstrValue dest $ (.&.) a b
interp (BOR dest val) = do
  a <- getInstrValue dest
  b <- getInstrValue val
  setInstrValue dest $ (.|.) a b
interp (XOR dest val) = do
  a <- getInstrValue dest
  b <- getInstrValue val
  setInstrValue dest $ xor a b
interp (IFE dest val next) = do
  a <- getInstrValue dest
  b <- getInstrValue val
  if a == b then interp next
  else return ()
interp (IFN dest val next) = do
  a <- getInstrValue dest
  b <- getInstrValue val
  if a /= b then interp next
  else return ()
interp (IFG dest val next) = do
  a <- getInstrValue dest
  b <- getInstrValue val
  if a > b then interp next
  else return ()
interp (IFB dest val next) = do
  a <- getInstrValue dest
  b <- getInstrValue val
  if ((.&.) a b) /= 0 then interp next
  else return ()

interpList :: [BasicOp] -> State DCPUState ()
interpList [] = return ()
interpList (x : xs) = do
  interp x
  interpList xs

--runProg :: [BasicOp] -> State DCPUState ()
runProg :: [BasicOp] -> ((), DCPUState)
runProg prog = (runState (interpList prog)) initialState

{--
Example:
runProg [SET (Register RegA) (Literal 0x30)
        , SET (Pointer 0x1001) (Literal 0x20)
        , ADD (Register RegB) (Literal 0x10000)
        , IFE (Register RegA) (Literal 48) (SET (Register RegX) (Literal 0x1))
        ]
--}
