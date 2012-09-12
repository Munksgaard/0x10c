type Word = Int

type Pointer = Int

data DCPUState = State { ram :: [Word]
                       , a :: Word
                       , b :: Word
                       , c :: Word
                       , x :: Word
                       , y :: Word
                       , z :: Word
                       , i :: Word
                       , j :: Word
                       , pc :: Pointer
                       , sp :: Pointer
                       , o :: Word
                       } deriving (Show)

-- state kun som ram?
-- special cases for første 64 ram entries så de henviser til state i en datatype?
--  som ovenfor, p-64 på ram, 

--  scratch that, jeg er noob


