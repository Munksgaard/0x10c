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


data Instr = SET Pointer Int
           | ADD Int Int
           | SUB Int Int
           | MUL Int Int
           | DIV Int Int
           | MOD Int Int




-- Basic opcodes: (4 bits)
--     0x0: non-basic instruction - see below
--     0x1: SET a, b - sets a to b
--     0x2: ADD a, b - sets a to a+b, sets O to 0x0001 if there's an overflow, 0x0 otherwise
--     0x3: SUB a, b - sets a to a-b, sets O to 0xffff if there's an underflow, 0x0 otherwise
--     0x4: MUL a, b - sets a to a*b, sets O to ((a*b)>>16)&0xffff
--     0x5: DIV a, b - sets a to a/b, sets O to ((a<<16)/b)&0xffff. if b==0, sets a and O to 0 instead.
--     0x6: MOD a, b - sets a to a%b. if b==0, sets a to 0 instead.
--     0x7: SHL a, b - sets a to a<<b, sets O to ((a<<b)>>16)&0xffff
--     0x8: SHR a, b - sets a to a>>b, sets O to ((a<<16)>>b)&0xffff
--     0x9: AND a, b - sets a to a&b
--     0xa: BOR a, b - sets a to a|b
--     0xb: XOR a, b - sets a to a^b
--     0xc: IFE a, b - performs next instruction only if a==b
--     0xd: IFN a, b - performs next instruction only if a!=b
--     0xe: IFG a, b - performs next instruction only if a>b
--     0xf: IFB a, b - performs next instruction only if (a&b)!=0

-- state kun som ram?
-- special cases for første 64 ram entries så de henviser til state i en datatype?
--  som ovenfor, p-64 på ram, 

--  scratch that, jeg er noob


