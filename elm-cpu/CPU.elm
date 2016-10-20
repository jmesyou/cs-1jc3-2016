-- Simulate a very simple CPU

-- CPU = Central Processing Unit
--     \= GPU / Graphics Processing Unit (includes Xeon Phi)
--    - most general
--    - von Neumann architecture

module CPU exposing (..)

import Array exposing (Array)

-- the transitions for the CPUState are all instructions
-- "the CPU is a good listener"

type Instruction  = Load           RegisterNumber           -- put value here
                                   RegisterNumber           -- from address which is sum of this register value
                                   RegisterNumber           -- and this register value
  -- this is like reading a number in the margin of the Sudoku puzzle
                  | Store          RegisterNumber           -- store value in this register
                                   RegisterNumber           -- to address which is sum of this register value
                                   RegisterNumber           --   and this register value
  -- this is like writing something in the margin of the Sudoku puzzle
                  | LoadImmediate  RegisterNumber           -- put value here
                                   RegisterValue            -- the value
  -- this is like reading a number as part of a logic puzzle
                  | Add            RegisterNumber           -- put result here
                                   RegisterNumber           -- first thing to add
                                   RegisterNumber           -- second thing to add

                  | Multiply       RegisterNumber           -- put result here
                                   RegisterNumber           -- first thing to multiply
                                   RegisterNumber           -- second thing to multiply

                  | And            RegisterNumber           -- put result here
                                   RegisterNumber           -- first thing to and
                                   RegisterNumber           -- second thing to and

                  | Or             RegisterNumber           -- put result here
                                   RegisterNumber           -- first thing to or
                                   RegisterNumber           -- second thing to or

                  | Not            RegisterNumber           -- put result here
                                   RegisterNumber           -- reverse bits from here

                  | Rotate         RegisterNumber           -- put result here
                                   RegisterNumber           -- value to rotate
                                   Int                      -- rotate bits (left is positive)

                  | Compare        RegisterNumber           -- compare the value in this register
                                   RegisterNumber           -- to the value in this register (result goes in CPU State)

                  | Branch         (List ComparisonResult)  -- results (maybe all) which will cause branch
                                   RegisterNumber           -- instruction to branch to if true

                  | Halt

type alias Program = Int -> Maybe Instruction

type alias Data = Array Int
type alias Code = List Instruction

mkProgram : List Instruction -> Program
mkProgram instrs = let code = Array.fromList instrs
                   in  \ idx -> Array.get idx code

type alias RegisterNumber = Int
type alias RegisterValue = Int
type alias ComparisonResult = Order
