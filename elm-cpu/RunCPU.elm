-- Simulate a very simple CPU

module RunCPU exposing (..)

import CPU exposing (..)
import List exposing (member)
import Bitwise
import Array exposing (Array)

initialState = CPUState  (0,0,0,0,0,0,0,0)     -- all registers start with value zero
                         0                     -- start execution with the first instruction
                         EQ                    -- result of test starts as equal
                         Nothing
dataSize = 1024
initialData  : Array Int
initialData  = Array.initialize 1024 identity

initialTinyData = Array.initialize 4 identity

-- executeOne gives a semantics for the CPU instruction language
executeOne : Instruction -> (CPUState,Data) -> (CPUState,Data)
executeOne instr (cpuState,dat) =
  case instr of
    Load target addr1 addr2 -> let
                                 newCpuState = case (getRegisterVal addr1  cpuState
                                                    ,getRegisterVal addr2  cpuState) of
                                                 (Just addr1',Just addr2') -> case Array.get (addr1' + addr2') dat of
                                                                                     Just loadValue -> changeRegister target loadValue cpuState
                                                                                     Nothing -> illegalAddress cpuState
                                                 otherwise -> illegalRegister cpuState
                               in
                                 nextInstruction (newCpuState, dat)

    Store regNum addr1 addr2 -> nextInstruction <|
                                    case (getRegisterVal addr1  cpuState
                                         ,getRegisterVal addr2  cpuState
                                         ,getRegisterVal regNum cpuState) of
                                      (Just addr1',Just addr2',Just value3) -> if 0 <= addr1' + addr2' && addr1' + addr2' < dataSize
                                                                            then (cpuState, Array.set (addr1' + addr2') value3 dat)
                                                                            else (illegalAddress cpuState, dat)
                                      otherwise -> (illegalRegister cpuState,dat)

    LoadImmediate target value -> nextInstruction
                                    (changeRegister target value cpuState, dat)

    Add target arg1 arg2 -> twoArgOp (cpuState,dat) target arg1 arg2 (+)
    Multiply target arg1 arg2 -> twoArgOp (cpuState,dat) target arg1 arg2 (*)
    And target arg1 arg2 -> twoArgOp (cpuState,dat) target arg1 arg2 Bitwise.and
    Or target arg1 arg2 -> twoArgOp (cpuState,dat) target arg1 arg2 Bitwise.or

    Not target arg1 -> (case getRegisterVal arg1 cpuState of
                          Just val  -> changeRegister target (Bitwise.complement val) cpuState
                          otherwise -> illegalRegister cpuState
                       , dat)
                       |> nextInstruction

    Rotate target arg1 shift -> (case getRegisterVal arg1 cpuState of
                                   Just val  -> changeRegister target (Bitwise.shiftLeft val (-shift)) cpuState
                                   otherwise -> illegalRegister cpuState
                                , dat)
                                |> nextInstruction

    Compare arg1 arg2 -> (case (getRegisterVal arg1 cpuState
                               ,getRegisterVal arg2 cpuState) of
                            (Just x,Just y) -> let (CPUState regs inst result halted) = cpuState
                                               in   CPUState regs inst (compare x y) halted
                            otherwise       -> illegalRegister cpuState
                         ,dat)
                         |> nextInstruction

    Branch branchOn targetAddress ->
      let (CPUState regs inst result halted) = cpuState
      in (case getRegisterVal targetAddress cpuState of
            Just nextInst -> if result `member` branchOn
                               then if 0 <= nextInst && nextInst < 4400
                                      then CPUState regs nextInst result halted
                                      else illegalInstrAddress cpuState
                               else CPUState regs (inst + 1) result halted
            otherwise     -> illegalRegister cpuState
         ,dat)
    Halt  ->
      let (CPUState regs inst result halted) = cpuState
      in  (CPUState regs inst result (Just ReachedHalt),dat)

twoArgOp (cpuState,dat) target arg1 arg2 op = (case (getRegisterVal arg1 cpuState
                                                    ,getRegisterVal arg2 cpuState) of
                                                 (Just x,Just y) -> changeRegister  target (x `op` y) cpuState
                                                 otherwise       -> illegalRegister cpuState
                                              , dat)
                                              |> nextInstruction


illegalAddress (CPUState regs currInstr comparedAs _)
              = CPUState regs currInstr comparedAs (Just IllegalAddress)

illegalInstrAddress (CPUState regs currInstr comparedAs _)
                   = CPUState regs currInstr comparedAs (Just IllegalInstrAddress)

illegalRegister (CPUState regs currInstr comparedAs _)
               = CPUState regs currInstr comparedAs (Just IllegalRegisterNum)

changeRegister  regNum
                newValue
                (CPUState  (reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8)
                           currentInstruction
                           comparedAs
                           maybeHalted
                )
  = case regNum of
      1 -> CPUState (newValue,reg2,reg3,reg4,reg5,reg6,reg7,reg8)
                    currentInstruction comparedAs maybeHalted
      2 -> CPUState (reg1,newValue,reg3,reg4,reg5,reg6,reg7,reg8)
                    currentInstruction comparedAs maybeHalted
      3 -> CPUState (reg1,reg2,newValue,reg4,reg5,reg6,reg7,reg8)
                    currentInstruction comparedAs maybeHalted
      4 -> CPUState (reg1,reg2,reg3,newValue,reg5,reg6,reg7,reg8)
                    currentInstruction comparedAs maybeHalted
      5 -> CPUState (reg1,reg2,reg3,reg4,newValue,reg6,reg7,reg8)
                    currentInstruction comparedAs maybeHalted
      6 -> CPUState (reg1,reg2,reg3,reg4,reg5,newValue,reg7,reg8)
                    currentInstruction comparedAs maybeHalted
      7 -> CPUState (reg1,reg2,reg3,reg4,reg5,reg6,newValue,reg8)
                    currentInstruction comparedAs maybeHalted
      8 -> CPUState (reg1,reg2,reg3,reg4,reg5,reg6,reg7,newValue)
                    currentInstruction comparedAs maybeHalted
      _ -> CPUState (reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8)
                    currentInstruction comparedAs (Just IllegalRegisterNum)

getRegisterVal  regNum
                (CPUState  (reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8)
                           currentInstruction
                           comparedAs
                           maybeHalted
                )
  = case regNum of
      0 -> Just 0
      1 -> Just reg1
      2 -> Just reg2
      3 -> Just reg3
      4 -> Just reg4
      5 -> Just reg5
      6 -> Just reg6
      7 -> Just reg7
      8 -> Just reg8
      _ -> Nothing


nextInstruction  (CPUState  (reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8)
                            currentInstruction
                            comparedAs
                            maybeHalted
                 ,dat)
  = (CPUState (reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8)
              (case maybeHalted of
                 Nothing -> currentInstruction + 1
                 otherwise -> currentInstruction     -- don't go on if halted
              )
              comparedAs
              maybeHalted
    ,dat)

runProgram : CPU.Program -> (CPUState,Data) -> (CPUState,Data)
runProgram prog init = runProgram' prog init

runProgram' : CPU.Program -> (CPUState,Data) -> (CPUState,Data)
runProgram' prog (CPUState regs inst result halted,dat)
  = case halted of
      Nothing   -> case prog inst of
                     Just i  -> runProgram' prog (executeOne i (CPUState regs inst result halted,dat))
                     Nothing -> ((CPUState regs inst result (Just IllegalInstrAddress)),dat)
      otherwise -> ((CPUState regs inst result halted),dat)

type alias CurrentInstruction = Int

type HaltedBecause  = ReachedHalt
                    | IllegalRegisterNum
                    | IllegalAddress
                    | IllegalInstrAddress

type CPUState = CPUState  (RegisterValue, RegisterValue, RegisterValue, RegisterValue,
                           RegisterValue, RegisterValue, RegisterValue, RegisterValue)
                          CurrentInstruction
                          ComparisonResult
                          (Maybe HaltedBecause)

isHalted : CPUState -> Bool
isHalted (CPUState _ _ _ maybeHalted) = case maybeHalted of
                                          Nothing -> False
                                          _       -> True

-- registers are like the working memory in our brains
--   to be useful, we need these numbers fast
--     because of physics students (and the speed of light) this means close
--     only so many things can be close, that's the limit on registers
-- when we run out of space, we put our numbers in main memory (RAM)
--   (there is an intermediate middle-sized memory called cache)
-- think of main memory as book, with lines and pages
--   number of these boxes is called a memory address
