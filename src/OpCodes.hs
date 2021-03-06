module OpCodes where

import Data.Text (Text)
import Runtime.Types

data OpCode = OpConst JSVal   -- push constant
            | OpNum Double
            | OpInt Int
            | OpStr Text
            | OpBool Bool
            | OpNull
            | OpUndef
            | OpThis          -- push current `this`
            | OpVar Text      -- read variable, push reference
            | OpArray Int     -- make array from last n values on stack
            | OpSparse Int    -- make sparse array from last 2n+1 values on stack
            | OpGet Text      -- property access with identifier
            | OpGet2          -- property access with value
            | OpGetValue      -- dereference top of stack
            | OpToBoolean     -- convert TOS to boolean
            | OpToNumber      -- convert TOS to number
            | OpDiscard       -- discard TOS
            | OpDup           -- duplicate TOS
            | OpSwap          -- swap top 2 items on stack
            | OpRoll3         -- move TOS down 2
            | OpAdd           -- ...e2 e1 -> ...(e1+e2)
            | OpSub           -- ...e2 e1 -> ...(e1-e2)
            | OpMul           -- ...e2 e1 -> ...(e1*e2)
            | OpBinary Text   -- binary op on top 2
            | OpUnary Text    -- unary op on TOS
            | OpPostInc Int   -- Add arg to TOS reference without modifying val
            | OpModify Text   -- modify TOS reference & eat it
            | OpDelete        -- delete reference at TOS, leave bool
            | OpTypeof        -- replace TOS with its type name
            | OpStore         -- val is TOS, ref is NOS: write val to ref, leave val
            | OpFunCall Int   -- Function call with n arguments on stack, func is TOS, last arg is NOS
            | OpNewCall Int   -- As function call, but creating new object
            | OpObjLit Int    -- Create object literal from top N k-v pairs on stack
            | OpLambda        -- Convert VLambda at TOS to a function object
            | OpBranch Int
            | OpBFalse Int
            | Nop             -- Do nothing
            deriving (Show, Eq)

type CompiledExpr = [OpCode]
