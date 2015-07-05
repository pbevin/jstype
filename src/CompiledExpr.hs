module CompiledExpr where

import Expr
import Runtime.Types

data CompiledExpr = OpConst JSVal    -- push constant
                   | OpThis          -- push current `this`
                   | OpVar Ident     -- read variable, push reference
                   | OpArray Int     -- make array from last n values on stack
                   | OpSparse Int    -- make sparse array from last 2n+1 values on stack
                   | OpGet Ident     -- property access with identifier
                   | OpGet2          -- property access with value
                   | OpGetValue      -- dereference top of stack
                   | OpToBoolean     -- convert TOS to boolean
                   | OpToNumber      -- convert TOS to number
                   | OpDiscard       -- discard TOS
                   | OpDup           -- duplicate TOS
                   | OpSwap          -- swap top 2 items on stack
                   | OpRoll3         -- move TOS down 2
                   | OpBinary Ident  -- binary op on top 2
                   | OpUnary Ident   -- unary op on TOS
                   | OpModify Ident  -- modify TOS reference & eat it
                   | OpDelete        -- delete reference at TOS, leave bool
                   | OpTypeof        -- replace TOS with its type name
                   | OpStore         -- val is TOS, ref is NOS: write val to ref, leave val
                   | BasicBlock [CompiledExpr]
                   | IfEq JSVal CompiledExpr
                   | Interpreted Expr
                   deriving (Show, Eq)
