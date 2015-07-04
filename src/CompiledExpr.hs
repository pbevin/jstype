module CompiledExpr where

import Expr
import Runtime.Types

data CompiledExpr = OpConst JSVal    -- push constant
                   | OpThis          -- push current `this`
                   | OpVar Ident     -- read variable, push reference
                   | OpGet Ident     -- property access with identifier
                   | OpGet2          -- property access with value
                   | OpToValue       -- dereference top of stack
                   | OpToBoolean     -- convert TOS to boolean
                   | OpDiscard       -- discard TOS
                   | OpDup           -- duplicate TOS
                   | OpBinary Ident  -- binary op on top 2
                   | BasicBlock [CompiledExpr]
                   | IfEq JSVal CompiledExpr
                   | Interpreted Expr
                   deriving (Show, Eq)
