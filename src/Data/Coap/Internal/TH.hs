{-# LANGUAGE TemplateHaskell #-}
module Data.Coap.Internal.TH where
import Language.Haskell.TH

import Data.Maybe (fromJust)

generateEnum :: Name -> Integer -> Q [Dec]
generateEnum nam start = do
  info <- reify nam
  return $ [InstanceD [] (AppT (ConT ''Enum) (ConT nam)) $
    case info of
        TyConI (DataD _ _ _ consts _) ->
          makeMethods (getConName <$> consts) start
        _ -> error "Invalid call to generateEnum"
    ]

getConName :: Con -> Name
getConName (NormalC nam _) = nam

makeMethods :: [Name] -> Integer -> [Dec]
makeMethods consts i =
  let (fromCs, toCs) = makeClauses consts i
  in  [ FunD 'fromEnum fromCs
      , FunD 'toEnum   toCs
      ]

makeClauses :: [Name] -> Integer -> ([Clause], [Clause])
makeClauses [] _ = ([], [])
makeClauses (x:xs) i =
  let (fromC, toC)   = makeClause x i
      (fromCs, toCs) = makeClauses xs (i+1)
  in  (fromC:fromCs, toC:toCs)

makeClause :: Name -> Integer -> (Clause, Clause)
makeClause c v =
  ( Clause [ConP c []] (NormalB (LitE (IntegerL v))) []
  , Clause [LitP (IntegerL v)] (NormalB (ConE c)) []
  )
