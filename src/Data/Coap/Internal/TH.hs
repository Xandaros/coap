{-# LANGUAGE TemplateHaskell #-}
module Data.Coap.Internal.TH where
import Language.Haskell.TH

import Data.Coap.Internal

import Data.Maybe (fromJust)

generateEnum :: Name -> Integer -> Q [Dec]
generateEnum nam start = do
  info <- reify nam
  return $ [InstanceD [] (AppT (ConT ''XEnum) (ConT nam)) $
    case info of
        TyConI (DataD _ _ _ consts _) ->
          makeMethods nam (getConName <$> consts) start
        _ -> error "Invalid call to generateEnum"
    ]

getConName :: Con -> Name
getConName (NormalC nam _) = nam

makeMethods :: Name -> [Name] -> Integer -> [Dec]
makeMethods name consts i =
  let (fromCs, toCs) = makeClauses consts i
  in  [ FunD 'fromxEnum fromCs
      , FunD 'toxEnum   (toCs ++ [makeDefault name])
      ]

makeDefault :: Name -> Clause
makeDefault name = let var = mkName "var"
                       prefix = LitE . StringL $ "Invalid value for " ++ show name ++ ": "
                   in  Clause [VarP var] (NormalB $ AppE (ConE 'Left) (InfixE (Just prefix) (VarE '(++)) (Just (AppE (VarE 'show) (VarE var))))) []

makeClauses :: [Name] -> Integer -> ([Clause], [Clause])
makeClauses [] _ = ([], [])
makeClauses (x:xs) i =
  let (fromC, toC)   = makeClause x i
      (fromCs, toCs) = makeClauses xs (i+1)
  in  (fromC:fromCs, toC:toCs)

makeClause :: Name -> Integer -> (Clause, Clause)
makeClause c v =
  ( Clause [ConP c []] (NormalB (LitE (IntegerL v))) []
  , Clause [LitP (IntegerL v)] (NormalB (AppE (ConE 'Right) (ConE c))) []
  )
