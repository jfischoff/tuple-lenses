{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts #-}
module Data.Tuple.Lens.TH where
import Language.Haskell.TH
import Control.Lens
import Control.Lens.Tuple
import Control.Applicative
import Control.Monad

declareLenses :: [[Int]] -> Q [Dec]
declareLenses = mapM declareLens'
    
--_1_2 = lens (\x -> (x^._1, x^._2)) (\x (a, b) -> _1 .~ a <&> _2 .~ b $ x) 
declareLens :: [Int] -> Q [Dec]
declareLens = fmap (:[]) . declareLens'

declareLens' :: [Int] -> Q Dec
declareLens' indices = do
    let body      = normalB $ mkLens indices 
        name      = mkName $ concatMap (\x -> "_" ++ show x) indices  
    funD name [clause [] body []]
    
tl :: [Int] -> Q Exp
tl = mkLens

mkLens :: [Int] -> Q Exp
mkLens indices = do
    let lensNames = map intToLens indices
        getter    = mkGetter lensNames
        setter    = mkSetter lensNames
    [e| lens $getter $setter |]
 
-- (\x -> (x^._1, x^._2))    
mkGetter :: [Name] -> Q Exp
mkGetter ls = do
    x <- newName "x"
    let mkGet l = infixE (Just (varE x)) (varE '(^.)) (Just (varE l))
    lamE [return $ VarP x] . tupE . map mkGet $ ls
    
--(\x (a, b) -> _1 .~ a <&> _2 .~ b $ x)         
mkSetter :: [Name] -> Q Exp
mkSetter ls = do
    x <- newName "x"
    args <- replicateM (length ls) . newName $ "a"
    let mkSet l n = infixE (Just (varE l)) (varE '(.~)) (Just (varE n))
        pattern = [return $ VarP x, tupP . map (return . VarP) $ args]
        foldAmp = foldl1 (\x y -> infixE (Just x) (varE '(<&>)) (Just y)) 
        setters = foldAmp . zipWith mkSet ls $ args
    lamE pattern . appE setters . varE $ x


intToLens :: Int -> Name
intToLens i = case i of
    1 -> '_1
    2 -> '_2
    3 -> '_3    
    4 -> '_4
    5 -> '_5
    6 -> '_6
    7 -> '_7
    8 -> '_8
    9 -> '_9
    _ -> error $ show i ++ " is an unsupported tuple index. Only 1 - 9 are supported." 
    




