{-# LANGUAGE 
    TypeFamilies, 
    FlexibleInstances, 
    MultiParamTypeClasses, 
    TemplateHaskell,
    TypeOperators
    #-}

module Zice where

import Control.Category
import Data.Label 
import Data.Maybe as M
import Prelude hiding ((.),id)

data family Electrical a

data Resistance = Resistance

data Current = Current

data Potential = Potential

data Power = Power 

data instance Electrical Resistance = R Double 
    deriving (Eq,Ord)

data instance Electrical Current = A Double 
    deriving (Eq,Ord)

data instance Electrical Potential = V Double  
    deriving (Eq,Ord)

data instance Electrical Power = W Double 
    deriving (Eq,Ord) 

instance Show (Electrical Resistance) where
    show (R a) = show a ++ " Ohms"

instance Show (Electrical Current) where
    show (A a) = show a ++ " Amps"

instance Show (Electrical Potential) where
    show (V a) = show a ++ " Volts"

instance Show (Electrical Power) where
    show (W a) = show a ++ " Watts" 

instance Num (Electrical Resistance) where
    (R a) + (R b) = R (a + b)
    (R a) - (R b) = R (a - b)
    fromInteger a = R (fromInteger a) 

instance Num (Electrical Current) where
    (A a) + (A b) = A (a + b)
    (A a) - (A b) = A (a - b)
    fromInteger a = A (fromInteger a)

instance Num (Electrical Potential) where
    (V a) + (V b) = V (a + b)
    (V a) - (V b) = V (a - b)
    fromInteger a = V (fromInteger a)

instance Num (Electrical Power) where
    (W a) + (W b) = W (a + b)
    (W a) - (W b) = W (a - b)
    fromInteger a = W (fromInteger a)

instance Fractional (Electrical Resistance) where
    recip (R a) = R (recip a)
--    fromRational (R a) = R (fromRational a)

instance Fractional (Electrical Current) where
    recip (A a) = A (recip a)
--    fromRational (A a) = A (fromRational a)

instance Fractional (Electrical Potential) where
    recip (V a) = V (recip a)
--    fromRational (V a) = V (fromRational a)

instance Fractional (Electrical Power) where
    recip (W a) = W (recip a)
--    fromRational (W a) = W (fromRational a)

class ElectricalCalculate a b where
    at :: Electrical a -> Electrical b -> Component

instance ElectricalCalculate Current Resistance where
    (A i) `at` (R r) = Component { _cr = R r , _ca = A i , _cv = V (i*r), _cw = W (i*i*r) }

instance ElectricalCalculate Resistance Current where
    (R r) `at` (A i) = Component { _cr = R r , _ca = A i , _cv = V (i*r), _cw = W (i*i*r) }

instance ElectricalCalculate Current Potential where 
    (A i) `at` (V e) = Component { _cr = R (e/i) , _ca = A i , _cv = V e , _cw = W (e*i) } 

instance ElectricalCalculate Potential Current where 
    (V e) `at` (A i) = Component { _cr = R (e/i) , _ca = A i , _cv = V e , _cw = W (e*i) } 

instance ElectricalCalculate Resistance Potential where
    (R r) `at` (V e) = Component { _cr = R r , _ca = A (e/r) , _cv = V e, _cw = W (e*(e/r)) }

instance ElectricalCalculate Potential Resistance where
    (V e) `at` (R r) = Component { _cr = R r , _ca = A (e/r) , _cv = V e, _cw = W (e*(e/r)) }

-- NOW FOR POWER :-)

instance ElectricalCalculate Power Resistance where 
    (W p) `at` (R r) = Component { _cr = R r , _ca = A ((p/r)/r) , _cv = V (p/r) , _cw = W p }

instance ElectricalCalculate Resistance Power where 
    (R r) `at` (W p) = Component { _cr = R r , _ca = A ((p/r)/r) , _cv = V (p/r) , _cw = W p }

instance ElectricalCalculate Power Current where
    (W p) `at` (A i) = Component { _cr = R ((p/i)/i) , _ca = A i , _cv = V (p/i) , _cw = W p } 

instance ElectricalCalculate Current Power  where
    (A i) `at` (W p) = Component { _cr = R ((p/i)/i) , _ca = A i , _cv = V (p/i) , _cw = W p } 

instance ElectricalCalculate Power Potential where
    (W p) `at` (V e) = Component { _cr = R (e/(p/e)) , _ca = A (p/e) , _cv = V e , _cw = W p } 

instance ElectricalCalculate Potential Power where 
    (V e) `at` (W p) = Component { _cr = R (e/(p/e)) , _ca = A (p/e) , _cv = V e , _cw = W p } 
 
data Component = Component {
        _cr :: Electrical Resistance,
        _ca :: Electrical Current,
        _cv :: Electrical Potential,
        _cw :: Electrical Power
        } deriving (Show) 

a `from` b = a $ b
infixr 0 `from`

$(mkLabels[''Component])
