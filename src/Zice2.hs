{-# LANGUAGE TypeFamilies #-}
--This way of coding does not work 

module Zice where

--data family Electrical a 

data Electrical = R Double | A Double | V Double | W Double 
    deriving (Ord) --Build Custom Eq 

instance Eq Electrical where
    (R a) == (R a) = a == a
    (A a) == (A a) = a == a
    (V a) == (V a) = a == a
    (W a) == (W a) = a == a 
    
instance Show Electrical where
    show (R a) = show a ++ " Ohms"
    show (A a) = show a ++ " Amps"
    show (V a) = show a ++ " Volts"
    show (W a) = show a ++ " Watts" 

instance Num Electrical where
    (R a) + (R b) = R (a + b)
    (R a) - (R b) = R (a - b)
    fromInteger a = R (fromInteger a) 
    (A a) + (A b) = A (a + b)
    (A a) - (A b) = A (a + b)
    fromInteger a = A (fromInteger a)
    (A a) + (A b) = A (a + b)
    (A a) - (A b) = A (a + b)
    fromInteger a = A (fromInteger a)
    (V a) + (V b) = V (a + b)
    (V a) - (V b) = V (a + b)
    fromInteger a = V (fromInteger a)
    (W a) + (W b) = W (a + b)
    (W a) - (W b) = W (a + b)
    fromInteger a = W (fromInteger a)

instance Fractional Electrical where
    recip (R a) = R (recip a)
    recip (A a) = A (recip a)
    recip (V a) = V (recip a)
    recip (W a) = W (recip a)



--Resistance,Voltage,Amperage,Watts
calculate :: Electrical -> Electrical -> (Electrical,Electrical,Electrical,Electrical) 
calculate (R r) (A i) = (R r,V i*r,A i,W i*i*r)

