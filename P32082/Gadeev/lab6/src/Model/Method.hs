module Model.Method
    ( Method(..)
    , compute
    , getAccuracyOrder
    ) where

data Method
    = Euler
    | Midpoint
    | RungeKutta
    | Adams
    | Milne
    deriving (Eq, Show)

compute
    :: Method
    -> (Double -> Double -> Double)
    -> Double
    -> Double
    -> Double
    -> Double
    -> [Double]
    -> [(Double, Double)]
compute method f a b h e ys = take (floor ((b-a)/h)+1) result where
    result = (zip [a, a+h..] ys) <> (getNext <$> [0..])
    getNext i = case method of
        Euler -> (xn+h, yn+h*fn)
        Midpoint -> (xn+h, yn+h/2*(fn+(f (xn+h) (yn+h*fn))))
        RungeKutta -> (xn+h, yn+1/6*(k1+2*k2+2*k3+k4))
        Adams -> (xn3+h, yn3+h*fn3+df+df2+df3)
        Milne -> (xn3+h, snd $ head $ dropWhile errorCheck cs)
        where
            (xn, yn) = result!!i
            (xn1, yn1) = result!!(i+1)
            (xn2, yn2) = result!!(i+2)
            (xn3, yn3) = result!!(i+3)
            fn = f xn yn
            fn1 = f xn1 yn1
            fn2 = f xn2 yn2
            fn3 = f xn3 yn3
            df = h*h/2*(fn3-fn2)
            df2 = 5*(h**3)/12*(fn3-2*fn2+fn1)
            df3 = 3*(h**4)/8*(fn3-3*fn2+3*fn1-fn)
            k1 = h*fn
            k2 = h*(f (xn+h/2) (yn+k1/2))
            k3 = h*(f (xn+h/2) (yn+k2/2))
            k4 = h*(f (xn+h) (yn+k3))
            milnePrediction = yn+4*h/3*(2*fn1-fn2+2*fn3)
            milneCorrect pr = yn2+h/3*(fn2+4*fn3+(f (xn3+h) pr))
            milneCorrections = iterate milneCorrect milnePrediction
            cs = zip milneCorrections (tail milneCorrections)
    errorCheck (c1, c2) = abs (c1-c2) >= e

getAccuracyOrder :: Method -> Double
getAccuracyOrder Euler = 1
getAccuracyOrder Midpoint = 2
getAccuracyOrder _ = 4
