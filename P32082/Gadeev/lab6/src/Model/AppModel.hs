{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( module Model.Method
    , Menu(..)
    , AppModel(..)
    , xLock
    , yLock
    , calcMethod
    , preCalcMethod
    , currentEquation
    , pointA
    , pointB
    , accuracy
    , stepSize
    , computationPoints
    , errorEstimation
    , currentMenu
    , initModel
    , equations
    , solutions
    ) where

import Control.Lens
import Data.Text (Text)

import Model.Method

data Menu
    = MParameters
    | MPoints
    deriving (Eq, Show)

data AppModel = AppModel
    { _amXLock :: Bool
    , _amYLock :: Bool
    , _amCalcMethod :: Method
    , _amPreCalcMethod :: Maybe Method
    , _amCurrentEquation :: Int
    , _amPointA :: Double
    , _amPointB :: Double
    , _amAccuracy :: Double
    , _amStepSize :: Double
    , _amComputationPoints :: [(Double, Double)]
    , _amErrorEstimation :: Double
    , _amCurrentMenu :: Menu
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel
    { _amXLock = False
    , _amYLock = False
    , _amCalcMethod = Euler
    , _amPreCalcMethod = Just RungeKutta
    , _amCurrentEquation = 0
    , _amPointA = -1
    , _amPointB = 1
    , _amAccuracy = 0.01
    , _amStepSize = 0.5
    , _amComputationPoints = []
    , _amErrorEstimation = 0
    , _amCurrentMenu = MParameters
    }

equations :: [(Double -> Double -> Double, Text)]
equations =
    [ (\x y -> y+x-1, "y' = y + x - 1")
    , (\x y -> 2*y-2*x**2+1, "y' = 2y - 2x^2 + 1")
    , (\x y -> 2*x*y-2*x**4-3*x**2+3, "y' = 2xy - 2x^4 - 3x^2 + 3")
    , (\x _ -> x**2+1, "y' = x^2 + 1")
    ]

solutions :: [(Double -> Double, Text)]
solutions =
    [ (\x -> exp x - x, "y = e^x - x")
    , (\x -> x**2 + x, "y = x^2 + x")
    , (\x -> exp (x**2) + x**3 + 3*x, "y = e^(x^2) + x^3 + 3x")
    , (\x -> (x**3)/3 + x, "y = (x^3)/3 + x")
    ]
