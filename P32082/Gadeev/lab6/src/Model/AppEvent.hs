module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Data.Fixed
import Monomer
import Monomer.Graph
import TextShow

import Model.AppModel

data AppEvent
    = AppInit
    | AppResetGraph
    | AppResetA
    | AppResetB
    | AppResetAccuracy
    | AppResetStepSize
    | AppChangePointA Int (Double, Double)
    | AppChangePointB Int (Double, Double)
    | AppPointClicked Int
    deriving (Eq, Show)

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit ->
        [ Model $ model
            & computationPoints .~ result
            & errorEstimation .~ estimation
        ]
    AppResetGraph -> [Message "mainGraph" GraphReset]
    AppResetA ->
        [ Model $ model & pointA .~ (-1)
        , Event AppInit
        ]
    AppResetB ->
        [ Model $ model & pointB .~ 1
        , Event AppInit
        ]
    AppResetAccuracy ->
        [ Model $ model & accuracy .~ 0.01
        , Event AppInit
        ]
    AppResetStepSize ->
        [ Model $ model & stepSize .~ 0.5
        , Event AppInit
        ]
    AppChangePointA _ (x, _) ->
        [ Model $ model & pointA .~ g x
        , Event AppInit
        ]
    AppChangePointB _ (x, _) ->
        [ Model $ model & pointB .~ g x
        , Event AppInit
        ]
    AppPointClicked i ->
        [ Model $ model & currentMenu .~ MPoints
        , SetFocusOnKey $ WidgetKey $ showt i
        ]
    where
        g x = (fromIntegral $ (div' x 0.01 :: Int))*0.01
        result = getResult h
        result2 = getResult $ h*2
        f = fst $ equations!!(model ^. currentEquation)
        a = model ^. pointA
        b = model ^. pointB
        h = model ^. stepSize
        e = model ^. accuracy
        method = model ^. calcMethod
        fe = fst $ solutions!!(model ^. currentEquation)
        getResult step = compute method f a b step e ys where
            ys = if method `elem` [Adams, Milne]
                then ys'
                else [fe a]
            ys' = take 4 $ case model ^. preCalcMethod of
                Nothing -> fe <$> [a, a+step..]
                Just m -> snd <$> (compute m f a b step e $ [fe a])
        estimation = maximum $ zipWith rungeR result1 result2
        result1 = (result!!) <$> takeWhile (< length result) [0,2..]
        p = getAccuracyOrder $ model ^. calcMethod
        rungeR (_, y1) (_, y2) = (abs (y1-y2))/(2**p-1)
