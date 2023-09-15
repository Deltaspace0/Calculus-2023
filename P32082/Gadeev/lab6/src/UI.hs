module UI
    ( buildUI
    ) where

import Control.Lens
import Monomer
import Monomer.Graph
import TextShow

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = tree where
    tree = hstack_ [childSpacing_ 16]
        [ graphWithData_ points
            [ lockX_ $ model ^. xLock
            , lockY_ $ model ^. yLock
            ] `nodeKey` "mainGraph"
        , separatorLine
        , vstack_ [childSpacing_ 16]
            [ button "Reset" AppResetGraph
            , hgrid_ [childSpacing_ 64]
                [ labeledCheckbox "Lock X" xLock
                , labeledCheckbox "Lock Y" yLock
                ]
            , separatorLine
            , methodDropdown
            , widgetIf (model ^. calcMethod `elem` [Adams, Milne]) $
                hstack_ [childSpacing_ 16]
                    [ label "Get y1, y2, y3 using"
                    , preMethodDropdown
                    ]
            , equationDropdown
            , label $ snd currentSolution
            , hstack
                [ label "Error estimation: "
                , labelS $ model ^. errorEstimation
                ]
            , separatorLine
            , hgrid_ [childSpacing_ 16]
                [ optionButton "Parameters" MParameters currentMenu
                , optionButton "Points" MPoints currentMenu
                ]
            , separatorLine
            , case model ^. currentMenu of
                MParameters -> menuParameters
                MPoints -> menuPoints
            ] `styleBasic` [sizeReqW $ fixedSize 300]
        ] `styleBasic` [padding 16]
    menuParameters = vstack_ [childSpacing_ 16]
        [ hgrid_ [childSpacing_ 64]
            [ hstack_ [childSpacing_ 16]
                [ label "a = "
                , numericField_ pointA [onChange
                    (const AppInit :: Double -> AppEvent)]
                ]
            , button "Reset" AppResetA
            ]
        , hgrid_ [childSpacing_ 64]
            [ hstack_ [childSpacing_ 16]
                [ label "b = "
                , numericField_ pointB [onChange
                    (const AppInit :: Double -> AppEvent)]
                ]
            , button "Reset" AppResetB
            ]
        , hgrid_ [childSpacing_ 64]
            [ hstack_ [childSpacing_ 16]
                [ label "e = "
                , numericField_ accuracy
                    [ decimals 5
                    , minValue 0.00001
                    , onChange
                        (const AppInit :: Double -> AppEvent)
                    ]
                ]
            , button "Reset" AppResetAccuracy
            ]
        , hgrid_ [childSpacing_ 64]
            [ hstack_ [childSpacing_ 16]
                [ label "h = "
                , numericField_ stepSize
                    [ decimals 5
                    , minValue 0.01
                    , onChange
                        (const AppInit :: Double -> AppEvent)
                    ]
                ]
            , button "Reset" AppResetStepSize
            ]
        ]
    menuPoints = vscroll $ vstack_ [childSpacing_ 16] $
        [ hgrid_ [childSpacing_ 16]
            [ label "X:"
            , label "Y:"
            ]
        ] <> pointPanels
    pointPanels = makePointPanel <$> [0..length ps-1]
    makePointPanel i = hgrid_ [childSpacing_ 16]
        [ numericField_ (pointField i . _1) [decimals 5, readOnly]
        , numericField_ (pointField i . _2) [decimals 5, readOnly]
            `nodeKey` (showt i)
        ]
    pointField i = lens getter setter where
        getter = (^?! ix i) . _amComputationPoints
        setter = flip $ set $ computationPoints . ix i
    points =
        [
            [ graphPoints $ solutionF <$> xs
            , graphColor red
            ]
        ,   [ graphPoints ps
            , graphColor blue
            ]
        ,   [ graphPoints ps
            , graphColor blue
            , graphSeparate
            , graphOnClick AppPointClicked
            ]
        ,   [ graphPoint (model ^. pointA, 0)
            , graphColor blue
            , graphHoverColor lightBlue
            , graphActiveColor darkBlue
            , graphWidth 4
            , graphSeparate
            , graphOnChange AppChangePointA
            ]
        ,   [ graphPoint (model ^. pointB, 0)
            , graphColor green
            , graphHoverColor lightGreen
            , graphActiveColor darkGreen
            , graphWidth 4
            , graphSeparate
            , graphOnChange AppChangePointB
            ]
        ]
    solutionF x = (x, (fst currentSolution) x)
    currentSolution = solutions!!(model ^. currentEquation)
    ps = model ^. computationPoints
    xs = [-10, -9.98..10]
    methodDropdown = dropdown_
        calcMethod
            [ Euler
            , Midpoint
            , RungeKutta
            , Adams
            , Milne
            ]
        methodTitle methodTitle
        [onChange (const AppInit :: Method -> AppEvent)]
    preMethodDropdown = dropdown_
        preCalcMethod
            [ Nothing
            , Just Euler
            , Just Midpoint
            , Just RungeKutta
            ]
        preMethodTitle preMethodTitle
        [onChange (const AppInit :: Maybe Method -> AppEvent)]
    equationDropdown = dropdown_
        currentEquation
        [0..length equations-1]
        equationTitle equationTitle
        [onChange (const AppInit :: Int -> AppEvent)]
    methodTitle t = label $ case t of
        Euler -> "Euler"
        Midpoint -> "Modified Euler (Midpoint)"
        RungeKutta -> "Runge-Kutta"
        Adams -> "Adams"
        Milne -> "Milne"
    preMethodTitle Nothing = label "Exact values"
    preMethodTitle (Just t) = methodTitle t
    equationTitle t = label $ snd $ equations!!t
