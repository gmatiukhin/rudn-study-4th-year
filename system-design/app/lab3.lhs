\documentclass[12pt]{article}
\usepackage[T1, T2A]{fontenc}
\usepackage[utf8]{inputenc}
\usepackage[russian]{babel}
\usepackage{hyperref}
\usepackage{datetime}
\usepackage{amsmath}
\usepackage{amsfonts}

\usepackage{ifthen}
\usepackage{tikz}
\usetikzlibrary{shapes.geometric, arrows.meta, bending, math}

\tikzstyle{node} = [circle, text centered, draw, minimum width=0.5cm, minimum height=0.5cm]
\tikzstyle{arrow} = [thick,->,>=stealth]

\usepackage{minted}
\newminted[code]{haskell}{}
\newminted[spec]{haskell}{}

\author{Григорий Матюхин}
\date{\formatdate{24}{09}{2024}}
\title{Lab2}
\begin{document}
\maketitle
\newpage
\tableofcontents
\newpage
\begin{code}
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy
import Text.Printf
import System.Directory

factorial :: Integer -> Integer
factorial 0 = 1
factorial i = i * factorial (i - 1)

stationaryProbabilityDistribution :: Double -> Double -> Integer -> Integer -> Integer -> Integer -> Double
stationaryProbabilityDistribution a1 a2 n1_max n2_max n1 n2 = f n1 n2 * p0
  where
    p0 = sum [f x y | x <- [0 .. n1_max], y <- [0 .. n2_max]] ** (-1)
    f :: Integer -> Integer -> Double
    f x y = g a1 n1 * g a2 n2 * fromInteger (factorial (x + y))
    g :: Double -> Integer -> Double
    g a n = a ^ n / fromInteger (factorial n)

timeBlockingProbability :: Double
timeBlockingProbability = 0

averageRequests :: Double -> Double -> Double -> Double -> Double -> Double -> Double
averageRequests lambda1 lambda2 theta1 theta2 lambda theta =
  lambda * (theta / (theta1 * lambda1 + theta2 * lambda2))

averageTime :: Double -> Double -> Double
averageTime avg_req lambda = avg_req / lambda

main :: IO ()
main = do
  let lambda1 = 8
      lambda2 = 6
      theta1 = 2
      theta2 = 3
      c = 10
      n1_max = 20
      n2_max = 30
      rho1 = lambda1 * theta1
      rho2 = lambda2 * theta2
      a1 = rho1 / c
      a2 = rho2 / c
      dist = [stationaryProbabilityDistribution a1 a2 n1_max n2_max n1 n2 | n1 <- [0 .. n1_max], n2 <- [0 .. n2_max]]
      req_avg1 = averageRequests lambda1 lambda2 theta1 theta2 lambda1 theta1
      req_avg2 = averageRequests lambda1 lambda2 theta1 theta2 lambda2 theta2
  print "Probability distribution"
  print dist
  printf "Prob sum %f\n" $ sum dist
  printf "Average requests 1: %f\n" req_avg1
  printf "Average requests 2: %f\n" req_avg2
  printf "Average time 1: %f\n" $ averageTime req_avg1 lambda1
  printf "Average time 2: %f\n" $ averageTime req_avg2 lambda2
  printf "Time blocking probability (E): %f\n" timeBlockingProbability
  createDirectoryIfMissing True "assets/lab3"
  toFile def "assets/lab3/avg_time.png" $ do
    layout_title .= "avg_time"
    setColors [opaque blue, opaque red]
    let avg_req x = averageRequests x lambda2 theta1 theta2 x theta1
     in plot (line "avg_time" [[(x, averageTime (avg_req x) x) | x <- [0.5, 1 .. 100]]])
  toFile def "assets/lab3/avg_num.png" $ do
    layout_title .= "avg_num"
    setColors [opaque blue, opaque red]
    let avg_req x = averageRequests x lambda2 theta1 theta2 x theta1
     in plot (line "avg_num" [[(x, avg_req x) | x <- [0, 0.5 .. 100]]])
\end{code}
\end{document}
