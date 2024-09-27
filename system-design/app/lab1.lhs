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
\title{Lab1}
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

stationaryProbabilityDistribution :: Double -> Double -> Integer -> Integer -> Double
stationaryProbabilityDistribution rho1 rho2 c n = p0 * t n
  where
    t :: Integer -> Double
    t a = (rho1 + rho2) ^ a / fromInteger (factorial a)
    p0 = sum [t i | i <- [0 .. c]] ** (-1)

averageRequests :: Double -> Double -> Integer -> Double
averageRequests rho1 rho2 c =
  sum [fromInteger i * stationaryProbabilityDistribution rho1 rho2 c i | i <- [0 .. c]]

timeBlockingProbability :: Double -> Double -> Integer -> Double
timeBlockingProbability rho1 rho2 c = stationaryProbabilityDistribution rho1 rho2 c c

requestBlockingProbability :: Double -> Double -> Double -> Double -> Double
requestBlockingProbability lambda1 lambda2 lambda timeBlockingProb = lambda / (lambda1 + lambda2) * timeBlockingProb

main :: IO ()
main = do
  let lambda1 = 50
      lambda2 = 40
      mu = 0.5
      c = 121
      mu1 = mu
      mu2 = mu
      rho1 = lambda1 / mu1
      rho2 = lambda2 / mu2
      timeBlockingProb = timeBlockingProbability rho1 rho2 c
  print "Probability distribution"
  print [stationaryProbabilityDistribution rho1 rho2 p c | p <- [0 .. c]]
  printf "Average requests: %f\n" $ averageRequests rho1 rho2 c
  printf "Time blocking probability (E): %f\n" timeBlockingProb
  printf "Request blocking probability for l1 (B1): %f\n" $ requestBlockingProbability lambda1 lambda2 lambda1 timeBlockingProb
  printf "Request blocking probability for l2 (B2): %f\n" $ requestBlockingProbability lambda1 lambda2 lambda2 timeBlockingProb
  createDirectoryIfMissing True "assets/lab1"
  toFile def "assets/lab1/tbp.png" $ do
    layout_title .= "Lab 1"
    setColors [opaque blue, opaque red]
    let tbp l = timeBlockingProbability (l / mu) 0 c
     in plot (line "tbp" [[(x, tbp x) | x <- [0, 0.5 .. 100]]])
  toFile def "assets/lab1/avg_req.png" $ do
    layout_title .= "Lab 1"
    setColors [opaque blue, opaque red]
    let avg l = averageRequests (l / mu) 0 c
     in plot (line "avg_req" [[(x, avg x) | x <- [0, 0.5 .. 100]]])
\end{code}
\end{document}
