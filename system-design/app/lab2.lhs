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

stationaryProbabilityDistribution :: Double -> Double -> Integer -> Integer -> Integer -> Double
stationaryProbabilityDistribution rho1 rho2 c g n
  | 1 <= n && n <= g = p0 * t1 n
  | otherwise = p0 * t2 n
  where
    t1 :: Integer -> Double
    t1 a = (rho1 + rho2) ^ a / fromInteger (factorial a)
    t2 :: Integer -> Double
    t2 a = (rho1 + rho2) ^ g * rho1 ^ (a - g) / fromInteger (factorial a)
    p0 = (sum [t1 i | i <- [0 .. g]] + sum [t2 i | i <- [g + 1 .. c]]) ** (-1)

averageRequests :: (Integer -> Double) -> Integer -> Double
averageRequests distrib c =
  sum [fromInteger i * distrib i | i <- [1 .. c]]

timeBlockingProbability1 :: (Integer -> Double) -> Integer -> Double
timeBlockingProbability1 distrib = distrib

timeBlockingProbability2 :: (Integer -> Double) -> Integer -> Integer -> Double
timeBlockingProbability2 distrib g c = sum [distrib i | i <- [g .. c]]

main :: IO ()
main = do
  let lambda1 = 80
      lambda2 = 50
      mu = 2
      c = 60
      g = 40
      mu1 = mu
      mu2 = mu
      rho1 = lambda1 / mu1
      rho2 = lambda2 / mu2
      distrib = stationaryProbabilityDistribution rho1 rho2 c g
      dist = [distrib p | p <- [1 .. c]]
  print "Probability distribution"
  print dist
  printf "Prob sum %f\n" $ sum dist
  printf "Average requests: %f\n" $ averageRequests distrib c
  printf "Time blocking probability (E1): %f\n" $ timeBlockingProbability1 distrib c
  printf "Time blocking probability (E2): %f\n" $ timeBlockingProbability2 distrib g c
  createDirectoryIfMissing True "assets/lab2"
  toFile def "assets/lab2/tbp.png" $ do
    layout_title .= "Tbp"
    setColors [opaque blue, opaque red]
    let dist l = stationaryProbabilityDistribution (l / mu) 0 c g
        tbp distrib = timeBlockingProbability1 distrib
     in plot (line "tbp" [[(x, tbp (dist x) c) | x <- [0, 0.5 .. 100]]])
  toFile def "assets/lab2/avg.png" $ do
    layout_title .= "Avg"
    setColors [opaque blue, opaque red]
    let dist l = stationaryProbabilityDistribution (l / mu) 0 c g
        avg distrib = averageRequests distrib
     in plot (line "avg" [[(x, avg (dist x) c) | x <- [0, 0.5 .. 100]]])
\end{code}
\end{document}
