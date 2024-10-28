\documentclass[11pt]{article}
\usepackage{geometry}
 \geometry{
 a4paper,
 total={170mm,257mm},
 left=20mm,
 top=20mm,
 }
\usepackage[T1, T2A]{fontenc}
\usepackage[utf8]{inputenc}
\usepackage[russian]{babel}
\usepackage{hyperref}
\usepackage{datetime}
\usepackage{makecell}
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{amsfonts}
\usepackage{scrextend}
\usepackage{seqsplit}
\usepackage{multicol}

\usepackage{ifthen}
\usepackage{tikz}
\usetikzlibrary{shapes.geometric, arrows.meta, bending, math, decorations.pathreplacing}
\usepackage{forloop}

\tikzstyle{circ} = [circle, text centered, draw, minimum size=0.7cm, inner sep=0cm]
\tikzstyle{smallCirc} = [circle, text centered, draw, fill=black, minimum size=0.2cm, inner sep=0cm]
\tikzstyle{state} = [rectangle, text centered, draw, minimum size=1cm]
\tikzstyle{node} = [circle, text centered, draw, minimum width=0.5cm, minimum height=0.5cm]
\tikzstyle{arrow} = [thick,->,>=stealth]

\pgfmathdeclarefunction{myfunct}{1}{\pgfmathparse{(#1+7)/2}}

\usepackage{minted}
\newminted[code]{haskell}{}
\newminted[spec]{haskell}{}

\usepackage{svg}
\usepackage{graphicx}
\graphicspath{ {assets/lab4/images/} }

\author{Григорий Матюхин}
\date{\today}
\title{
	Отчет по лабораторной работе \textnumero4: \\
	\large Неполнодоступная двухсервисная модель Эрланга с резервированием для заявок
второго типа и разными интенсивностями обслуживания.}

\begin{document}
\maketitle
\newpage
\tableofcontents
\newpage

\section{Описание модели}

\tikzmath{
  % Helpful booleans
  \True = 1;
  \False = 0;
  % Task parameters
  \C = 4;
  \g = 3;
  \lambdal = 1;
  \lambdaZ = 2;
  \m = 2;
  \rhol = \lambdal / \m;
  \rhoZ = \lambdaZ / \m;
  % Edge function for the possible state space
  function edge(\x) { return \C - \x; };
  % So that we don't loop too much
  \drawLimit = 10;
  % This limit is a higher because we need some hidden nodes
  % to draw the visible ones correctly
  \prevLimit = \drawLimit - 1;
  function floorEdge(\x) { return floor(edge(\x)); };
  \maxX = \g;
  function maxY(\l) {
    \start = floorEdge(0);
    \end = floorEdge(\l);
    int \m;
    if \start > \end then {
      \m = \start;
    } else {
      \m = \end;
    };
    return \m;
  };
  \maxY = maxY(\maxX);
  function inverseFloorEdge(\y) {
    int \currY, \prevY, \ret, \isRetSet;
    \isRetSet = \False;
    \ret = 0;
    \currY = floorEdge(0);
    for \x in {0,...,\maxX + 1} {
      \prevY = \currY;
      \currY = floorEdge(\x);
      if \prevY == \y then {
        if or(not(\isRetSet), \x != \ret) then {
          \ret = \x - 1;
          \isRetSet = \True;
        };
      };
      if and(\currY < \y, \y < \prevY) then {
        \ret = \x - 1;
        \isRetSet = \True;
      };
    };
    if not(\isRetSet) then {
      \ret = \maxY;
    };
    return \ret;
  };
  function isUnderEdge(\x,\y) {
    int \v;
    \v = floorEdge(\x);
    if \y <= \v then { return \True; } else { return \False; };
  };
  function isStrictUnderEdge(\x,\y) {
    int \v;
    \v = floorEdge(\x);
    if \y < \v then { return \True; } else { return \False; };
  };
  function isStrictUnderInverseEdge(\x,\y) {
    int \v;
    \v = inverseFloorEdge(\y);
    if \x < \v then { return \True; } else { return \False; };
  };
  int \x, \y, \cnt;
  \cnt = 0;
  for \x in {0,...,\maxX} {
    for \y in {0,...,\maxY} {
      \isUnder = \False;
      if and(\x <= \drawLimit, \y <= \drawLimit) then {
        \isUnder = isUnderEdge(\x,\y);
        if \isUnder then {
          \cnt = \cnt + 1;
        };
      };
      \under{\x,\y} = \isUnder;
    };
  };
}

Рассматривается неполнодоступная двухсервисная модель Эрланга с разными интенсивностями
обслуживания и резервированием для обслуживания запросов на предоставление услуги 2-го типа:
$C=\C, g = \g, \rho_1 = \rhol, \rho_2 = \rhoZ$.
Запросы на предоставление услуги 2-го типа сначала заполняют зарезервированную емкость.

\subsection{Пространство состояний}

\begin{gather*}
  \mathcal{X} = \left\{(n_1, n_2): 0 \leq n_1 \leq g, 0 \leq
  \begingroup
  \let\clearpage\relax
  \include{assets/lab4/tex/fun.tex}
  \endgroup
  , C = \C, g = \g\right\}, \|\mathcal{X}\| = \cnt
\end{gather*}

\subsection{Схема модели}

\begin{tikzpicture}[]
  \foreach \x in {0,...,\maxX}
    \foreach \y in {0,...,\maxY}{
      \ifnum \numexpr \under{\x,\y} = \True
        \node [node] (\x\y) at (2*\x,2*\y) {$\x,\y$};
      \fi
    }

  % vertical
  \foreach \x [count=\xi] in {0,...,\maxX}
    \foreach \y [count=\yi] in {0,...,\numexpr\maxY - 1\relax}{
      \ifnum \numexpr \under{\x,\yi} = \True
        \begin{scope}[transform canvas={xshift=-.3em}]
          \draw [arrow] (\x\y) -- node[anchor=east] {$\lambda_2$} (\x\yi);
        \end{scope}
        \begin{scope}[transform canvas={xshift=.3em}]
          \draw [arrow] (\x\yi) -- node[anchor=west] {$\yi\mu_2$} (\x\y);
        \end{scope}
      \fi
    }

  % horizontal
  \foreach \x [count=\xi] in {0,...,\numexpr\maxX - 1\relax}
    \foreach \y [count=\yi] in {0,...,\maxY}{
      \ifnum \numexpr \under{\xi,\y} = \True
        \begin{scope}[transform canvas={yshift=.3em}]
          \draw [arrow] (\x\y) -- node[anchor=south] {$\lambda_1$} (\xi\y);
        \end{scope}
        \begin{scope}[transform canvas={yshift=-.3em}]
          \draw [arrow] (\xi\y) -- node[anchor=north] {$\xi\mu_1$} (\x\y);
        \end{scope}
      \fi
    }
\end{tikzpicture}

\subsection{Множества блокировок запросов $B_1, B_2$}

\tikzmath{
  print {$B_1 = \{$};
  int \x, \y;
  for \y in {0,...,\maxY} {
    \x = inverseFloorEdge(\y);
    if \x > \maxX then {
      \x = \maxX;
    };
    print {$(\x, \y)$, };
  };
  print {$\}$\\};
  print {$B_2 = \{$};
  for \x in {0,...,\maxX} {
    \y = floorEdge(\x);
    if \y >= 0 then {
      print {$(\x, \y)$, };
    };
  };
  print {$\}$};
}

\subsection{Множества приемов запросов $S_1, S_2$}

\tikzmath{
  print {$S_1 = X \setminus B_1 = \{$};
  int \x, \y;
  for \x in {0,...,\maxX} {
    for \y in {0,...,\maxY} {
      \isUnder = isStrictUnderInverseEdge(\x,\y);
      if and(\isUnder, \x < \maxX) then {
        print {$(\x, \y)$, };
      };
    };
  };
  print {$\}$\\};
  print {$S_2 = X \setminus B_2 = \{$};
  for \x in {0,...,\maxX} {
    for \y in {0,...,\maxY} {
      \isUnder = isStrictUnderEdge(\x,\y);
      if and(\isUnder, \y < \maxY) then {
        print {$(\x, \y)$, };
      };
    };
  };
  print {$\}$};
}

\subsection{Распределение вероятностей состояния системы $p(n_1, n_2)$}

\begin{multicols}{3}
  \noindent
  \begingroup
  \let\clearpage\relax
  \include{assets/lab4/tex/prob.tex}
  \endgroup
\end{multicols}

\newpage
\section{Численный анализ}

\subsection{Как устроена эта программа}

Исходный код этого очета, написанный с использованием \LaTeX{} и исходный код этой программы
--- один и тот же файл.
Это возможно, потому что Haskell поддерживает "Грамотное программирование" в файлах \texttt{.lhs}.
Это значит, что компилятор смотрит только на текст в специально обозначенных местах.
В случае Haskell код должен находится между \verb!\begin{code}! и \verb!\end{code}!.

Например этот кусок файла будет просто текстом для \LaTeX{} компилятора,
но программой, выводящей \texttt{hello} в консоль для Haskell компилятора:

\noindent
\verb!\begin{code}!\\
\verb!  main :: IO ()!\\
\verb!  main = do!\\
\verb!    putStrLn "hello"!\\
\verb!\end{code}!

\subsection{Реализация}

Сначала импортируем нужные нам библиотеки:

\begin{code}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy
import Text.Printf
import System.Directory
import Text.Regex.Posix
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Text as T
import System.IO
import Data.Maybe
import Data.Typeable (Typeable)
import qualified Language.Haskell.Interpreter as Hint
\end{code}

Определим функцию стационарного распределения вероятностей состояний системы 
\begin{code}
stationaryProbabilityDistribution
    :: Double -> Double -> (Double -> Double) -> Double -> Double -> Double -> Double
stationaryProbabilityDistribution rho1 rho2 edge maxX n1 n2 = p0 * f n1 n2
  where
    p0 = sum [f x y | x <- [0 .. maxX], y <- [0 .. edge x]] ** (-1)
    f :: Double -> Double -> Double
    f x y = g rho1 x * g rho2 y
    g :: Double -> Double -> Double
    g rho n = rho ** n / factorial n
      where
        factorial :: Double -> Double
        factorial 0 = 1
        factorial i = i * factorial (i - 1)
\end{code}

Определим вероятность блокировки по времени для запросов на представление услуг 1-го типа 
\begin{code}
timeBlockingProbability1 :: (Double -> Double) -> Double -> (Double -> Double -> Double) -> Double
timeBlockingProbability1 inv limit statProbDist
  = sum [statProbDist i j | j <- [0 .. limit], i <- [inv j]]

timeBlockingProbability2 :: (Double -> Double) -> Double -> (Double -> Double -> Double) -> Double
timeBlockingProbability2 edge limit statProbDist
  = sum [statProbDist i j | i <- [0 .. limit], j <- [edge i]]
\end{code}

Посчитаем среднее число обслуживаемых запросов
\begin{code}
averageRequests1 :: (Double -> Double) -> Double -> (Double -> Double -> Double) -> Double
averageRequests1 inv limit statProbDist
  = sum [i * statProbDist i j | j <- [0 .. limit], i <- [0 .. inv j]]

averageRequests2 :: (Double -> Double) -> Double -> (Double -> Double -> Double) -> Double
averageRequests2 edge limit statProbDist
  = sum [j * statProbDist i j | i <- [0 .. limit], j <- [0 .. edge i]]
\end{code}

\subsection{Входные параметры}

Для расчета основных вероятностных характеристик модели были взяты следующие параметры:

\begin{tabular}{c c c}
  $C$ & = & \C \\
  $\rho_1$ & = & \rhol \\
  $\rho_2$ & = & \rhoZ
\end{tabular}

В исходном коде они выглядят вот так:

\noindent
\verb!  \begin{tabular}{c c c}!\\
\verb!    $C$ & = & \C \\!\\
\verb!    $\rho_1$ & = & \rhol \\!\\
\verb!    $\rho_2$ & = & \rhoZ!\\
\verb!  \end{tabular}!

Это отличается от предыдущих лабораторных работ.

\begin{code}
replace :: String -> String -> String -> String
replace a b str = T.unpack $ T.replace (T.pack a) (T.pack b) $ T.pack str

data TaskParams
  = TaskParams{ function    :: IO (Either Hint.InterpreterError (Double -> Double))
              , rawFunction :: String
              , variables   :: M.Map String Double }

taskParams :: String -> TaskParams
taskParams haystack = TaskParams{ function  = func
                                , rawFunction = raw
                                , variables = vars }
  where
    (func, raw) = getEdgeFun
    getEdgeFun ::(IO (Either Hint.InterpreterError (Double -> Double)), String)
    getEdgeFun = (eval @(Double -> Double)
                 $ prepFun
                 $ replaceVariables ( M.toList vars) rawFunc
                 , rawFunc)
      where
        rawFunc = escape findEdgeFun
        eval :: forall t. Typeable t => String -> IO (Either Hint.InterpreterError t)
        eval s = Hint.runInterpreter $ do
          Hint.setImports ["Prelude"]
          Hint.interpret s (Hint.as :: t)
        prepFun :: String -> String
        prepFun fun = "\\x -> fromIntegral $ floor $ " ++ fun
        escape = replace "\\" " "
        replaceVariables :: [(String, Double)] -> String -> String
        replaceVariables [] fun  = fun
        replaceVariables vs fun
          = replaceVariables (tail vs) (replace name (show value) fun)
          where
            (name, value) = head vs
        findEdgeFun :: String
        findEdgeFun = head subMatches
          where
            (_, _, _, subMatches) = haystack =~ funRegex
                :: (String, String, String, [String])
            funRegex
              = "^\\s*function\\s+edge\\(\\\\x\\)\\s+\\{\\s*return\\s+(.*);\\s*\\};\\s*$"
    getVars :: String -> M.Map String Double
    getVars = M.map read . M.fromList . map getParts . getLines
      where
        getLines :: String -> [String]
        getLines fileCont = getAllTextMatches (fileCont =~ valueRegex)
        getParts :: String -> (String, String)
        getParts valueLine = (head subMatches, subMatches !! 1)
          where
            (_, _, _, subMatches) = valueLine =~ valueRegex
                :: (String, String, String, [String])
        valueRegex = "^\\s*\\\\([a-zA-Z0-9{}]+)\\s*=\\s*([0-9.]+);\\s*$"
    vars = getVars haystack

getInverseEdge :: (Double -> Double) -> Double -> (Double -> Double)
getInverseEdge edge limit = inverseEdge
  where
    inverseEdge :: Double -> Double
    inverseEdge y = last (valueTable !! floor y)
    valueTable = map L.sort $ foldl f (replicate maxY []) [(x, y) | x <- [0 .. limit], y <- [0 .. edge x]]
      where
        f :: [[Double]] -> (Double, Double) -> [[Double]]
        f acc (x, y) = zipWith (curry g) acc [0 .. length acc]
          where
            g :: ([Double], Int) -> [Double]
            g (lst, idx)
              | fromIntegral idx == y = lst ++ [x]
              | otherwise             = lst
        maxY = floor $ max (edge 0) (edge limit) + 1
\end{code}

\subsection{Вывод данных}

\subsubsection{Графики}

Нам необходимо показать несколько графиков, поэтому составим функции для них:

\begin{code}
prepFileName :: String -> String
prepFileName = replace " " "_"

plotBlockingProb
  :: Double -> Double -> (Double -> Double) -> Double -> FilePath -> String -> IO ()
plotBlockingProb mu statRho edge limit dir name =
  toFile def (dir ++ prepFileName name ++ ".svg") $ do
    layout_title .= name
    setColors [opaque blue]
    let spd l = stationaryProbabilityDistribution (l / mu) statRho edge limit
      in plot (line "" [[(l, sum [spd l i j | i <- [0 .. limit], j <- [edge i]]) | l <- [0 .. 100]]])

plotAverageRequests
  :: Double -> Double -> (Double -> Double) -> Double -> FilePath -> String -> IO ()
plotAverageRequests mu statRho edge limit dir name =
  toFile def (dir ++ prepFileName name ++ ".svg") $ do
    layout_title .= name
    setColors [opaque blue, opaque red]
    let spd l = stationaryProbabilityDistribution (l / mu) statRho edge limit
      in plot (line "" [[(l, averageRequests1 edge limit (spd l) + averageRequests2 edge limit (spd l)) | l <- [0 .. 100]]])
\end{code}

Будем сохранять их в специальную директорию,
а затем импортировать при генерации отчета
используя \verb!\includegraphics!.

\subsubsection{Текстовый вывод}

Чтобы показывать текстовый вывод программы в финальном отчете
запишем его в отдельный \LaTeX{} файл, а затем включим его в отчет
используя \verb!\include!.

Но сначала, нам нужна функция, которая подсчитает интересующие нас
параметры и запишет их в отдельный файл.

\begin{code}
writeProbDist
  :: Double -> Double -> Double -> (Double -> Double) -> FilePath -> IO ()
writeProbDist rho1 rho2 maxX edge fname = do
  writeFile fname
    $ printf "%% Automatically generated output file!\n"
      ++ concat [format n1 n2 (stationaryProbabilityDistribution rho1 rho2 edge maxX n1 n2)
        | n1 <- [0 .. maxX], n2 <- [0 .. edge n1]]
  where
    format :: Double -> Double -> Double -> String
    format n1 n2 = printf "$(%d, %d) = %.5f$\\\\\n" (floor n1 :: Int) (floor n2 :: Int)

writeSummary
  :: Double -> Double -> Double -> Double -> (Double -> Double) -> (Double -> Double) -> FilePath -> IO ()
writeSummary rho1 rho2 maxX maxY edge inv fname = do
  let statProbDist = stationaryProbabilityDistribution rho1 rho2 edge maxX
      avgReq1 = averageRequests1 inv maxY statProbDist
      avgReq2 = averageRequests2 edge maxX statProbDist
  writeFile fname
    $ printf "%% Automatically generated output file!\n"
    ++ printf "\\begin{tabular}{l l l l}\n"
    ++ printf "Вероятность блокировки по времени & $E_1$ & = & %f \\\\\n"
        (timeBlockingProbability1 inv maxY statProbDist)
    ++ printf "Вероятность блокировки по времени & $E_2$ & = & %f \\\\\n"
        (timeBlockingProbability2 edge maxX statProbDist)
    ++ printf "Среднее число запросов типа 1 & $\\overline{N_1}$ & = & %f \\\\\n"
        avgReq1
    ++ printf "Среднее число запросов типа 2 & $\\overline{N_2}$ & = & %f \\\\\n"
        avgReq2
    ++ printf "Среднее число запросов обоих типов& $\\overline{N}$ & = & %f\n"
        (avgReq1 + avgReq2)
    ++ printf "\\end{tabular}"

writeEdgeFunToOutput :: String -> FilePath -> IO ()
writeEdgeFunToOutput func fname = do
  writeFile fname
    $ printf "%% Automatically generated output file!\n"
    ++ prepFun func
  where
    prepFun :: String -> String
    prepFun fun = "n_2 \\leq" ++ replace "x" "n_1" fun
\end{code}

\subsection{\texttt{main} функция}

Тепеь напишем функцию \texttt{main}, которя является точкой входа в программу.

\begin{code}
main :: IO ()
main = do
    handle <- openFile "./app/lab4.lhs" ReadMode
    contents <- hGetContents handle 
    let params = taskParams contents
    Right edge <- function params
    let vars = variables params
        rawFunc = rawFunction params
        g = fromJust $ M.lookup "g" vars
        lambda1 = fromJust $ M.lookup "lambdal" vars
        lambda2 = fromJust $ M.lookup "lambdaZ" vars
        mu = fromJust $ M.lookup "m" vars
        rho1 = lambda1 / mu
        rho2 = lambda2 / mu
        labDir = "assets/lab4/"
        texDir = labDir ++ "tex/"
        imgDir = labDir ++ "images/"
        maxX = g
        inv = getInverseEdge edge maxX
        maxY = fromInteger $ floor $ max (edge 0) (edge maxX)
    createDirectoryIfMissing True texDir
    writeEdgeFunToOutput rawFunc (texDir ++ "fun.tex")
    writeProbDist rho1 rho2 g edge (texDir ++ "prob.tex")
    writeSummary rho1 rho2 maxX maxY edge inv (texDir ++ "output.tex")
    createDirectoryIfMissing True imgDir 
    plotBlockingProb mu rho1 edge maxX imgDir "Blocking Probability (E2) for lambda2"
    plotBlockingProb mu rho2 edge maxX imgDir "Blocking Probability (E2) for lambda1"
    plotBlockingProb mu rho1 inv maxY imgDir "Blocking Probability (E1) for lambda2"
    plotBlockingProb mu rho2 inv maxY imgDir "Blocking Probability (E1) for lambda1"
    plotAverageRequests mu rho1 inv maxY imgDir "Average Requests (N) for lambda2"
    plotAverageRequests mu rho2 inv maxY imgDir "Average Requests (N) for lambda1"
\end{code}

\section{Результаты}

\begingroup
\let\clearpage\relax
\include{assets/lab4/tex/output.tex}
\endgroup

\includesvg[width=220pt]{Blocking_Probability_(E1)_for_lambda1.svg}
\includesvg[width=220pt]{Blocking_Probability_(E1)_for_lambda2.svg}

\includesvg[width=220pt]{Blocking_Probability_(E2)_for_lambda1.svg}
\includesvg[width=220pt]{Blocking_Probability_(E2)_for_lambda2.svg}

\includesvg[width=220pt]{Average_Requests_(N)_for_lambda1.svg}
\includesvg[width=220pt]{Average_Requests_(N)_for_lambda2.svg}
\end{document}
