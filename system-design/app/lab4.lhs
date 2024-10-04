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

\usepackage{ifthen}
\usepackage{tikz}
\usetikzlibrary{shapes.geometric, arrows.meta, bending, math, decorations.pathreplacing}
\usepackage{forloop}

\tikzstyle{circ} = [circle, text centered, draw, minimum size=0.7cm, inner sep=0cm]
\tikzstyle{smallCirc} = [circle, text centered, draw, fill=black, minimum size=0.2cm, inner sep=0cm]
\tikzstyle{state} = [rectangle, text centered, draw, minimum size=1cm]
\tikzstyle{node} = [circle, text centered, draw, minimum width=0.5cm, minimum height=0.5cm]
\tikzstyle{arrow} = [thick,->,>=stealth]

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
  \maxSum = 7;
  \maxSumPrev = \maxSum - 1;
  \maxSumSucc = \maxSum + 1;
  \C = 4;
  \g = 3;
  \rhol = 0.5;
  \rhoz = 1;
}

Рассматривается неполнодоступная двухсервисная модель Эрланга с разными интенсивностями
обслуживания и резервированием для обслуживания запросов на предоставление услуги 2-го типа:
$C=\C, g = \g, \rho_1 = \rhol, \rho_2 = \rhoz$.
Запросы на предоставление услуги 2-го типа сначала заполняют зарезервированную емкость.

\subsection{Пространство состояний}

\begin{gather*}
\mathcal{X} = \left\{(n_1, n_2): n_1 = \overline{0,5}, n_2 = \overline{0,3}, n_1 + 2 \times n_2 < \maxSum\right\}, \|\mathcal{X}\| = 16
\end{gather*}

\subsection{Схема модели}

\begin{tikzpicture}[]
  \foreach \x in {0,...,6}
    \foreach \y in {0,...,6}{
      \ifnum \numexpr \x + 2 * \y \relax < \maxSum
        \node [node] (\x\y) at (2*\x,2*\y) {$\x,\y$};
      \fi
    }

  \foreach \x [count=\xi] in {0,...,6}
      \foreach \y [count=\yi] in {0,...,6}{

        % vertical
        \ifnum \numexpr \x + 2 * \yi \relax < \maxSum \begin{scope}[transform canvas={xshift=-.3em}]
            \draw [arrow] (\x\y) -- node[anchor=east] {$\lambda_2$} (\x\yi);
          \end{scope}
          \begin{scope}[transform canvas={xshift=.3em}]
            \draw [arrow] (\x\yi) -- node[anchor=west] {$\yi\mu_2$} (\x\y);
          \end{scope}
        \fi

        % horizontal
        \ifnum \numexpr \xi + 2 * \y \relax < \maxSum
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

\begin{gather}
  B_1 = {(n_1, n_2) \in X: \maxSum \leq n_1 + 2 \times (n_2 + 1) \leq \pgfmathprintnumber{\maxSumSucc}}=\\
  = \left\{\right\}
  B_2 = {(n_1, n_2) \in X: (n_1 + 1) + 2 \times n_2 = \maxSum}
\end{gather}

\subsection{Множества приемов запросов $S_1, S_2$}

\begin{gather}
  S_1 = X \setminus B_1 \\
  S_2 = X \setminus B_2
\end{gather}

\subsection{Распределение вероятностей состояния системы $p(n_1, n_2)$}

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
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy
import Text.Printf
import System.Directory
import Text.Regex.Posix
import qualified Data.Map as M
import System.IO
import Data.Maybe
import qualified Data.Text as T
\end{code}

Определим функцию стационарного распределения вероятностей состояний системы 
\begin{code}
stationaryProbabilityDistribution
    :: Double -> Double -> Double -> Double -> Double -> Double -> Double
stationaryProbabilityDistribution a1 a2 n1_max n2_max n1 n2 = f n1 n2 * p0
  where
    p0 = sum [f x y | x <- [0 .. n1_max], y <- [0 .. n2_max]] ** (-1)
    f :: Double -> Double -> Double
    f x y = g a1 n1 * g a2 n2 * factorial (x + y)
    g :: Double -> Double -> Double
    g a n = a ** n / factorial n
    factorial :: Double -> Double
    factorial 0 = 1
    factorial i = i * factorial (i - 1)
\end{code}

Определим вероятность блокировки по времени для запросов на представление услуг 1-го типа 
\begin{code}
timeBlockingProbability :: Double
timeBlockingProbability = 0
\end{code}

Посчитаем среднее число обслуживаемых запросов
\begin{code}
averageRequests :: Double -> Double -> Double -> Double -> Double -> Double -> Double
averageRequests lambda1 theta1 lambda2 theta2 lambda theta =
    lambda * (theta / (theta1 * lambda1 + theta2 * lambda2))
\end{code}

И среднее время их обслуживания
\begin{code}
averageTime :: (Double -> Double) -> Double -> Double
averageTime avg_req lambda = avg_req lambda / lambda
\end{code}

\subsection{Входные параметры}

Для расчета основных вероятностных характеристик модели были взяты следующие параметры:

\begin{tabular}{c c c}
  $C$ & = & 10 \\
  $\lambda_1$ & = & 8 \\
  $\lambda_2$ & = & 6 \\
  $\theta_1$ & = & 2 \\
  $\theta_2$ & = & 3 \\
  $n_1^{max}$ & = & 10 \\
  $n_2^{max}$ & = & 10
\end{tabular}

В исходном коде они выглядят вот так:

\noindent
\verb!  \begin{tabular}{c c c}!\\
\verb!    $C$ & = & 10!\\
\verb!    $\lambda_1$ & = & 8!\\
\verb!    $\lambda_2$ & = & 6!\\
\verb!    $\theta_1$ & = & 2!\\
\verb!    $\theta_2$ & = & 3!\\
\verb!    $n_1^{max}$ & = & 10!\\
\verb!    $n_2^{max}$ & = & 10!\\
\verb!  \end{tabular}!

А само представление выше, вот так:

\begin{addmargin}{1em}
\begin{verbatim}
\noindent
\verb!  \begin{tabular}{c c c}!\\
\verb!    $C$ & = & 10!\\
\verb!    $\lambda_1$ & = & 8!\\
\verb!    $\lambda_2$ & = & 6!\\
\verb!    $\theta_1$ & = & 2!\\
\verb!    $\theta_2$ & = & 3!\\
\verb!    $n_1^{max}$ & = & 10!\\
\verb!    $n_2^{max}$ & = & 10!\\
\verb!  \end{tabular}!
\end{verbatim}
\end{addmargin}

Это сделано для того, чтобы можно было только одно из определений выбиралось регулярным выражением.
(А для последнего представления использовалось окружение \texttt{verbatim}.)

Составим функцию, которая прочтет содержимое файла и найдет заданные значения параметров:

\begin{code}
getValues :: String -> M.Map String Double
getValues = M.map read . M.fromList . map getParts . getLines
  where
    getLines :: String -> [String]
    getLines fileCont = getAllTextMatches (fileCont =~ valueRegex)
    getParts :: String -> (String, String)
    getParts valueLine = (head subMatches, subMatches !! 1)
      where
        (_, _, _, subMatches) = valueLine =~ valueRegex
            :: (String, String, String, [String])
    valueRegex = "^\\s*\\$\\\\?([a-zA-Z0-9_^{}]+)\\$\\s*&\\s*=\\s*&\\s*([0-9.]+)?"
\end{code}

\subsection{Вывод данных}

\subsubsection{Графики}

Нам необходимо показать несколько графиков, поэтому составим функции для них:

\begin{code}
prepFileName :: String -> String
prepFileName = T.unpack . T.replace (T.pack " ") (T.pack "_") . T.toLower . T.pack

plotAverageTime :: (Double -> Double) -> Double -> FilePath -> String -> IO ()
plotAverageTime avg n_max dir name =
    toFile def (dir ++ prepFileName name ++ ".svg") $ do
        layout_title .= name
        setColors [opaque blue]
        plot (line "" [[(x, averageTime avg x) | x <- [0.5, 1 .. n_max]]])

plotAverageRequests :: (Double -> Double) -> Double -> FilePath -> String -> IO ()
plotAverageRequests avg n_max dir name =
    toFile def (dir ++ prepFileName name ++ ".svg") $ do
        layout_title .= name
        setColors [opaque blue, opaque red]
        plot (line "" [[(x, avg x) | x <- [0, 0.5 .. n_max]]])
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
writeTexOutput
    :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> FilePath -> IO ()
writeTexOutput l1 l2 t1 t2 n1_max n2_max c fname = do
    let rho1 = l1 * t1
        rho2 = l2 * t2
        a1 = rho1 / c
        a2 = rho2 / c
        req_avg1 l = averageRequests l t1 l2 t2 l t1
        req_avg2 l = averageRequests l1 t1 l t2 l t2
    writeFile fname
        $ printf "%% Automatically generated output file!\n"
        ++ printf "Распределение вероятностей:\\\\\n"
        ++ printf "\\seqsplit{%s}\\\\\n"
            (show [stationaryProbabilityDistribution a1 a2 n1_max n2_max n1 n2 
                | n1 <- [0 .. n1_max], n2 <- [0 .. n2_max]])
        ++ printf "\\begin{tabular}{l l l l}\n"
        ++ printf "Среднее число запросов типа 1 & $\\overline{N_1}$ & = & %f\n \\\\\n"
            (req_avg1 l1)
        ++ printf "Среднее число запросов типа 2 & $\\overline{N_2}$ & = & %f\n \\\\\n"
            (req_avg2 l2)
        ++ printf "Среднее время обслуживания типа 1 & $T_1$ & = & %f \\\\\n"
            (averageTime req_avg1 l1)
        ++ printf "Среднее время обслуживания типа 2 & $T_1$ & = & %f \\\\\n"
            (averageTime req_avg2 l2)
        ++ printf "Вероятность блокировки по времени & $E$ & = & %f\n"
            timeBlockingProbability
        ++ printf "\\end{tabular}"
\end{code}

\subsection{\texttt{main} функция}

Тепеь напишем функцию \texttt{main}, которя является точкой входа в программу.

\begin{code}
main :: IO ()
main = do
    handle <- openFile "./app/lab4.lhs" ReadMode
    contents <- hGetContents handle 
    let values = getValues contents
        l1 = fromJust $ M.lookup "lambda_1" values
        l2 = fromJust $ M.lookup "lambda_2" values
        t1 = fromJust $ M.lookup "theta_1" values
        t2 = fromJust $ M.lookup "theta_2" values
        c = fromJust $ M.lookup "C" values
        n1_max = fromJust $ M.lookup "n_1^{max}" values
        n2_max = fromJust $ M.lookup "n_2^{max}" values
        labDir = "assets/lab4/"
        texDir = labDir ++ "tex/"
        imgDir = labDir ++ "images/"
    createDirectoryIfMissing True texDir
    writeTexOutput l1 l2 t1 t2 n1_max n2_max c (texDir ++ "output.tex")
    createDirectoryIfMissing True imgDir 
    let avg1 x = averageRequests x t1 l2 t2 x t1
        avg2 x = averageRequests l1 t1 x t2 x t1
    plotAverageTime avg1 n1_max imgDir "Average Time for lambda_1"
    plotAverageTime avg2 n2_max imgDir "Average Time for lambda_2"
    plotAverageRequests avg1 n1_max imgDir "Average Requests for lambda_1"
    plotAverageRequests avg2 n2_max imgDir "Average Requests for lambda_2"
\end{code}

\section{Результаты}

\begingroup
\let\clearpage\relax
\include{assets/lab3/tex/output.tex}
\endgroup

\includesvg[width=220pt]{average_time_for_lambda_1.svg}
\includesvg[width=220pt]{average_time_for_lambda_2.svg}

\includesvg[width=220pt]{average_requests_for_lambda_1.svg}
\includesvg[width=220pt]{average_requests_for_lambda_2.svg}
\end{document}
