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
\usepackage{amsfonts}
\usepackage{scrextend}
\usepackage{seqsplit}

\usepackage{ifthen}
\usepackage{tikz}
\usetikzlibrary{shapes.geometric, arrows.meta, bending, math, decorations.pathreplacing}

\tikzstyle{circ} = [circle, text centered, draw, minimum size=0.7cm, inner sep=0cm]
\tikzstyle{smallCirc} = [circle, text centered, draw, fill=black, minimum size=0.2cm, inner sep=0cm]
\tikzstyle{state} = [rectangle, text centered, draw, minimum size=1cm]
\tikzstyle{arrow} = [thick,->,>=stealth]

\usepackage{minted}
\newminted[code]{haskell}{}
\newminted[spec]{haskell}{}

\usepackage{svg}
\usepackage{graphicx}
\graphicspath{ {assets/lab2/images/} }

\author{Григорий Матюхин}
\date{\today}
\title{
	Отчет по лабораторной работе \textnumero2: \\
	\large Неполнодоступная двухсервисная модель Эрланга с одинаковыми интенсивностями обслуживания и зарезервированной емкостью.}

\begin{document}
\maketitle
\newpage
\tableofcontents
\newpage

\section{Теориетические сведения}

\subsection{Описание модели}

Рассмотрим звено сети емкостью $C$.
Пусть пользователям сети предоставляются услуги двух типов.
Запросы на предоставление услуг представляют собой ПП с интенсивностями $\lambda_1, \lambda_2$.
Среднее время обслуживания запросов каждого типа $\mu_1^{-1},\mu_2^{-1}$ соответственно.
Рассмотрим случай $\mu_1 = \mu_2 = \mu$.
Часть пропускной способности соты зарезервирована для обслуживания
запросов на предоставление услуги 1-го или 2-го типа.
Оставшаяся часть пропускной способности является полнодоступной
для запросов на предоставление услуг обоих типов.
Предположим, что сначала заполняется полнодоступная емкость.

В классификации Башарина-Кендалла:
$\underset{\lambda_1}{M}\underset{\lambda_2}{M}|\underset{\mu_1}{M}\underset{\mu_1}{M}|C,g|0$.

\begin{tabular}{l l l}
  $C$ & -- & \makecell[tl]{пиковая пропускная способность соты;} \\
  $g$ & -- & \makecell[tl]{полнодоступная часть пропускной способности соты;}\\
  $C - g$ & -- & \makecell[tl]{пропускная способность, зарезервированная для обслуживания запросов на \\
  предоставление услуги 1-го или 2-го типа;}\\
  $\lambda_1$, $\lambda_2$ & -- & \makecell[tl]{интенсивность поступления
  запросов на предоставление \\ услуги 1, 2-го типа [запросов/ед.вр.];} \\
  $\mu^{-1}$ & -- & \makecell[tl]{среднее время обслуживания запроса на предоставление \\
  услуги 1, 2-го типа [запросов/ед.вр.];} \\
  $\rho_1$, $\rho_2$ & -- & \makecell[tl]{интенсивность предложенной нагрузки, \\
  создаваемой запросами на предоставление услуги 1, 2-го типа;} \\
  $X(t)$ & -- &  \makecell[tl]{число запросов, обслуживаемых в системе в \\
  момент времени $t$, $t \geq 0$ (случайный процесс (СП), \\
  описывающий функционирование системы в момент времени $t$, $t \geq 0$ );} \\
  $X$ & -- & \makecell[tl]{пространство состояний системы;} \\
  $n$ & -- & \makecell[tl]{число обслуживаемых в системе запросов;} \\
  $B_1$, $B_2$ & -- & \makecell[tl]{множество блокировок запросов на предоставление услуги 1, 2-го типа;} \\
  $S_1$, $S_2$ & -- & \makecell[tl]{множество приема запросов на предоставление услуги 1, 2-го типа.}
\end{tabular}

Схема модели:

\begin{center}
  \begin{tikzpicture}[node distance=1cm, scale=0.7, every node/.style={scale=0.7}]
    \node (req1) at (0, -3) {\LargeУслуга 1};
    \node (req2) at (0, -6){\LargeУслуга 2};

    \node [circ] at (6, 0) (start) {};
    \node [smallCirc] at (6, -1) (sm1) {};
    \node [smallCirc] at (6, -1.5) (sm2) {};
    \node [smallCirc] at (6, -2) (sm3) {};
    \node [circ] at (6, -3) (mid1) {};

    \draw[decorate, decoration={brace, raise=10pt, amplitude=5pt}, line width=1pt] (start) -- (mid1) node [midway, anchor=west, xshift=23pt] {\Large$C-g$};

    \node [smallCirc] at (6, -4) (sm4) {};
    \node [smallCirc] at (6, -4.5) (sm5) {};
    \node [smallCirc] at (6, -5) (sm6) {};
    \node [circ] at (6, -6) (mid2) {};
    \node [smallCirc] at (6, -7) (sm7) {};
    \node [smallCirc] at (6, -7.5) (sm8) {};
    \node [smallCirc] at (6, -8) (sm9) {};
    \node [circ] at (6, -9) (end) {};

    \draw[decorate, decoration={brace, raise=10pt, amplitude=5pt}, line width=1pt] (mid1) -- (end) node [midway, anchor=west, xshift=23pt] {\Large$g$};

    \draw[decorate, decoration={brace, raise=50pt, amplitude=5pt}, line width=1.1pt, scale=1.5] (start) -- (end) node [midway, anchor=west, xshift=80pt] {\Large$C$};

    \draw [arrow] (req1) -- node[anchor=south] {\large$\lambda_1, \mu$} (3.5, -3);
    \draw [arrow] (req2) -- node[anchor=south] {\large$\lambda_2, \mu$} (3.5, -6);

    \draw (3.5, -3) -- (start);
    \draw (3.5, -3) -- (end);

    \draw (3.5, -6) -- (mid1);
    \draw (3.5, -6) -- (end);
  \end{tikzpicture}
\end{center}

Пространство состояний системы:
\begin{gather}
  X = \left\{0,..., C\right\}, \left|X\right| = C + 1
\end{gather}

\begin{center}
  \begin{tikzpicture}[node distance=2cm, scale=0.7, every node/.style={scale=0.7}]
    \node [state] (o) {$0$};
    \node [state, right of=o] (l) {$1$};
    \node [state, right of=l] (gl) {$g-1$};
    \node [state, right of=gl] (g) {$g$};
    \node [state, right of=g] (g1) {$g+1$};
    \node [state, right of=g1] (cl) {$C-1$};
    \node [state, right of=cl] (c) {$C$};

    \draw [arrow] (o) .. controls(1, 1.2) .. node [anchor=south] {$\lambda_1 + \lambda_2$} (l);
    \draw [arrow] (l) .. controls(1, -1.2) .. node [anchor=north] {$\mu$} (o);


    \draw [arrow] (gl) .. controls(5, 1.2) .. node [anchor=south] {$\lambda_1 + \lambda_2$} (g);
    \draw [arrow] (g) .. controls(5, -1.2) .. node [anchor=north] {$g\mu$} (gl);

    \draw [arrow] (g) .. controls(7, 1.2) .. node [anchor=south] {$\lambda_1$} (g1);
    \draw [arrow] (g1) .. controls(7, -1.2) .. node [anchor=north] {$(g+1)\mu$} (g);


    \draw [arrow] (cl) .. controls(11, 1.2) .. node [anchor=south] {$\lambda_1$} (c);
    \draw [arrow] (c) .. controls(11, -1.2) .. node [anchor=north] {$C\mu$} (cl);

    \draw [dashed] (l) -- (gl);
    \draw [dashed] (g1) -- (cl);
  \end{tikzpicture}
\end{center}

Множество блокировок запросов на предоставление услуги $i$-типа, $i = 1,2$:
\begin{gather}
  B_1 = \left\{C\right\} \\
  B_2 = \left\{g,g+1,\ldots,C\right\}
\end{gather}

Множество приема запросов на предоставление услуги $i$-типа, $i = 1,2$:
\begin{gather}
  S_1 = \overline{B_1} = X \setminus B_1 = \left\{0, ... C-1\right\} \\
  S_2 = \overline{B_2} = X \setminus B_1 = \left\{0, ... C_2-1\right\}
\end{gather}

Система уравнений глобального баланса (СУГБ):

\begin{gather}
  \begin{cases}
    (\lambda_1 + \lambda_2)p_0 = \mu p_1, \\
    (\lambda_1 + \lambda_2 + n\mu)p_n = (\lambda_1 + \lambda_2)p_{n - 1} + (n + 1)\mu p_{n+1}, n = \overline{1,g-1}, \\
    (\lambda_1 + g\mu)p_g = (\lambda_1 + \lambda_2)p_{g - 1} + (g + 1)\mu p_{g+1}, n = \overline{g+1,C-1}, \\
    (\lambda_1 + n\mu)p_g = \lambda_1p_{n - 1} + (n + 1)\mu p_{n+1}, n = \overline{g+1,C-1}, \\
    C\mu p_C = \lambda_1p_{C-1}, \\
  \end{cases}
\end{gather}

Система уравнений локального баланса (СУЛБ):

\begin{gather}
  \begin{cases}
    (\lambda_1 + \lambda_2)p_{n-1} = n\mu p_n, n= \overline{1,g}, \\
    \lambda_1p_{n-1} = n\mu p_n, n=\overline{g+1,C}, \\
  \end{cases}
\end{gather}

Обозначим $\rho_1 = \frac{\lambda_1}{\mu}, \rho_2 = \frac{\lambda_2}{\mu}$.

Стационарное распределение вероятностей состояний системы:

\begin{gather}\label{eq:spd}
  p_n = 
  \begin{cases}
    p_0 \times \frac{(\rho_1 + \rho_2)^n}{n!}, n=\overline{1,g}, \\
    p_0 \times \frac{(\rho_1 + \rho_2)^g \times \rho_1^{n-g}}{n!}, n=\overline{g+1,C},
  \end{cases}
\end{gather}

где

\begin{gather}
  p_0 = \left(\sum_{n=1}^g\frac{(\rho_1+\rho_2)^n}{n!} + \sum_{n=g+1}^C\frac{(\rho_1+\rho_2)^g \times \rho_1^{n-g}}{n!}\right)^{-1}
\end{gather}

\subsection{Вероятностные характеристики модели}

Основные вероятностные характеристики (ВХ) модели:
\begin{itemize}
  \item Вероятность блокировки по времени $E_1$ запроса на
    предоставление услуги $i$-типа, $i=1,2$:
    \begin{gather}\label{eq:tbp1}
      E_1 = \sum_{n\in B_1}p_n = p_C;
    \end{gather}
  \item Вероятность блокировки по времени $E_2$ запроса на
    предоставление услуги $i$-типа, $i=1,2$:
    \begin{gather}\label{eq:tbp2}
      E_2 = \sum_{n\in B_2}p_n = p_g + p_{g+1} + \ldots + p_C = \sum_{n=g}^Cp_n;
    \end{gather}
  \item Среднее число $\overline{N}$ обслуживаемых в системе запросов:
    \begin{gather}\label{eq:avg}
      \overline{N} = \sum_{n \in X}np_n
    \end{gather}
\end{itemize}

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

Определим функцию стационарного распределения вероятностей состояний системы \eqref{eq:spd}:

\begin{code}
stationaryProbabilityDistribution
    :: Double -> Double -> Double -> Double -> Double -> Double
stationaryProbabilityDistribution rho1 rho2 c g n
    | 1 <= n && n <= g = p0 * t1 n
    | otherwise = p0 * t2 n
  where
    t1 :: Double -> Double
    t1 a = (rho1 + rho2) ** a / factorial a
    t2 :: Double -> Double
    t2 a = (rho1 + rho2) ** g * rho1 ** (a - g) / factorial a
    p0 = (sum [t1 i | i <- [0 .. g]] + sum [t2 i | i <- [g + 1 .. c]]) ** (-1)
    factorial :: Double -> Double
    factorial 0 = 1
    factorial i = i * factorial (i - 1)
\end{code}

Определим вероятность блокировки по времени для запросов на представление услуг 1-го типа \eqref{eq:tbp1}:
\begin{code}
timeBlockingProbability1 :: (Double -> Double) -> Double -> Double
timeBlockingProbability1 dist = dist
\end{code}

И для 2-го типа \eqref{eq:tbp2}:
\begin{code}
timeBlockingProbability2 :: (Double -> Double) -> Double -> Double -> Double
timeBlockingProbability2 dist g c = sum [dist i | i <- [g .. c]]
\end{code}

Посчитаем среднее число обслуживаемых запросов \eqref{eq:avg}:
\begin{code}
averageRequests :: (Double -> Double) -> Double -> Double
averageRequests dist c =
    sum [i * dist i | i <- [1 .. c]]
\end{code}

\subsection{Входные параметры}

Для расчета основных вероятностных характеристик модели были взяты следующие параметры:

\begin{tabular}{c c c}
  $C$ & = & 60 \\
  $\mu$ & = & 0.5 \\
  $\lambda_1$ & = & 55 \\
  $\lambda_2$ & = & 50 \\
  $g$ & = & 10
\end{tabular}

В исходном коде они выглядят вот так:

\noindent
\verb!  \begin{tabular}{c c c}!\\
\verb!    $C$ & = & 60!\\
\verb!    $\mu$ & = & 2!\\
\verb!    $\lambda_1$ & = & 80!\\
\verb!    $\lambda_2$ & = & 50!\\
\verb!    $g$ & = & 40!\\
\verb!  \end{tabular}!

А само представление выше, вот так:

\begin{addmargin}{1em}
\begin{verbatim}
\noindent
\verb!  \begin{tabular}{c c c}!\\
\verb!    $C$ & = & 60!\\
\verb!    $\mu$ & = & 2!\\
\verb!    $\lambda_1$ & = & 80!\\
\verb!    $\lambda_2$ & = & 50!\\
\verb!    $g$ & = & 40!\\
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
    valueRegex = "^\\s*\\$\\\\?(\\w+)\\$\\s*&\\s*=\\s*&\\s*([0-9.]+)?"
\end{code}

\subsection{Вывод данных}

\subsubsection{Графики}

Нам необходимо показать несколько графиков, поэтому составим функции для них:

\begin{code}
prepFileName :: String -> String
prepFileName = T.unpack . T.replace (T.pack " ") (T.pack "_") . T.toLower . T.pack

plotTimeBlockingProb :: (Double -> Double) -> Double -> FilePath -> String -> IO ()
plotTimeBlockingProb tbp c dir name =
    toFile def (dir ++ prepFileName name ++ ".svg") $ do
        layout_title .= name
        setColors [opaque blue]
        plot (line "" [[(x, tbp x) | x <- [0.5, 1 .. c]]])

plotAverageRequests :: (Double -> Double -> Double) -> Double -> FilePath -> String -> IO ()
plotAverageRequests dist c dir name =
    toFile def (dir ++ prepFileName name ++ ".svg") $ do
        layout_title .= name
        setColors [opaque blue, opaque red]
        let avg x = averageRequests (dist x) c
            in plot (line "" [[(x, avg x) | x <- [0 .. c]]])
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
writeTexOutput :: Double -> Double -> Double -> Double -> Double -> FilePath -> IO ()
writeTexOutput l1 l2 mu g c fname = do
    let rho1 = l1 / mu
        rho2 = l2 / mu
        dist = stationaryProbabilityDistribution rho1 rho2 c g
    writeFile fname
        $ printf "%% Automatically generated output file!\n"
        ++ printf "Распределение вероятностей:\\\\\n"
        ++ printf "\\seqsplit{%s}\\\\\n"
            (show [stationaryProbabilityDistribution rho1 rho2 c g n | n <- [0..c]])
        ++ printf "\\begin{tabular}{l l l l}\n"
        ++ printf "Вероятность блокировки по по времени & $E_1$ & = & %f \\\\\n"
            (timeBlockingProbability1 dist c)
        ++ printf "Вероятность блокировки по по времени & $E_2$ & = & %f \\\\\n"
            (timeBlockingProbability2 dist g c)
        ++ printf "Среднее число запросов & $\\overline{N}$ & = & %f\n"
            (averageRequests dist c)
        ++ printf "\\end{tabular}"
\end{code}

\subsection{\texttt{main} функция}

Тепеь напишем функцию \texttt{main}, которя является точкой входа в программу.

\begin{code}
main :: IO ()
main = do
    handle <- openFile "./app/lab2.lhs" ReadMode
    contents <- hGetContents handle 
    let values = getValues contents
        l1 = fromJust $ M.lookup "lambda_1" values
        l2 = fromJust $ M.lookup "lambda_2" values
        c = fromJust $ M.lookup "C" values
        mu = fromJust $ M.lookup "mu" values
        g = fromJust $ M.lookup "g" values
        labDir = "assets/lab2/"
        texDir = labDir ++ "tex/"
        imgDir = labDir ++ "images/"
    createDirectoryIfMissing True texDir
    writeTexOutput l1 l2 mu g c (texDir ++ "output.tex")
    createDirectoryIfMissing True imgDir
    let dist x = stationaryProbabilityDistribution (x / mu) 0 c g
        tbp x = timeBlockingProbability1 (dist x) c
        in plotTimeBlockingProb tbp c imgDir "Time Blocking Probability for lambda_1"
    let dist x = stationaryProbabilityDistribution (x / mu) 0 c g
        tbp x = timeBlockingProbability2 (dist x) g c
        in plotTimeBlockingProb tbp c imgDir "Time Blocking Probability for lambda_2"
    let dist x = stationaryProbabilityDistribution (x / mu) 0 c g
        in plotAverageRequests dist c imgDir "Average Requests for lambda_1"
    let dist x = stationaryProbabilityDistribution 0 (x / mu) c g
        in plotAverageRequests dist c imgDir "Average Requests for lambda_2"
\end{code}

\section{Результаты}

\begingroup
\let\clearpage\relax
\include{assets/lab2/tex/output.tex}
\endgroup

\includesvg[width=220pt]{time_blocking_probability_for_lambda_1.svg}
\includesvg[width=220pt]{time_blocking_probability_for_lambda_2.svg}

\includesvg[width=220pt]{average_requests_for_lambda_1.svg}
\includesvg[width=220pt]{average_requests_for_lambda_2.svg}

\end{document}

