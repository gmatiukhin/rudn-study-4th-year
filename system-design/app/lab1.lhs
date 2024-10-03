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
\tikzstyle{square} = [regular polygon, regular polygon sides=4, inner sep=0cm, text centered, draw, minimum size=1.7cm]
\tikzstyle{hiddenSquare} = [regular polygon, regular polygon sides=4, inner sep=0cm, text centered, minimum size=2.5cm]
\tikzstyle{arrow} = [thick,->,>=stealth]

\usepackage{minted}
\newminted[code]{haskell}{}
\newminted[spec]{haskell}{}

\usepackage{svg}
\usepackage{graphicx}
\graphicspath{ {assets/lab1/images/} }

\author{Григорий Матюхин}
\date{\today}
\title{
	Отчет по лабораторной работе \textnumero1: \\
	\large Полнодоступная двухсервисная модель Эрланга с одинаковыми интенсивностями обслуживания.}

\begin{document}
\maketitle
\newpage
\tableofcontents
\newpage

\section{Теориетические сведения}

\subsection{Описание модели}

Исследуется сота сети связи емкостью $С$.
Пусть пользователям сети предоставляются услуги двух типов.
Запросы в виде двух пуассоновский потоков (ПП) с
интенсивностями $\lambda_1$, $\lambda_2$ поступают в соту.
Среднее время обслуживания запросов на предоставление услуг каждого типа
$\mu_1^{-1}$, $\mu_2^{-1}$ соответственно.
Исследуются основные характеристики модели для случая $\mu_1 = \mu_2 = \mu$

В классификации Башарина-Кендалла:
$\underset{\lambda_1}{M}\underset{\lambda_2}{M}|\underset{\mu_1}{M}\underset{\mu_1}{M}|C|0$.

\begin{tabular}{l l l}
  $C$ & -- & \makecell[tl]{пиковая пропускная способность соты;} \\
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
    \node (req1) at (0, -1) {\LargeУслуга 1};
    \node (req2) at (0, -2){\LargeУслуга 2};

    \node [circ] at (4, 0) (start) {};
    \node [smallCirc] at (4, -1) (sm1) {};
    \node [smallCirc] at (4, -1.5) (sm2) {};
    \node [smallCirc] at (4, -2) (sm3) {};
    \node [circ] at (4, -3) (end) {};

    \draw[decorate, decoration={brace, raise=10pt, amplitude=5pt}, line width=1.1pt, scale=1.5] (start.north) -- (end.south) node [midway, anchor=west, xshift=23pt] {\Large$C$};

    \draw [arrow] (req1) -- node[anchor=south] {\large$\lambda_1, \mu$} (3.5, -1);
    \draw [arrow] (req2) -- node[anchor=south] {\large$\lambda_2, \mu$} (3.5, -2);
  \end{tikzpicture}
\end{center}

Пространство состояний системы:
\begin{gather}
  X = \left\{0,..., C\right\}, \left|X\right| = C + 1
\end{gather}

\begin{center}
  \begin{tikzpicture}[node distance=2cm, scale=0.7, every node/.style={scale=0.7}]
    \node [square] (o) {$0$};
    \node [square, right of=o] (l) {$1$};
    \node [hiddenSquare, right of=l] (elipses1) {\ldots};
    \node [square, right of=elipses1] (n1) {$n-1$};
    \node [square, right of=n1] (n) {$n$};
    \node [hiddenSquare, right of=n] (elipses2) {\ldots};
    \node [square, right of=elipses2] (c1) {$C-1$};
    \node [square, right of=c1] (c) {$C$};

    \draw [arrow] (o) .. controls(1, 1.2) .. node[anchor=south] {$\lambda_1 + \lambda_2$} (l);
    \draw [arrow] (l) .. controls(1, -1.2) .. node[anchor=north] {$\mu$} (o);

    \draw [arrow] (l) .. controls(3, 1.2) .. node[anchor=south] {$\lambda_1 + \lambda_2$} (elipses1);
    \draw [arrow] (elipses1) .. controls(3, -1.2) .. node[anchor=north] {$2\mu$} (l);

    \draw [arrow] (elipses1) .. controls(5, 1.2) .. node[anchor=south] {$\lambda_1 + \lambda_2$} (n1);
    \draw [arrow] (n1) .. controls(5, -1.2) .. node[anchor=north] {$(n-1)\mu$} (elipses1);

    \draw [arrow] (n1) .. controls(7, 1.2) .. node[anchor=south] {$\lambda_1 + \lambda_2$} (n);
    \draw [arrow] (n) .. controls(7, -1.2) .. node[anchor=north] {$n\mu$} (n1);

    \draw [arrow] (n) .. controls(9, 1.2) .. node[anchor=south] {$\lambda_1 + \lambda_2$} (elipses2);
    \draw [arrow] (elipses2) .. controls(9, -1.2) .. node[anchor=north] {$(n+1)\mu$} (n);

    \draw [arrow] (elipses2) .. controls(11, 1.2) .. node[anchor=south] {$\lambda_1 + \lambda_2$} (c1);
    \draw [arrow] (c1) .. controls(11, -1.2) .. node[anchor=north] {$(C-1)\mu$} (elipses2);

    \draw [arrow] (c1) .. controls(13, 1.2) .. node[anchor=south] {$\lambda_1 + \lambda_2$} (c);
    \draw [arrow] (c) .. controls(13, -1.2) .. node[anchor=north] {$C\mu$} (c1);
  \end{tikzpicture}
\end{center}

Множество блокировок запросов на предоставление услуги $i$-типа, $i = 1,2$:
\begin{gather}
  B_1 = B_2 = \left\{C\right\}
\end{gather}

Множество приема запросов на предоставление услуги $i$-типа, $i = 1,2$:
\begin{gather}
  S_i = \overline{B_i} = X \setminus B_i = \left\{0, ... C-1\right\}
\end{gather}

Система уравнений глобального баланса (СУГБ):

\begin{gather}
  \begin{cases}
    (\lambda_1 + \lambda_2)p_0 = \mu p_1, \\
    (\lambda_1 + \lambda_2 + n\mu)p_n = (\lambda_1 + \lambda_2)p_{n - 1} + (n + 1)\mu p_{n+1}, n = \overline{1,C-1}, \\
    (\lambda_1 + \lambda_2)p_{C - 1} = C\mu p_C,
  \end{cases}
\end{gather}

Система уравнений локального баланса (СУЛБ):

\begin{gather}
  (\lambda_1 + \lambda_2)p_{n - 1} = n\mu p_n, n = \overline{1,C-1}
\end{gather}

Стационарное распределение вероятностей состояний системы:

\begin{gather}\label{eq:spd}
  p_n = \left(\sum_{i = 0}^{C}\frac{(\rho_1 + \rho_2)^i}{i!}\right)^{-1} \times
\frac{(\rho_1 + \rho_2)^n}{n!}, n=\overline{0,C}
\end{gather}

Доказательство:

Используя СУЛБ, найдем стационарное распределение
вероятностей состояний системы $p_n, n = \overline{1, C}$:

\begin{gather}
  p_n = p_{n-1}\frac{\lambda_1 + \lambda_2}{n\mu} = 
    p_{n-1}\frac{\rho_1 + \rho_2}{n} = ... =
    p_0\frac{(\rho_1 + \rho_2)^n}{n!}, n = \overline{1, C}
\end{gather}

Для нахождения вероятности $p_0$ воспользуемся условием нормировки $\sum_{n=0}^C p_n = 1$:

\begin{gather}
  p_o = \left(\sum_{n=0}^C\frac{(\rho_1 + \rho_2)^n}{n!}\right)^{-1}
\end{gather}

\subsection{Вероятностные характеристики модели}

Основные вероятностные характеристики (ВХ) модели:
\begin{itemize}
  \item Вероятность блокировки по времени $E_i$ запроса на
    предоставление услуги $i$-типа, $i=1,2$:
    \begin{gather}\label{eq:tbp}
      E_1 = E_2 = E = \sum_{n \in B_i}p_n = p_C    
    \end{gather}
  \item Вероятность блокировки по вызовам $B_i$ запроса на
    предоставление услуги $i$-типа, $i=1,2$:
    \begin{gather}\label{eq:rbp}
      B_i = \frac{\lambda_i}{\lambda_1 + \lambda_2}E
    \end{gather},
    где $\frac{\lambda_i}{\lambda_1 + \lambda_2}$ -- вероятность того,
    что поступит запрос на предоставление услуги $i$-типа;
  \item Вероятность блокировки по нагрузке $C_i$ запроса на
    предоставление услуги $i$-типа, $i=1,2$:
    \begin{gather}
      C_1 = C_2 = E    
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
stationaryProbabilityDistribution :: Double -> Double -> Double -> Double -> Double
stationaryProbabilityDistribution rho1 rho2 c n = p0 * t n
  where
    t :: Double -> Double
    t a = (rho1 + rho2) ** a / factorial a
      where
        factorial :: Double -> Double
        factorial 0 = 1
        factorial i = i * factorial (i - 1)
    p0 = sum [t i | i <- [0 .. c]] ** (-1)
\end{code}

Определим вероятность блокировки по времени \eqref{eq:tbp}:
\begin{code}
timeBlockingProbability :: Double -> Double -> Double -> Double
timeBlockingProbability rho1 rho2 c = stationaryProbabilityDistribution rho1 rho2 c c
\end{code}

Определим вероятность блокировки по вызовам \eqref{eq:rbp}:
\begin{code}
requestBlockingProbability :: Double -> Double -> Double -> Double -> Double
requestBlockingProbability l1 l2 lambda timeBlockingProb =
    lambda / (l1 + l2) * timeBlockingProb
\end{code}

Посчитаем среднее число обслуживаемых запросов \eqref{eq:avg}:
\begin{code}
averageRequests :: Double -> Double -> Double -> Double
averageRequests rho1 rho2 c =
    sum [i * stationaryProbabilityDistribution rho1 rho2 c i | i <- [0 .. c]]
\end{code}

\subsection{Входные параметры}

Для расчета основных вероятностных характеристик модели были взяты следующие параметры:

\begin{tabular}{c c c}
  $C$ & = & 100 \\
  $\mu$ & = & 0.6 \\
  $\lambda_1$ & = & 48 \\
  $\lambda_2$ & = & 39
\end{tabular}

В исходном коде они выглядят вот так:

\noindent
\verb!  \begin{tabular}{c c c}!\\
\verb!    $C$ & = & 100!\\
\verb!    $\mu$ & = & 0.6!\\
\verb!    $\lambda_1$ & = & 48!\\
\verb!    $\lambda_2$ & = & 39!\\
\verb!  \end{tabular}!

А само представление выше, вот так:

\begin{addmargin}{1em}
\begin{verbatim}
\noindent
\verb!  \begin{tabular}{c c c}!\\
\verb!    $C$ & = & 100!\\
\verb!    $\mu$ & = & 0.6!\\
\verb!    $\lambda_1$ & = & 48!\\
\verb!    $\lambda_2$ & = & 39!\\
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

plotTimeBlockingProb :: Double -> Double -> FilePath -> String -> IO ()
plotTimeBlockingProb mu c dir name =
    toFile def (dir ++ prepFileName name ++ ".svg") $ do
        layout_title .= name
        setColors [opaque blue]
        let tbp l = timeBlockingProbability (l / mu) 0 c
            in plot (line "" [[(x, tbp x) | x <- [0, 0.5 .. c]]])

plotRequestBlockingProbability
  ::Double -> Double ->  Double -> Double -> FilePath -> String -> IO ()
plotRequestBlockingProbability lambda other_lambda mu c dir name =
    toFile def (dir ++ prepFileName name ++ ".svg") $ do
        layout_title .= name
        setColors [opaque blue]
        let tbp l = timeBlockingProbability (l / mu) (other_lambda / mu) c
            rbp l = requestBlockingProbability l other_lambda l $ tbp l
            in plot (line "" [[(x, rbp x) | x <- [0, 0.5 .. lambda]]])

plotAverageRequests :: Double -> Double -> FilePath -> String -> IO ()
plotAverageRequests mu c dir name =
    toFile def (dir ++ prepFileName name ++ ".svg") $ do
        layout_title .= name
        setColors [opaque blue, opaque red]
        let avg l = averageRequests (l / mu) 0 c
            in plot (line "" [[(x, avg x) | x <- [0, 0.5 .. c]]])
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
writeTexOutput l1 l2 mu c tbp fname = do
    let rho1 = l1 / mu
        rho2 = l2 / mu
    writeFile fname
        $ printf "%% Automatically generated output file!\n"
        ++ printf "Распределение вероятностей:\\\\\n"
        ++ printf "\\seqsplit{%s}\\\\\n"
            (show [stationaryProbabilityDistribution rho1 rho2 c n | n <- [0..c]])
        ++ printf "\\begin{tabular}{l l l l}\n"
        ++ printf "Вероятность блокировки по по времени & $E$ & = & %f \\\\\n" tbp
        ++ printf "Вероятность блокировки по вызовам & $B_1$ & = & %f \\\\\n"
            (requestBlockingProbability l1 l2 l1 tbp)
        ++ printf "Вероятность блокировки по вызовам & $B_2$ & = & %f \\\\\n"
            (requestBlockingProbability l1 l2 l2 tbp)
        ++ printf "Среднее число запросов & $\\overline{N}$ & = & %f\n"
            (averageRequests rho1 rho2 c)
        ++ printf "\\end{tabular}"
\end{code}

\subsection{\texttt{main} функция}

Тепеь напишем функцию \texttt{main}, которя является точкой входа в программу.

\begin{code}
main :: IO ()
main = do
    handle <- openFile "./app/lab1.lhs" ReadMode
    contents <- hGetContents handle 
    let values = getValues contents
        l1 = fromJust $ M.lookup "lambda_1" values
        l2 = fromJust $ M.lookup "lambda_2" values
        c = fromJust $ M.lookup "C" values
        mu = fromJust $ M.lookup "mu" values
        rho1 = l1 / mu
        rho2 = l2 / mu
        tbp = timeBlockingProbability rho1 rho2 c
        labDir = "assets/lab1/"
        texDir = labDir ++ "tex/"
        imgDir = labDir ++ "images/"
    createDirectoryIfMissing True texDir
    writeTexOutput l1 l2 mu c tbp (texDir ++ "output.tex")
    createDirectoryIfMissing True imgDir
    plotTimeBlockingProb mu c imgDir "Time Blocking Probability"
    plotRequestBlockingProbability l1 l2 mu c imgDir
        "Request Blocking Probability for lambda_1"
    plotRequestBlockingProbability l2 l1 mu c imgDir
        "Request Blocking Probability for lambda_2"
    plotAverageRequests mu c imgDir "Average Requests"
\end{code}

\section{Результаты}

\begingroup
\let\clearpage\relax
\include{assets/lab1/tex/output.tex}
\endgroup

\includesvg[width=220pt]{time_blocking_probability.svg}
\includesvg[width=220pt]{average_requests.svg}

\includesvg[width=220pt]{request_blocking_probability_for_lambda_1.svg}
\includesvg[width=220pt]{request_blocking_probability_for_lambda_2.svg}

\end{document}
