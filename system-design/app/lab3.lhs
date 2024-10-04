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

\tikzstyle{circ} = [circle, text centered, draw, minimum size=0.7cm, inner sep=0cm]
\tikzstyle{smallCirc} = [circle, text centered, draw, fill=black, minimum size=0.2cm, inner sep=0cm]
\tikzstyle{state} = [rectangle, text centered, draw, minimum size=1cm]
\tikzstyle{arrow} = [thick,->,>=stealth]

\usepackage{minted}
\newminted[code]{haskell}{}
\newminted[spec]{haskell}{}

\usepackage{svg}
\usepackage{graphicx}
\graphicspath{ {assets/lab3/images/} }

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

Проаназируем соту сети емкостью $C$.
Пусть пользователи генерируют запросы на передачу данных двух типов.
Запросы на передачу данных представляют собой ПП с интенсивностью $\lambda_i, i=1,2$.
Средняя длина передаваемого файла $\theta_i, i=1,2$.
Минимальная емкость, необходимая для передачи данных равна $b_i, i=1,2$.

\begin{tabular}{l l l}
  $C$ & -- & \makecell[tl]{пиковая пропускная способность соты;} \\
  $\lambda_i, i=1,2$ & -- & \makecell[tl]{интенсивность поступления запросов на передачу данных\\
  первого/второго типа [запросов/ед.вр];} \\
  $\theta_i, i=1,2$ & -- & \makecell[tl]{длина передаваемого файла первого/второго типа [бит];} \\
  $\rho_i, i=1,2$ & -- & \makecell[tl]{интенсивность предложенной нагрузки, создаваемой \\
  запросами на передачу данных первого/второго типа;} \\
  $a_i, i=1,2$ & -- & \makecell[tl]{доля нагрузки, создаваемой запросами на передачу данных\\
  первого/второго типа, которая приходится на единицу\\
  пропускной способности (безразмерная величина);} \\
  $b_i, i=1,2$ & -- & \makecell[tl]{минимальное требование к ресурсам сети, необходимое для \\
  передачи данных первого/второго типа;} \\
  $X_i(t), i=1,2$ & -- &  \makecell[tl]{число обслуживаемых в системе запросов на передачу \\
  данных первого/второго типа в момент времени $t, t>=0$;} \\
  $X(t) = (X_1(t),X_2(t))$ & -- & \makecell[tl]{СП, описывающий функционирование системы в момент\\
  времени $t, t>=0$;} \\
  $X$ & -- & \makecell[tl]{пространство состояний системы;} \\
  $n_i, i=1,2$ & -- & \makecell[tl]{число передаваемых в системе блоков данных\\
  первого/второго типа;} \\
  $B_i, i=1,2$ & -- & \makecell[tl]{множество блокировок запросов на передачу данных\\
  первого/второго типа;} \\
  $S_i, i=1,2$ & -- & \makecell[tl]{множество приема запросов на передачу данных\\
  первого/второго типа.}
\end{tabular}

Схема модели:

\begin{center}
  \begin{tikzpicture}[node distance=1cm, scale=0.7, every node/.style={scale=0.7}]
  \end{tikzpicture}
\end{center}

Пространство состояний системы:
\begin{gather}
  X = \left\{(n_1, n_2): n_1 >=0,n_2>=0\right\}
\end{gather}

Рассмотрим некоторое центральное состояние $(n_1,n_2), (n_1,n_2) \in X$.
Построим диаграмму интенсивностей переходов для центрального состояния:

\begin{center}
  \begin{tikzpicture}[node distance=2cm, scale=0.7, every node/.style={scale=0.7}]
  \end{tikzpicture}
\end{center}

Пояснения:

\begin{tabular}{l l l}
  $\frac{C}{n_1+n_2}$ & -- & \makecell[tl]{скорость передачи данных первого/второго типа в\\
  состоянии $(n_1,n_2)$;}\\
  $\frac{\theta_1}{\frac{C}{n_1+n_2}}=\frac{\theta_1}{C}(n_1 + n_2)$ & --
    & \makecell[tl]{среднее время обслуживания запроса на передачу данных\\
    первого типа в состоянии $(n_1,n_2)$;}\\
  $\frac{\theta_2}{\frac{C}{n_1+n_2}}=\frac{\theta_2}{C}(n_1 + n_2)$ & --
    & \makecell[tl]{среднее время обслуживания запроса на передачу данных\\
    второго типа в состоянии $(n_1,n_2)$;}\\
  $\frac{C}{\theta_1(n_1+n_2)}$ & -- & \makecell[tl]{интенсивность обслуживания запроса на передачу данных\\
  первого типа в состоянии $(n_1,n_2)$;}\\
  $\frac{C}{\theta_2(n_1+n_2)}$ & -- & \makecell[tl]{интенсивность обслуживания запроса на передачу данных\\
  второго типа в состоянии $(n_1,n_2)$;}
\end{tabular}

Множество блокировок запросов на передачу данных:
\begin{gather}
  B_i = \left\{\varnothing\right\}, i=1,2 \\
\end{gather}

Множество приема запросов на передачу данных:
\begin{gather}
  S_i = \overline{B_i} = X \setminus B_i = \left\{0,1,2,...\right\}, i=1,2 \\
\end{gather}

Система уравнений глобального баланса (СУГБ):

\begin{gather}
  \left(\lambda_1 + \lambda_2 + \frac{C}{(n_1+n_2)\theta_i}+\frac{C}{(n_1+n_2)\theta_2}n_2\right) \times
  p(n1,n2) = \\
  = \lambda_1p(n_1 - 1, n_2) \times U(n_1) + \lambda_2p(n_1,n_2-1)\times U(n_2) +\\
  +\frac{C}{(n_1 + 1 + n_2)\theta_1}(n_1 + 1)p(n_1+1,n_2) + \\
  +\frac{C}{(n_1 + n_2 + 1)\theta_2}(n_2 + 1)p(n_1, n_2 + 1),(n_1, n_2) \in X
\end{gather}

Чтобы выписать систему уравнений частичного баланса (СУЧБ), проверим критерий
Колмогорова. Рассмотрим произвольный замкнутый контур

Рассмотрим произведение интенсивностей переходов:

\begin{itemize}
  \item по часовой стрелке $\frac{n_2}{n_1+n_2}\frac{C}{\theta_2}\frac{n_1}{n_1+n_2-1}\frac{C}{\theta_1}\lambda_1\lambda_2$;
  \item против часовой стрелке $\frac{n_2}{n_1+n_2}\frac{C}{\theta_1}\frac{n_2}{n_1+n_2-1}\frac{C}{\theta_2}\lambda_1\lambda_2$;
\end{itemize}

Произведения равны. Критерий выполнен, следовательно, СП $(X_1(t), X_2(t))$,
описывающий поведение системы является обратимым марковским процессом, СУЧБ существует.

СУЧБ:

\begin{gather}
  \begin{cases}
    p(n_1, n_2)\frac{C}{(n_1+n_2)\theta_1}n_1 = \lambda_1p(n_1-1,n_2),n_1 > 0,
    p(n_1, n_2)\frac{C}{(n_1+n_2)\theta_2}n_2 = \lambda_2p(n_1,n_2-1),n_2 > 0,
  \end{cases},
  (n_1, n_2)\in X
\end{gather}

Обозначим $\rho_i = \lambda_i\theta_i, a_i=\frac{\rho_i}{C},\rho_i <C, i=1,2$.

Стационарное распределение вероятностей состояний системы:

\begin{gather}\label{eq:spd}
  p(n_1, n_2) = \frac{a_1^{n_1}}{n_1!}\frac{a_2^{n_2}}{n_2!}(n_1+n_2)!p(0,0)
\end{gather}

где

\begin{gather}
  p(0,0) = \left(\sum_{(n_1,n_2) \in X}(n_1 + n_2)!\frac{a_1^{n_1}}{n_1!}\frac{a_2^{n_2}}{n_2!}\right)^{-1}
\end{gather}

\subsection{Вероятностные характеристики модели}

Основные вероятностные характеристики (ВХ) модели:
\begin{itemize}
  \item Вероятность блокировки по времени $E_i, i=1,2$ запроса на
    передачу данных первого/второго типа:
    \begin{gather}\label{eq:tbp}
      E_1 = E_2 = 0;
    \end{gather}
  \item Среднее число $\overline{N_i}, i=1,2$
  обслуживаемых в системе запросов на передачу данных первого/второго типа:
    \begin{gather}\label{eq:avg_req}
      \overline{N_i} = \lambda_i\frac{\theta_i}{(\theta_1\lambda_1 + \theta_2\lambda_2)}, i=1,2
    \end{gather}
  \item Среднее время $T_i, i=1,2$ обслуживания запроса на передачу данных первого/второго типа:
    \begin{gather}\label{eq:avg_time}
      T_i = \frac{\overline{N_i}}{\lambda_i}
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

Определим вероятность блокировки по времени для запросов на представление услуг 1-го типа \eqref{eq:tbp}:
\begin{code}
timeBlockingProbability :: Double
timeBlockingProbability = 0
\end{code}

Посчитаем среднее число обслуживаемых запросов \eqref{eq:avg_req}:
\begin{code}
averageRequests :: Double -> Double -> Double -> Double -> Double -> Double -> Double
averageRequests lambda1 theta1 lambda2 theta2 lambda theta =
    lambda * (theta / (theta1 * lambda1 + theta2 * lambda2))
\end{code}

И среднее время их обслуживания \eqref{eq:avg_time}:
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
    handle <- openFile "./app/lab3.lhs" ReadMode
    contents <- hGetContents handle 
    let values = getValues contents
        l1 = fromJust $ M.lookup "lambda_1" values
        l2 = fromJust $ M.lookup "lambda_2" values
        t1 = fromJust $ M.lookup "theta_1" values
        t2 = fromJust $ M.lookup "theta_2" values
        c = fromJust $ M.lookup "C" values
        n1_max = fromJust $ M.lookup "n_1^{max}" values
        n2_max = fromJust $ M.lookup "n_2^{max}" values
        labDir = "assets/lab3/"
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
