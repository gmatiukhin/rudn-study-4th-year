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
\title{Labr}
\begin{document}
\maketitle
\newpage
\tableofcontents
\newpage

\section{Пространство состояний}

\begin{gather*}
\mathcal{X} = \{(n_1, n_2): n_1 = \overline{0,5}, n_2 = \overline{0,3}, n_1 + 2 * n_2 \leq 6\}, \|\mathcal{X}\| = 16
\end{gather*}

\tikzmath{
  \maxSum = 7;
}

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
            \draw [arrow] (\x\yi) -- node[anchor=west] {$\mu_2$} (\x\y);
          \end{scope}
        \fi

        % horizontal
        \ifnum \numexpr \xi + 2 * \y \relax < \maxSum
          \begin{scope}[transform canvas={yshift=.3em}]
            \draw [arrow] (\x\y) -- node[anchor=south] {$\lambda_1$} (\xi\y);
          \end{scope}
          \begin{scope}[transform canvas={yshift=-.3em}]
            \draw [arrow] (\xi\y) -- node[anchor=north] {$\mu_1$} (\x\y);
          \end{scope}
        \fi
      }

\end{tikzpicture}

\begin{code}
main :: IO ()
main = do
  print "hello"
\end{code}

\end{document}
