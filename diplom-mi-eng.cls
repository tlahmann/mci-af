% Diplomarbeitsvorlage, basierend auf ubicomp-long.cls von Stefan Dietzel
% (http://www.vf.utwente.nl/~dietzels/) und der Vorlage von Guido de Melo (http://guido.demelo.de)
% Zusammengestellt von Jonas Beinder (http://www.uni-ulm.de/in/mi/mitarbeiter/jonas-beinder.html)
\ProvidesClass{diplom-mi-eng}[2011/02/10 v0.1a Diplomarbeitsvorlage Medieninformatik]
% Version mit englischen Anzeigetexten, Julia Greim, 14/08/2013

\LoadClass[
	a4paper,	% A4
	titlepage,	% Mit Titelseite
	DIV=9,		% Text-Randverhältnis 6:3
	BCOR=10mm,	% Binding Correction
	oneside		% einseitig - alternativ twoside
]{scrbook}

\RequirePackage[T1]{fontenc}

\RequirePackage[
	pdftex,
	colorlinks=true,
	linkcolor=black,
	citecolor=black,
	urlcolor=black
]{hyperref}		% Links im PDF

%\RequirePackage{ngerman}
\RequirePackage[sc]{mathpazo}		% Palatino
\RequirePackage{helvet}				% Helvetica
\RequirePackage{booktabs}			% Schönere Tables
\RequirePackage{microtype}			% Micro-Typographische Verbesserungen
\RequirePackage{graphicx}			% Grafiken
\RequirePackage{setspace}			% Zeilenabstand
\RequirePackage[utf8]{inputenc}		% Codierung - Achtung! Wenn eine andere Codierung als UTF-8 verwendet wird, muss dies hier angegeben werden!
\RequirePackage[numbers]{natbib}	% Quellenformatierung

\def\normalsize{\@setfontsize{\normalsize}{10}{12.00pt}}	% 10 pt Schriftgröße
\normalsize
\abovedisplayskip 1.5ex plus4pt minus2pt
\belowdisplayskip \abovedisplayskip
\abovedisplayshortskip 0pt plus4pt
\belowdisplayshortskip 1.5ex plus4pt minus2pt

\RequirePackage[font=sf,sf]{caption}   	% immer Sans-Serif Font für Bildunterschriften
% \RequirePackage[sf,SF]{subfigure}		
\RequirePackage{subcaption}		

\def\abstractname{Abstract}
\newenvironment{abstract}{
	\cleardoublepage
	\section{\abstractname}
}
{\vfill\cleardoublepage}

\def\@type{}
\newcommand\type[1]{\def\@type{#1}}
\def\@jahr{}
\newcommand\jahr[1]{\def\@jahr{#1}}
\def\@matnr{}
\newcommand\matnr[1]{\def\@matnr{#1}}
\def\@emailA{}
\newcommand\emailA[1]{\def\@emailA{#1}}
\def\@emailB{}
\newcommand\emailB[1]{\def\@emailB{#1}}
\def\@emailC{}
\newcommand\emailC[1]{\def\@emailC{#1}}
\def\@authorA{}
\newcommand\authorA[1]{\def\@authorA{#1}}
\def\@authorB{}
\newcommand\authorB[1]{\def\@authorB{#1}}
\def\@authorC{}
\newcommand\authorC[1]{\def\@authorC{#1}}
\def\@fakultaet{}
\newcommand\fakultaet[1]{\def\@fakultaet{#1}}
\def\@institut{}
\newcommand\institut[1]{\def\@institut{#1}}
\def\@gutachterA{}
\newcommand\gutachterA[1]{\def\@gutachterA{#1}}
\def\@gutachterB{}
\newcommand\gutachterB[1]{\def\@gutachterB{#1}}
\def\@betreuer{}
\newcommand\betreuer[1]{\def\@betreuer{#1}}

% Titelseite
\renewcommand\maketitle{%
	\thispagestyle{empty}
	{	
		\begin{addmargin*}[-4mm]{-24mm}
			\includegraphics[height=1.8cm]{images/unilogo_bild}
			\hfill
			\includegraphics[height=1.8cm]{images/unilogo_wort}
			\vspace{.5cm}
			%\hspace*{100.5mm}\parbox[t]{60mm}{
				\begin{flushright}
					\textsf\bfseries Ulm University \textbar \mdseries ~89069 Ulm \textbar
					~Germany\\\vspace{1.25cm}
					\bfseries Faculty of \@fakultaet\\ \mdseries \@institut
				\end{flushright}
			%}
			\vspace{2cm}
			\normalfont\Huge\bfseries\@title\\[.25cm]
			\normalsize
				\mdseries\@type~at Ulm University\\[1cm]
				\bfseries Presented by:\\
				\mdseries\@authorA\\
				\@emailA\\[.25cm]
				\mdseries\@authorB\\
				\@emailB\\[.25cm]
				\mdseries\@authorC\\
				\@emailC\\[.5cm]
				\bfseries Examiner:\\
				\mdseries\@gutachterA\\
				\@gutachterB\\[.5cm]
				\bfseries Advisor:\\
				\mdseries\@betreuer\\[1cm]
				\bfseries \@jahr
			
		\end{addmargin*}
	}
}

\newcommand\copyrightinfo{
	\copyright~\@jahr~\@author\\[.5em]
	This work is licensed under the Creative Commons \textbf{Attribution-NonCommercial-ShareAlike 3.0 Unported} License. 
	To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/3.0/ .
}

\newcommand\declaration{
	\ihead{Name: \@author}
	\ohead{Matriculation Number: \@matnr}
	\ofoot{\pagemark}	
	\vspace*{2cm}	
	\minisec{Declaration}
	\vspace{.5cm}
	I, \@author, matriculation number \@matnr, hereby declare that I created this work on my own. 
	Except where referenced and credited to the original author, I have not used the material of others in my work.\\[1cm]
	\hspace*{2cm} Ulm, \dotfill\\
	\hspace*{10cm} {\footnotesize \@author}
}

\RequirePackage{scrlayer-scrpage}
\pagestyle{scrheadings}
\clearscrheadfoot
\automark[section]{chapter}

\rohead{\MakeUppercase{\rightmark}}
\lehead{\MakeUppercase{\leftmark}}
\ofoot{\pagemark}

\setkomafont{pageheadfoot}{%
\normalfont\sffamily\scshape\footnotesize
}
\setkomafont{pagenumber}{%
\normalfont\sffamily\footnotesize
}

% Schusterjungen / Hurenkinder vermeiden
\widowpenalty10000
\clubpenalty10000

% Bis zu welcher Tiefe kommen Überschriften ins Inhaltsverzeichnis
\setcounter{tocdepth}{2}
