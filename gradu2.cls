% Originally written by Antti-Juhani Kaijanaho on December 2002.
% You may treat this file as if it were in the public domain.
% Subsequently modified by Matthieu Weber and Antti-Juhani Kaijanaho.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     PLEASE READ THE MANUAL!                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\def\filedate{2006/11/03}
\def\fileversion{2.3}
\def\fileinfo{JY MIT Gradut}
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{gradu2}[\filedate\space\fileversion\space\fileinfo]
\typeout{gradu2 <\filedate:\fileversion> - Tietotekniikan laitoksen graduille}

%% Some packages need special handling if the output is PDF, this \if is just
%% in case we need it
\newif\ifpdf
\ifx\pdfoutput\undefined
  \pdffalse
\else
  \ifnum\pdfoutput=1
    \pdftrue
  \else
    \pdffalse
  \fi
\fi

\newif\iffi
\fitrue

\newif\ifcopyright
\copyrighttrue

\newif\ifnumbib
\numbibtrue

\newif\ifnottoc
\nottoctrue

\newif\ifoldtitle
\oldtitletrue

\newif\ifshortthesis
\shortthesisfalse
\def\gradu@baseclass{\ifshortthesis article\else report\fi}

\newif\ifaltsubsec
\altsubsecfalse

\newif\ifpalatino
\palatinofalse

\newif\ifgradu@hyperref
\gradu@hyperreffalse

\newif\iflogo
\logofalse

\newif\ifaltlinespread
\altlinespreadfalse

%% Input encoding
\def\gradu@inputenc{utf8}
\DeclareOption{ansinew}{\gdef\gradu@inputenc{ansinew}}
\DeclareOption{applemac}{\gdef\gradu@inputenc{applemac}}
\DeclareOption{ascii}{\gdef\gradu@inputenc{ascii}}
\DeclareOption{cp1250}{\gdef\gradu@inputenc{cp1250}}
\DeclareOption{cp1252}{\gdef\gradu@inputenc{cp1252}}
\DeclareOption{cp437}{\gdef\gradu@inputenc{cp437}}
\DeclareOption{cp437de}{\gdef\gradu@inputenc{cp437de}}
\DeclareOption{cp850}{\gdef\gradu@inputenc{cp850}}
\DeclareOption{cp852}{\gdef\gradu@inputenc{cp852}}
\DeclareOption{cp865}{\gdef\gradu@inputenc{cp865}}
\DeclareOption{decmulti}{\gdef\gradu@inputenc{decmulti}}
\DeclareOption{latin1}{\gdef\gradu@inputenc{latin1}}
\DeclareOption{latin2}{\gdef\gradu@inputenc{latin2}}
\DeclareOption{latin3}{\gdef\gradu@inputenc{latin3}}
\DeclareOption{latin5}{\gdef\gradu@inputenc{latin5}}
\DeclareOption{latin9}{\gdef\gradu@inputenc{latin9}}
\DeclareOption{next}{\gdef\gradu@inputenc{next}}
\DeclareOption{utf8}{\gdef\gradu@inputenc{utf8}}

\DeclareOption{copyright}{\copyrighttrue}
\DeclareOption{nocopyright}{\copyrightfalse}

\DeclareOption{numbib}{\numbibtrue}
\DeclareOption{nonumbib}{\numbibfalse}

\DeclareOption{toc}{\nottocfalse}
\DeclareOption{nottoc}{\nottoctrue}

\DeclareOption{oldtitle}{\oldtitletrue}
\DeclareOption{newtitle}{\oldtitlefalse}

\DeclareOption{shortthesis}{\shortthesistrue}

\DeclareOption{altsubsec}{\altsubsectrue}

\let\gradu@lof\relax
\let\gradu@lot\relax
\DeclareOption{lof}{\gdef\gradu@lof{\listoffigures}}
\DeclareOption{lot}{\gdef\gradu@lot{\listoftables}}


\DeclareOption{finnish}{\fitrue}
\DeclareOption{english}{\fifalse}

\DeclareOption{palatino}{\palatinotrue}

\DeclareOption{logo}{\logotrue}

\DeclareOption{altlinespread}{\altlinespreadtrue}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{\gradu@baseclass}}
\ProcessOptions
\LoadClass[a4paper,12pt]{\gradu@baseclass}

\iflogo
\ifpdf
\IfFileExists{logo_jyu.pdf}
  {\typeout{Using logo file logo_jyu.pdf}}
  {\logofalse\ClassWarning{gradu2}{Could not find logo_jyu.pdf, logo is disabled}}
\else
\IfFileExists{logo_jyu.eps}
  {\typeout{Using logo file logo_jyu.eps}}
  {\logofalse\ClassWarning{gradu2}{Could not find logo_jyu.eps, logo is disabled}}
\fi
\fi

%\RequirePackage{color}
\RequirePackage{everyshi}[1994/12/09]
\RequirePackage[\gradu@inputenc]{inputenc}
\RequirePackage[T1]{fontenc}
\ifpalatino
\RequirePackage{mathpazo}
\RequirePackage[scaled=.95]{helvet}
\RequirePackage{courier}
\ifaltlinespread
\renewcommand{\baselinestretch}{1.575} % 1.5 * 1.05
\else
\renewcommand{\baselinestretch}{1.26} % 1.2 * 1.05
\fi
\else
\ifaltlinespread
\renewcommand{\baselinestretch}{1.5}
\else
\renewcommand{\baselinestretch}{1.2}
\fi
\fi
\RequirePackage[left=35mm,right=20mm,top=35mm,bottom=35mm,nohead]{geometry}
\RequirePackage[finnish,english]{babel}
\ifnumbib
\ifnottoc
\RequirePackage[numbib,nottoc]{tocbibind}
\else %nottoc
\RequirePackage[numbib]{tocbibind}
\fi %nottoc
\else %numbib
\ifnottoc
\RequirePackage[nottoc]{tocbibind}
\else  %nottoc
\RequirePackage{tocbibind}
\fi %nottoc
\fi %numbib

\iffi
\def\gradu@msg@muistitko{Muistitko k\"aytt\"a\"a}
\def\gradu@msg@komentoa{-komentoa?}
\def\termlistname{Sanasto}
\def\appendicesname{Liitteet}
\else
\def\gradu@msg@muistitko{Did you remember to use the}
\def\gradu@msg@komentoa{ command?}
\def\termlistname{Glossary}
\def\appendicesname{Appendices}
\fi

\newcounter{pages}

\iffi
\def\selectdefaultlanguage{\selectlanguage{finnish}}
\else
\def\selectdefaultlanguage{\selectlanguage{english}}
\fi

%% This requires extensive explanation.
%%
%% Defining the command that displays the total number of pages requires to know
%% if hyperref is loaded or not, because it relies on \if-commands that do not
%% exist unless hyperref is loaded.
%%   But hyperref must be loaded by the user, not by gradu2, because hyperref
%% overwrites definitions made in many packages. The user may use such
%% packages, therefore hyperref must be loaded *after* these packages. In
%% addition, hyperref must be loaded before packages based on the float package,
%% because the latter is incompatible with hyperref.
%% gradu2 therefore expects the user to potentially load hyperref, and if it is
%% the case, it defines \TotPages@putlabel just before \begin{document}. This
%% could not be done with \AtBeginDocument, because hyperref also uses
%% \AtBeginDocument extensively, and gradu2' \AtBeginDocument{\def\@schapter...}
%% would have been executed *before* hyperref is loaded (\begin{document} hooks
%% are executed in the same order they are defined, and gradu2 would define its
%% hook before hyperref defines its ones).  Defining a hook in a hook i.e.,
%% \AtBeginDocument{\AtBeginDocument{\def\whatever}} does not work, gradu2
%% therefore needs to define its own hook, independently from the normal
%% \begin{document} hooks. The proper way of doing so would be to redefine
%% \document, but for some obscure reason, this does not work. The working
%% solution consists in redefining \begin and check if it is \begin{document}
%% (and then execute gradu2' own hook before actually executing
%% \begin{document}) or \begin{sometingelse} (then do nothing special).
%%   gradu2' own hook is one \AtBeginDocument that puts its own commands after every others
%% (defining \TotPages@putlabel, then calling \maketitle).

%%%% Begin dirty hack
\def\gradu@before@begin@document{
\AtBeginDocument{%
  \@ifpackageloaded{hyperref}{
  \gradu@hyperreftrue
  \def\toclevel@schapter{0}%
  \def\toclevel@appendix{0}%
  \def\toclevel@algorithm{0}%

   \define@Hy@TotPages@putlabel
   }{%
   \gradu@hyperreffalse
   \define@TotPages@putlabel
   }%
   \pagenumbering{roman}
   \selectdefaultlanguage
   \iflogo
   \@ifpackageloaded{graphicx}{}{%
    \ClassWarning{gradu2}{The logo option requires the graphicx package to be
    loaded, loading it now.}
    \ifpdf
    \RequirePackage[pdftex]{graphicx}
    \else
    \RequirePackage[dvips]{graphicx}
    \fi
   }
   \fi
   \maketitle
}}

\RequirePackage{ifthen}
\let\gradu@begin=\begin
\renewcommand*{\begin}[1]{%
\ifthenelse{\equal{#1}{document}}{%
\gradu@before@begin@document
}{}%
\gradu@begin{#1}%
}
%%%% End dirty hack

\newcounter{TotPages}
\EveryShipout{\stepcounter{TotPages}}

%% The definition of \TotPages@putlabel must be delayed, since hyperref is not
%% yet loaded at this point, all the \ifHy@xxx are undefined but latex
%% still complains about the corresponding \fi
\def\define@Hy@TotPages@putlabel{
\def\TotPages@putlabel{%
  \addtocounter{page}{-1}%
  \if@filesw
    \begingroup
      \ifHy@pageanchor
        \ifHy@hypertexnames
          \ifHy@plainpages
             \def\Hy@temp{\arabic{page}}%
          \else
             \let\textlatin\@firstofone
             \edef\Hy@temp{\thepage}%
          \fi
        \else
          \def\Hy@temp{\the\Hy@pagecounter}%
        \fi
      \fi
      \immediate\write\@mainaux{%
          \string\newlabel{TotPages}{{\theTotPages}{\thepage}{}{%
          \ifHy@pageanchor page.\Hy@temp\fi
        }{}}%
      }%
    \endgroup
  \fi
  \addtocounter{page}{1}%
}}
\def\define@TotPages@putlabel{
\def\TotPages@putlabel{%
  \addtocounter{page}{-1}%
  \if@filesw
    \begingroup
      \immediate\write\@mainaux{%
          \string\newlabel{TotPages}{{\theTotPages}{\thepage}}%
      }%
    \endgroup
  \fi
  \addtocounter{page}{1}%
}}%

\AtEndDocument{%
\clearpage\TotPages@putlabel
}

\def\gradu@muistitko#1{\textit{\gradu@msg@muistitko} \texttt{\textbackslash #1}\textit{\gradu@msg@komentoa}}

\def\gradu@title{\gradu@muistitko{title}}
\def\gradu@author{\gradu@muistitko{author}}
\def\gradu@date{\today}
\def\gradu@paikka{Jyv\"askyl\"a}
\def\gradu@yliopisto{Jyv\"askyl\"an yliopisto}
\def\gradu@university{University of Jyv\"askyl\"a}
\def\gradu@laitos{%
  \iffi%
  Tietotekniikan laitos%
  \else%
  Department of Mathematical Information Technology%
  \fi%
}
\def\gradu@aine{%
  \iffi%
  Tietotekniikan%
  \else%
  in Information Technology%
  \fi%
}
\def\gradu@linja{}
\def\gradu@tyyppi{pro gradu -tutkielma}
\def\gradu@type{Master's Thesis}
\def\gradu@tiivistelma{\gradu@muistitko{tiivistelma}}
\def\gradu@abstract{\gradu@muistitko{abstract}}
\def\gradu@titletrans{\gradu@muistitko{translatedtitle}}
\def\gradu@avainsanat{\gradu@muistitko{avainsanat}}
\def\gradu@keywords{\gradu@muistitko{keywords}}
%\def\gradu@yhteystiedot{\gradu@muistitko{yhteystiedot}}
\def\gradu@copyrightowner{\gradu@makeauthorsl}
\def\gradu@license{All rights reserved.}
\def\gradu@copyrightyear{\number\year}
\def\gradu@acmccs{}
\def\gradu@ysa{}

\def\gradu@authors{}
\let\gradu@authorstart\relax
\let\gradu@authorsep\relax
\let\gradu@authorend\relax

\def\gradu@authorsl{}
\let\gradu@authorlstart\relax
\let\gradu@authorlsep\relax
\let\gradu@authorlend\relax

\def\gradu@srohtua{}
\let\gradu@rohtuastart\relax
\let\gradu@rohtuasep\relax
\let\gradu@rohtuaend\relax

\renewcommand{\title}[1]{\gdef\gradu@title{#1}}
\renewcommand{\author}[1]{%
 \ClassWarning{gradu2}{\noexpand\author is deprecated!}%
 \gdef\gradu@authors{#1 \ifoldtitle\else(Use \noexpand\setauthor!)\fi}
 \gdef\gradu@srohtua{#1 \ifoldtitle\else(Use \noexpand\setauthor!)\fi}
}
%\newcommand{\setauthor}[2]{\gdef\gradu@author{#1 #2}\gdef\gradu@rohtua{#2, #1}}
\newcounter{gradu@authorcount}
\newcommand{\setauthor}[2]{
  \@cons\gradu@authors{\gradu@authorstart#1\gradu@authorsep#2\gradu@authorend}
  \@cons\gradu@authorsl{\gradu@authorlstart#1\gradu@authorlsep#2\gradu@authorlend}
  \@cons\gradu@srohtua{\gradu@rohtuastart#2\gradu@rohtuasep#1\gradu@rohtuaend}
	\addtocounter{gradu@authorcount}{1}%
}

\newcounter{gradu@author@count}
\newcounter{gradu@authorcount@}
\def\gradu@makeauthors{%
  \setcounter{gradu@author@count}{\value{gradu@authorcount}}
	\setcounter{gradu@authorcount@}{0}%
  \gdef\gradu@authorstart{
		\addtocounter{gradu@author@count}{-1}%
	  \ifthenelse{\value{gradu@authorcount@} = 0}{\begin{tabular}{c}}{}
		\addtocounter{gradu@authorcount@}{1}%
	}
	\gdef\gradu@authorsep{\space}%
	\gdef\gradu@authorend{%
	  \ifthenelse{\value{gradu@author@count} = 0}{\end{tabular}}{\\[0.2em]}%
	}%
	\gradu@authors
}

\newcounter{gradu@authorl@count}
\newcounter{gradu@authorlcount@}
\def\gradu@makeauthorsl{%
  \setcounter{gradu@authorl@count}{\value{gradu@authorcount}}%
	\setcounter{gradu@authorlcount@}{0}%
  \gdef\gradu@authorlstart{%
		\addtocounter{gradu@authorl@count}{-1}%
	  \ifthenelse{\value{gradu@authorlcount@} > 0}{%
		  \ifthenelse{\value{gradu@authorl@count} = 0}%
			  {\iffi\space ja\space \else\space and\space \fi}{,\space}%
		}{}%
		\addtocounter{gradu@authorlcount@}{1}%
	}%
	\gdef\gradu@authorlsep{\space}%
	\gdef\gradu@authorlend{\relax}%
	\gradu@authorsl
}

\newcounter{gradu@rohtua@count}
\def\gradu@makesrohtua{
  \setcounter{gradu@rohtua@count}{\value{gradu@authorcount}}%
  \gdef\gradu@rohtuastart{\relax}
	\gdef\gradu@rohtuasep{,\space}%
	\gdef\gradu@rohtuaend{%
		\addtocounter{gradu@rohtua@count}{-1}%
	  \ifthenelse{\value{gradu@rohtua@count}=0}{\relax}{;\space}
	}%
	\gradu@srohtua
}

\renewcommand{\date}[1]{%
  \ClassWarning{gradu2}{\noexpand\date is deprecated!}%
  \gdef\gradu@date{#1}%
}
\newcommand{\setdate}[3]{\gdef\day{#1}\gdef\month{#2}\year=#3}
\newcommand{\paikka}[1]{\gdef\gradu@paikka{#1}}
\newcommand{\yliopisto}[1]{\gdef\gradu@yliopisto{#1}}
\newcommand{\university}[1]{\gdef\gradu@university{#1}}
\newcommand{\laitos}[1]{\gdef\gradu@laitos{#1}}
\newcommand{\aine}[1]{\gdef\gradu@aine{#1}}
\newcommand{\linja}[1]{\gdef\gradu@linja{(#1)}}
\newcommand{\tyyppi}[1]{\gdef\gradu@tyyppi{#1}}
\newcommand{\type}[1]{\gdef\gradu@type{#1}}
\newcommand{\keywords}[1]{\gdef\gradu@keywords{\begin{foreignlanguage}{english}#1\end{foreignlanguage}}}
\newcommand{\avainsanat}[1]{\gdef\gradu@avainsanat{\begin{foreignlanguage}{finnish}#1\end{foreignlanguage}}}
\newcommand{\contactinformation}[1]{\yhteystiedot{#1}}
%\newcommand{\yhteystiedot}[1]{\gdef\gradu@yhteystiedot{#1}}

\newcounter{gradu@yhteystiedotcount}
\def\gradu@yhteystiedot{}
\let\gradu@yhteystiedotstart\relax
\let\gradu@yhteystiedotend\relax

\newcommand{\yhteystiedot}[1]{%
  \@cons\gradu@yhteystiedot{\gradu@yhteystiedotstart#1\gradu@yhteystiedotend}
	\addtocounter{gradu@yhteystiedotcount}{1}
}
\newcounter{gradu@yhteystiedot@count}
\def\gradu@makeyhteystiedot{%
  \setcounter{gradu@yhteystiedot@count}{\value{gradu@yhteystiedotcount}}%
	\gdef\gradu@yhteystiedotend{%
		\addtocounter{gradu@yhteystiedot@count}{-1}%
	  \ifthenelse{\value{gradu@yhteystiedot@count} = 0}{}{,\space}%
	}%
	\gradu@yhteystiedot
}

\long\def\tiivistelma#1{\gdef\gradu@tiivistelma{\begin{foreignlanguage}{finnish}#1\end{foreignlanguage}}}
\long\def\abstract#1{\gdef\gradu@abstract{\begin{foreignlanguage}{english}#1\end{foreignlanguage}}}
\newcommand{\translatedtitle}[1]{\gdef\gradu@titletrans{#1}}
\newcommand{\copyrightowner}[1]{\gdef\gradu@copyrightowner{#1}}
\newcommand{\license}[1]{\gdef\gradu@license{#1}}
\newcommand{\copyrightyear}[1]{\gdef\gradu@copyrightyear{#1}}
\newcommand{\acmccs}[1]{\gdef\gradu@acmccs{\textbf{ACM Categories and Subject Descriptors:} \begin{foreignlanguage}{english}#1\end{foreignlanguage}\par}}
\newcommand{\ysa}[1]{\gdef\gradu@ysa{\textbf{Asiasanat (YSA):} \begin{foreignlanguage}{finnish}#1\end{foreignlanguage}\par}}

\newcommand{\fulltitle}{\gradu@title: \gradu@tyo}


\newcommand{\gradu@toolate}{%
  \ClassError{gradu2}{%
    \iffi%
    Bibliografiatietoja ei voi antaa enaa dokumentin alun jalkeen.%
    \else%
    Bibliographical data cannot be given after the beginning of document.%
    \fi%
  }{%
    \iffi%
    Komennot kuten \textbackslash title voi antaa vain ennen
    \textbackslash begin\{document\}-komentoa.
    \else%
    You can give commands like \textbackslash title only before
    the \textbackslash begin\{document\} command.
    \fi%
  }%
}

%\@ifundefined{\csname gradu@numpages \endcsname}
%  \def\gradu@numpages{\textbf{?}}
%\fi

\newcommand{\mainmatter}{
  \tableofcontents
  \gradu@lof
  \gradu@lot
  \setcounter{pages}{\value{page}}
  \clearpage
  \pagenumbering{arabic}
\ifshortthesis\else
  \setcounter{chapter}{0}
\fi
}
\renewcommand{\maketitle}{%
\iffi
    \def\gradu@tyo{\gradu@aine{} %
    \gradu@linja{}\if\gradu@linja\else\ \fi\gradu@tyyppi}%
\else
    \def\gradu@tyo{\gradu@type{} \gradu@aine{} \gradu@linja}%
\fi
\ifoldtitle%
  \begin{titlepage}%
    \vspace*{7truecm plus 1truecm minus 3truecm}%
    \centerline{\textbf{\gradu@makeauthors}}%
    \vspace{2truecm}%
    \centerline{\Large\textbf{\parbox[top][\height][c]{\textwidth}{\centering \gradu@title}}}%
    \vspace{4truecm}%
    \par{%
    \parindent9truecm\parskip0pt%
    \parbox{6.5cm}{\raggedright 
\iffi%
      \gradu@aine\ \gradu@linja\par%
      \gradu@tyyppi%
\else%
      \gradu@type\par%
      \gradu@aine\ \gradu@linja%
\fi%
      \par%
      \gradu@date%
      }}%
    \vfill%
\iflogo
    %\center{\hspace{3pt}\includegraphics[height=3.25truecm]{logo_jyu}}%
    % for use with \large and \normalsize instead of \normalsize and \small
    % \gradu@yliopisto and \gradu@laitos
    \center{\hspace{3pt}\includegraphics[height=2.05truecm]{logo_jyu}}%
\par\vspace{-.6cm}
\begingroup
\ifpalatino\else\fontfamily{ppl}\selectfont\fi
\iffi
    \center{\normalsize\MakeUppercase{\gradu@yliopisto}}%
\else
    \center{\normalsize\MakeUppercase{\gradu@university}}%
\fi
    \par\vspace{-.5cm}
    \center{\small\MakeUppercase{\gradu@laitos}}%
\endgroup
\else
    \vspace{1truecm plus 1truecm minus .5truecm}%
\iffi
    \center{\large\textbf{\gradu@yliopisto}}%
\else
    \center{\large\textbf{\gradu@university}}%
\fi
    \center{\textbf{\gradu@laitos}}%
\fi
    \center{\textbf{\gradu@paikka}}%
  \end{titlepage}%
  \newpage%
  \begin{titlepage}%
\iffi%
    \noindent\textbf{Tekij\"a\ifthenelse{\value{gradu@authorcount}>1}{t}{}:} \gradu@makeauthorsl\par%
    \noindent\textbf{Yhteystiedot:} \gradu@makeyhteystiedot\par%
    \noindent\textbf{Ty\"on nimi:} {\let\\\relax \gradu@title\par}%
    \noindent\textbf{Title in English:} \gradu@titletrans\par%
    \noindent\textbf{Ty\"o:} \gradu@tyo\par%
    \noindent\textbf{Sivum\"a\"ar\"a:} \ref{TotPages}\par%
    \noindent\textbf{Tiivistelm\"a:} \gradu@tiivistelma\par%
    \noindent\textbf{English abstract:} \gradu@abstract\par%
    \noindent\gradu@ysa%
    \noindent\textbf{Avainsanat:} \gradu@avainsanat\par%
    \noindent\gradu@acmccs%
    \noindent\textbf{Keywords:} \gradu@keywords\par%
\else%
    \noindent\textbf{Author\ifthenelse{\value{gradu@authorcount}>1}{s}{}:} \gradu@makeauthorsl\par%
    \noindent\textbf{Contact information:} \gradu@makeyhteystiedot\par%
    \noindent\textbf{Title:} {\let\\\relax \gradu@title\par}%
    \noindent\textbf{Ty\"on nimi:} \gradu@titletrans\par%
    \noindent\textbf{Project:} \gradu@tyo\par%
    \noindent\textbf{Page count:} \ref{TotPages}\par%
    \noindent\textbf{Abstract:} \gradu@abstract\par%
    \noindent\textbf{Suomenkielinen tiivistelm\"a:} \gradu@tiivistelma\par%
    \noindent\gradu@acmccs%
    \noindent\textbf{Keywords:} \gradu@keywords\par%
    \noindent\gradu@ysa%
    \noindent\textbf{Avainsanat:} \gradu@avainsanat\par%
\fi%
\ifcopyright%
    \bigskip%
    \noindent Copyright \copyright\ \gradu@copyrightyear\ \gradu@copyrightowner\par%
    \medskip%
    \noindent\gradu@license%
\fi%
  \end{titlepage}%
\else%
  % ------------- NEW TITLEPAGE FORMAT -------------
  \begin{titlepage}%
    \vspace*{5truecm plus 2truecm minus 3truecm}%
    \centerline{\textbf{\gradu@makeauthorsl}}%
    \vspace{2truecm}%
    \centerline{\Large\textbf{\parbox[top][\height][c]{\textwidth}{\centering \gradu@title}}}%
    \vspace{4truecm}%
    \par{%
\iffi%
     \parindent9truecm\parskip0pt%
      \gradu@aine\ \gradu@linja\par%
      \gradu@tyyppi%
\else%
     \parindent6truecm\parskip0pt%
      \gradu@type\par%
      \gradu@aine\ \gradu@linja%
\fi%
      \par%
      \gradu@date%
      }%
    \vfill%
    \vspace{1truecm plus 1truecm minus .5truecm}%
\iffi
    \center{\large\textbf{\gradu@yliopisto}}%
\else
    \center{\large\textbf{\gradu@university}}%
\fi
    \center{\textbf{\gradu@laitos}}%
    \center{\textbf{\gradu@paikka}}%
  \end{titlepage}%
\ifcopyright%
  \begin{titlepage}%
    \mbox{}\vfill%
    \centering%
    \noindent Copyright \copyright\ \gradu@copyrightyear\ \gradu@copyrightowner\par%
    \medskip%
    \noindent\gradu@license%
    \par\vskip 3 cm plus 1 cm minus 1 cm\par%
    \noindent \gradu@yliopisto\\%
    \gradu@paikka~\number\year
    %\par\vfill%
  \end{titlepage}%
\fi%
  \iffi%
    \def\gradu@finnishtitle{\gradu@title}%
    \def\gradu@englishtitle{\gradu@titletrans}%
  \else%
    \def\gradu@finnishtitle{\gradu@titletrans}%
    \def\gradu@englishtitle{\gradu@title}%
  \fi%
  \def\gradu@finnishabstract{%
    \begin{titlepage}%
      %\selectlanguage{finnish}%
      \section*{Tiivistelm\"a}%
      \gradu@makesrohtua\\%
      {\def\\{ }\gradu@finnishtitle{} / \fitrue\gradu@makeauthorsl\fifalse}\\%
      \gradu@paikka: \gradu@yliopisto, \number\year \\%
      \ref{TotPages}~s.\\%
      \gradu@tyyppi\par%
      \bigskip%
      \noindent\gradu@tiivistelma\par%
      \bigskip%
      \noindent\gradu@ysa%
      \noindent\textbf{Avainsanat: } \gradu@avainsanat%
      %\selectdefaultlanguage%
    \end{titlepage}%
  }%
  \def\gradu@englishabstract{%
    \begin{titlepage}%
      \section*{Abstract}%
      \gradu@makesrohtua\\%
      {\def\\{ }\gradu@englishtitle{} / \fifalse\gradu@makeauthorsl\fitrue}\\%
      \gradu@paikka: \gradu@university, \number\year \\%
      \ref{TotPages}~p.\\%
      \gradu@type\par%
      \bigskip%
      \noindent\gradu@abstract\par%
      \bigskip%
      \noindent\gradu@acmccs%
      \noindent\textbf{Keywords: } \gradu@keywords%
    \end{titlepage}%
  }%
\iffi
  \newpage%
  \gradu@finnishabstract%
  \newpage
  \gradu@englishabstract%
\else
  \newpage%
  \gradu@englishabstract%
  \newpage
  \gradu@finnishabstract%
\fi
\fi%
  \global\let\title\gradu@toolate%
  \global\let\author\gradu@toolate%
  \global\let\date\gradu@toolate%  
  \global\let\linja\gradu@toolate%  
  \global\let\tyyppi\gradu@toolate%  
  \global\let\keywords\gradu@toolate%  
  \global\let\linja\gradu@toolate%  
  \global\let\tyyppi\gradu@toolate%  
  \global\let\keywords\gradu@toolate%  
  \global\let\avainsanat\gradu@toolate%  
  \global\let\contactinformation\gradu@toolate%  
  \global\let\yhteystiedot\gradu@toolate%  
  \global\let\tiivistelma\gradu@toolate%  
  \global\let\abstract\gradu@toolate%  
  \global\let\translatedtitle\gradu@toolate%  
  \global\let\copyrightowner\gradu@toolate%  
  \global\let\license\gradu@toolate%  
  \global\let\copyrightyear\gradu@toolate%  
}

\newcommand{\preface}{\chapter*{\prefacename}
\addcontentsline{toc}{chapter}{\prefacename}
}

\newcommand{\termlist}{\chapter*{\termlistname}
\addcontentsline{toc}{chapter}{\termlistname}
}

\renewcommand\appendix{\par
  \addtocontents{toc}{\protect\contentsline{chapter}{\appendicesname}{}{}}
  \setcounter{chapter}{0}%
  \setcounter{section}{0}%
  \gdef\@chapapp{\appendixname}%
  \gdef\thechapter{\@Alph\c@chapter}}

\def\@makechapterhead#1{%
  \vspace*{2em}%
  {\parindent \z@ \raggedright \normalfont
    \Large \bfseries 
    \ifnum \c@secnumdepth >\m@ne
      \thechapter\hskip2ex
    \fi
    #1\par\nobreak
    \vskip 1em
  }}
\def\@makeschapterhead#1{%
  \vspace*{2em}%
  {\parindent \z@ \raggedright
    \normalfont
    \interlinepenalty\@M
    \Large \bfseries  #1\par\nobreak
    \vskip 1em
  }}

\renewcommand\section{\@startsection%
  {section}%
  {1}%
  {\z@}%
  {-2em}%
  {1em}%
  {\normalfont\bfseries\large}%
}

\renewcommand\subsection{\@startsection%
  {subsection}%
  {2}%
  {\z@}%
  {-\baselineskip}%
  {\ifaltsubsec
    -2ex
   \else
     2ex
   \fi}% aligns the first line of the following paragraph with the title:
         % bug or feature?
  %{2ex}%
  {\normalfont\bfseries\normalsize}%
}

\renewcommand\subsubsection{\ClassError{gradu2}{Don't use sectioning below subsection}{}}
\renewcommand\paragraph{\ClassError{gradu2}{Don't use sectioning below subsection}{}}
\renewcommand\subparagraph{\ClassError{gradu2}{Don't use sectioning below subsection}{}}

\setcounter{secnumdepth}{2}

\newenvironment{chapterquote}[1]{%
  \begin{quote}\em
  \def\gradu@quoteattr{#1}
}{%
  \\\mbox{}\hfill---~\gradu@quoteattr
  \end{quote}
  \@endparenv
}

\let\gradu@thebibliography\thebibliography
\def\thebibliography#1{%
  \iffi%
  \def\refname{L\"ahteet}%
  \def\bibname{L\"ahteet}%
  \else%
  \def\refname{References}%
  \def\bibname{References}%
  \fi%
  \gradu@thebibliography{#1}%
}
