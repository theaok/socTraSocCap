version 12

set cformat %9.2f
set pformat %3.2f
set linesize 78

//TODO have installations here
//ssc install dataout
//ssc install texsave

//a very useful piece for reversing the codingk
//net install revrs, from(http://fmwww.bc.edu/repec/bocode/r/) force


**** read in data into macros


insheet using ~/data/data_sources.csv, names clear
keep if fil !=""
count
forval i=1/`r(N)' {
		local k = bib[`i']
		local p = fil[`i']
	global `k'    use  `p'/`k',clear 
}


**** programs

set trace off


*** moving, displaying

cap program drop gh //graph hmdc :)
program define gh
gr export /tmp/g1.eps,replace
! convert /tmp/g1.eps /tmp/g1.png
! scp /tmp/g1.png akozaryn@rce.hmdc.harvard.edu:~/public_html/tmp/
end

cap program drop dy //DisplaY :)
program define dy
gr export /tmp/g1.eps,replace
! epstopdf /tmp/g1.eps 
! acroread /tmp/g1.pdf
end



*** TODO: classify

/*******/ //Execute Meta Data--it writes to a file interesting
/* emd */ //quantities that are then grabbed by latex
/*******/

cap program drop emd
program define emd
syntax, s(string asis)t(string asis)ra(string asis) 
file open tex using /tmp/tex, write text `ra'
file write tex `"%<*`t'>`:di `s' '%</`t'> "' _n
file close tex
end //a written out example follows
/* est restore wvsA2 */
/* count if e(sample)==1 */
/* emd, s(%9.0f `r(N)' )t(wvsN)ra(replace) */
/* file open tex using /tmp/tex, write text replace */
/* file write tex `"%<*wvsN>`:di  %9.0f `r(N)''%</wvsN> "' _n */
/* file close tex */



/*****************/
/* aok_spineplot */
/*****************/
capture program drop aok_spineplot
*//top x axis seem not to make any sense --this is not the fraction...so i dropped the label
*//
program define aok_spineplot
 syntax varlist  [if] [in],gr_name(string) note(string) yl(string asis) d(string) 

  preserve
  if "`if'"~="" | "`in'"~="" {
        keep `if'  `in'
  }


    tokenize "`varlist'"
    capture drop freq
    bysort `varlist': gen freq = _N
spineplot  `1' `2' `if', text(freq, mlabsize(*.8)mlabcol(black))bar1(color(gs11))bar2(color(gs6))bar3(color(gs11)) yla(`yl', axis(1))yti(`:var lab `1'', axis(1))legend(off)scheme(s2mono)note(`note')yscale(reverse) xti(" " , axis(1))xla(none , axis(1))
    graph export `d'out/base/`gr_name'.eps, as(eps)  replace
    sh(epstopdf `d'out/base/`gr_name'.eps)
    sh(xpdf `d'out/base/`gr_name'.pdf &)

  restore
end
 *//to change axes  ymti(0(.2)1, axis(1))xti(fraction by partyid) yti(fraction by pol views, axis(1))legend(off)


/*****************/
/* aok_mine      */
/*****************/
capture program drop aok_mine
program define aok_mine
 syntax  [if] [in],model(string) dv(string asis) x(string asis) by(string asis) tex_lab(string asis) d(string asis)

  preserve
  if "`if'"~="" | "`in'"~="" {
        keep `if'  `in'
  }

local tex_file `tex_lab'
local est_sto `tex_lab'
local tex_cap "`tex_lab';   \today"
local tex_note "Dependent variable: `:var lab `dv'';  Model: `model'"

codebook `by'
est clear
*//gen strings for labeling nicely :)
macro drop _factor*
levelsof `by', local(levels)
foreach l of local levels {
  local factor`l'  : label `by' `l'
  local factor `factor' `factor`l''
}
di "`factor'"

levelsof `by', local(levels)
foreach l of local levels{
  local factor`l'  : label `by' `l'
  xi: `model' `dv' `x' if `by'==`l', robust
  est title: `factor`l''
  est sto `est_sto'`l'
}

estout `est_sto'*  using `d'out/base/`tex_file'.tex , replace style(tex)prehead(\documentclass[]{article} \usepackage[pdftex]{graphicx}\usepackage{anysize}\renewcommand\thetable{ID}\usepackage[margin=0pt,font=small,labelfont=bf,listformat=empty]{caption} \marginsize{2.5cm}{2.5cm}{.5cm}{2.5cm} \renewcommand{\topfraction}{0.99} \renewcommand{\bottomfraction}{0.99} \renewcommand{\textfraction}{0.01} \begin{document} \begin{scriptsize}\begin{table}[htbp]\centering \caption{`tex_cap'}\label{`tex_lab'} \begin{tabular}{lllllllllllll} \hline {Variable})posthead(\hline) prefoot(\hline) postfoot(\hline *** p$<$0.01, ** p$<$0.05, * p$<$0.1 \end{tabular} \end{table} \end{scriptsize}\textbf{NOTES:} `tex_note' \end{document}) collabels(, none) eform cells(b(star fmt(%9.3f))) stats(N, labels( "N" ) fmt(%9.0f %9.0f))varlabels(_cons Constant) starl(* .1 ** .05 *** .01) label drop( _cons*)
sh(latex `d'out/base/`tex_file'.tex)
sh(xpdf `d'out/base/`tex_file'.pdf &)
  restore
end

*//------------------------- END programs -------------------------------------------


/*****************/
/* aok_ts */
/*****************/
capture program drop aok_ts
program define aok_ts
 syntax  [if] [in], gr_name(string) note(string asis) d(string asis) dv(string) by(string) filter(string asis) over(string)ylab(string asis) 
  preserve
  if "`if'"~="" | "`in'"~="" {
        keep `if'  `in'
  }
*-----------------------------------------
table   `over' `by', c(mean `dv' sd `dv' n `dv' ) format(%9.2f)center
capture macro drop _p _u
capture drop lab*

*//drop if <25 of dv per `by'  over
bys  `over' `by' : egen _count=count(`dv')
drop if _count<25
  
codebook `by', tab(100)
sum `by'
local first_tab `r(min)'
local last_tab `r(max)' 
local dv_lab : var lab `dv'

forval a=`first_tab'/`last_tab'{
local p`a' (line `dv'`a'_ma `over' , sort)
local label_`a' : label `by' `a'
gen lab_`dv'`a'="`label_`a''"
local p `p' `p`a''
}

keep if `by'<.
gen id=_n
reshape wide `dv', i(id) j(`by')
collapse   (first) lab_*  (mean)  `dv'* , by(`over')
tsset `over'
forval a=`first_tab'/`last_tab'{
tssmooth ma `dv'`a'_ma =`dv'`a', `filter'
}

forval a=`first_tab'/`last_tab'{
sum `over' if `dv'`a'_ma!=.
local last_yr `r(max)' 
local u`a' (scatter `dv'`a'_ma `over' if `over'==`last_yr' , sort ml(lab_`dv'`a') mlabsize(large)  msize(zero) mlabpos(3))
local u `u' `u`a''
}

twoway `p'  `u', scheme(s2mono) ti(`ti') legend(off) ylabel(`ylab', labsize(large)) xlabel(, labsize(large))ytitle(`dv_lab',size(large))xtitle(,size(large))note(`note',size(large))saving(`gr_name',replace)
graph export `d'out/tmp/`gr_name'.eps, as(eps)  replace mag(54)
sh(epstopdf `d'out/tmp/`gr_name'.eps)
sh(acroread `d'out/tmp/`gr_name'.pdf &)

di "!!!!!!!!!! adam check if the values essp at BOTH extremes correspond to the above table, essp with long smoothing like 10 yrs"
restore
end


/*****************/
/* aok_ts1 */
/*****************/

//  this thing just extends aok_ts to allow other types of filters...

capture program drop aok_ts1
program define aok_ts1
 syntax  [if] [in], gr_name(string) note(string asis) d(string asis) dv(string) by(string) filter(string asis)  filter_type(string asis) over(string)ylab(string asis) drop25(string asis)
  preserve
  if "`if'"~="" | "`in'"~="" {
        keep `if'  `in'
  }
*-----------------------------------------
table   `over' `by', c(mean `dv' sd `dv' n `dv' ) format(%9.2f)center
capture macro drop _p _u
capture drop lab*

if (`drop25'==1) { 
*//drop if <25 of dv per `by'  over
bys  `over' `by' : egen _count=count(`dv')
drop if _count<25
}


codebook `by', tab(100)
sum `by'
local first_tab `r(min)'
local last_tab `r(max)' 
local dv_lab : var lab `dv'

forval a=`first_tab'/`last_tab'{
local p`a' (line `dv'`a'_ma `over' , sort)
local label_`a' : label `by' `a'
gen lab_`dv'`a'="`label_`a''"
local p `p' `p`a''
}

keep if `by'<.
gen id=_n
reshape wide `dv', i(id) j(`by')
collapse   (first) lab_*  (mean)  `dv'* , by(`over')
tsset `over'
forval a=`first_tab'/`last_tab'{
tssmooth `filter_type' `dv'`a'_ma =`dv'`a', `filter'
}

forval a=`first_tab'/`last_tab'{
sum `over' if `dv'`a'_ma!=.
local last_yr `r(max)' 
local u`a' (scatter `dv'`a'_ma `over' if `over'==`last_yr' , sort ml(lab_`dv'`a') mlabsize(large)  msize(zero) mlabpos(3))
local u `u' `u`a''
}

twoway `p'  `u', scheme(s2mono) ti(`ti') legend(off) ylabel(`ylab', labsize(large)) xlabel(, labsize(large))ytitle(`dv_lab',size(large))xtitle(,size(large))note(`note',size(large))saving(`gr_name',replace)
graph export `d'`gr_name'.eps, as(eps)  replace mag(54)
sh(epstopdf `d'`gr_name'.eps)
sh(xpdf `d'`gr_name'.pdf &)
  restore
di "!!!!!!!!!! adam check if the values essp at BOTH extremes correspond to the above table, essp with long smoothing like 10 yrs"

end


  
/* /\************************************\/ */
/* /\* aok_tabout *\/ */
/* /\************************************\/ */


/* loc v1 lifests */
/* loc v2 pcgdp1996 */
/* loc v3 inc_ppp1996 */

/* loc c mean "`v1' sd `v1'"     "`v2' sd `v2'"    "`v3' sd `v3'" */
/* loc h "`:var lab `v1''&" "`:var lab `v1''"         "`:var lab `v2''&" "`:var lab `v2''"      "`:var lab `v3''&" "`:var lab `v3''" */

/* tabout ctry_id using `d'out/base/ss.tex, c("`c'") sum replace    h3("`h'"  "\\") */


  

/* tab qlivecom1 livecom1 */
/* la def qlivecom1 1"1-2" 2"3-7" 3"8-17" 4"18-30" 5">30"  */
/* la val qlivecom1 qlivecom1 */

/* loc c=1    */
/* foreach var of varlist clsenei clsetown movenei movetown qlivecom1{ */
/* tabout `var' siz using `d'out/base/tex`c'.tex, c(col) clab(_)style(tex)  replace  topf(~/desk/papers/root/tex/topf.tex) botf(~/desk/papers/root/tex/botf.tex) */
/* loc c=1+`c' */
/* } */
/* sh(sed  -i 's|>|$>$|g' ~/desk/papers/gss_town_AND_politics/out/base/tex*.tex) */
/* sh(latex `d'out/base/tex.tex) */
/* sh(xpdf `d'out/base/tex.pdf &) */

/* sh(cp `d'out/base/tex.pdf `d'cities_to_give/easterlin_preferences.pdf) */

/* foreach var of varlist clsenei clsetown movenei movetown qlivecom1{ */
/* tab year  `var' */
/* } */



/* sh(echo "   \documentclass[]{article} \usepackage[pdftex]{graphicx}\usepackage {tabularx} \usepackage{anysize}\renewcommand\thetable{ID}\usepackage[margin=0pt,font=small,labelfont=bf,listformat=empty]{caption} \marginsize{2.5cm}{2.5cm}{.5cm}{2.5cm} \renewcommand{\topfraction}{0.99} \renewcommand{\bottomfraction}{0.99} \renewcommand{\textfraction}{0.01} \begin{document}  \input{`d'out/base/tex1}\\ \input{`d'out/base/tex2}\\ \input{`d'out/base/tex3}\\ \input{`d'out/base/tex4}\\ \input{`d'out/base/tex5}\\ \textbf{NOTES:}  \end{document}))" >> `d'out/base/tex.tex) */
   
/*********/
/* aok_c */
/*********/

capture program drop aok_c
program define aok_c
syntax  ,f(string asis) d(string asis) t(string asis) n(string)
cd `d'
sh(gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=res2.pdf `f')
sh(cp `d'res2.pdf `t'`n')
sh(pdfxv `t'`n' &)
end


/*****************/
/* aok_mine      */
/*****************/
capture program drop aok_mine
program define aok_mine
 syntax  [if] [in],model(string) dv(string asis) x(string asis) by(string asis) tex_lab(string asis) d(string asis)

  preserve
  if "`if'"~="" | "`in'"~="" {
        keep `if'  `in'
  }

local tex_file `tex_lab'
local est_sto `tex_lab'
local tex_cap "`tex_lab';   \today"
local tex_note "Dependent variable: `:var lab `dv'';  Model: `model'"

codebook `by'
est clear
*//gen strings for labeling nicely :)
macro drop _factor*
levelsof `by', local(levels)
foreach l of local levels {
  local factor`l'  : label `by' `l'
  local factor `factor' `factor`l''
}
di "`factor'"

levelsof `by', local(levels)
foreach l of local levels{
  local factor`l'  : label `by' `l'
  xi: `model' `dv' `x' if `by'==`l', robust
  est title: `factor`l''
  est sto `est_sto'`l'
}

estout `est_sto'*  using `d'out/base/`tex_file'.tex , replace style(tex)prehead(\documentclass[]{article} \usepackage[pdftex]{graphicx}\usepackage{anysize}\renewcommand\thetable{ID}\usepackage[margin=0pt,font=small,labelfont=bf,listformat=empty]{caption} \marginsize{2.5cm}{2.5cm}{.5cm}{2.5cm} \renewcommand{\topfraction}{0.99} \renewcommand{\bottomfraction}{0.99} \renewcommand{\textfraction}{0.01} \begin{document} \begin{scriptsize}\begin{table}[htbp]\centering \caption{`tex_cap'}\label{`tex_lab'} \begin{tabular}{lllllllllllll} \hline {Variable})posthead(\hline) prefoot(\hline) postfoot(\hline *** p$<$0.01, ** p$<$0.05, * p$<$0.1 \end{tabular} \end{table} \end{scriptsize}\textbf{NOTES:} `tex_note' \end{document}) collabels(, none) eform cells(b(star fmt(%9.3f))) stats(N, labels( "N" ) fmt(%9.0f %9.0f))varlabels(_cons Constant) starl(* .1 ** .05 *** .01) label drop( _cons*)
sh(latex `d'out/base/`tex_file'.tex)
*sh(xpdf `d'out/base/`tex_file'.pdf &)
  restore
end

/*****************/
/* aok_hist      */
/*****************/
*//drop the shading hor lines in the background have numbers at bins cutoffc for continous var, drop
*//axes divide freq by 1k and put that in note drop th eoutline
capture program drop aok_hist
program define aok_hist
syntax  ,x(string) d(string asis)  

sh(rm `d'hist.tex)
sh(echo "\documentclass[]{article} \usepackage[pdftex]{graphicx}\usepackage {tabularx} \usepackage{anysize}\usepackage{subfig} \begin{document} \begin{figure} \label{hist} \centering "   >> `d'hist.tex )

cd `d'
macro drop _gr
preserve
local i=1
foreach v of local x{
  qui tab `v'
  if `r(r)'>10 {
*/*   xtile q`v' = `v'<., nq(4) */
*/*   la var q`v' "`v' quartiles"  */
*/*   drop `v' */
*/*   ren q`v' `v' */
  local bin "bin(5)"
local note "[categories classified into 5 bins]"
}
else{
  local bin "discrete"
  qui sum `v'
  local ylab_val "`r(min)'(1)`r(max)'"
}

qui histogram `v',`bin'  frequency horizontal  ylabel(`ylab_val', labsize(large) angle(horizontal) valuelabel) xlabel(#4, labsize(large)format(%9.0fc))scheme(s2mono)legend(off) ytitle("")xtitle(, size(large))saving(g`i',replace)title("`:var lab `v''",span size(large))note(" `note' ",size(medium)) aspectratio(1,placement(right))plotregion(margin(l=0 r=0 t=3b=0))graphregion(margin(l=0 r=10 t=0 b=0))
qui gr export g`i'.eps, as(eps)  replace
qui sh(epstopdf g`i'.eps)
*sh(xpdf g`i'.pdf &)
macro drop _bin
macro drop _ylab*
macro drop _note
di "hi adam, it's me stata, running variable `i'"
if `i'==3 |`i'==6 |`i'==9 |`i'==12 |`i'==15|`i'==18{
  sh(echo "\subfloat{\includegraphics[width=1.7in]{`d'g`i'.pdf}}\\\\ \vspace{-.15in}">> `d'hist.tex )
   }
   else {
  sh(echo "\subfloat{\includegraphics[width=1.7in]{`d'g`i'.pdf}}" >> `d'hist.tex )
   }
local i=`i'+1
}

qui sh(echo "  \vspace{.15in} \caption{histograms} \end{figure} \end{document}" >> `d'hist.tex )
qui sh(sed  -i 's|~|/home/aok|g'  `d'hist.tex )
qui sh(pdflatex  `d'hist.tex )
qui sh(xpdf  `d'hist.pdf &) 
restore
end 
*//combibing with gr merge sucks!   
   
*//imargin(l=0 r=0 t=0 b=0) graphregion(margin(l=1 r=1 t=1 b=1)) xsize(6)iscale(.8 aspectratio(.6)
/* gr combine g1.gph g2.gph g3.gph, col(3) scheme(s2mono)iscale(1)graphregion(margin(l=1 r=1 t=1 b=1))xsize(20)saving(c1, replace) */
/* gr export `d'`n'.eps, as(eps)  replace mag(40) */
/* sh(epstopdf `d'`n'.eps) */
/* sh(xpdf `d'`n'.pdf &)  */

/* gr combine g4.gph g5.gph g6.gph, col(3) scheme(s2mono)iscale(1)graphregion(margin(l=1 r=1 t=1 b=1))xsize(20)saving(c2, replace) */
/* gr export `d'`n'.eps, as(eps)  replace mag(40) */
/* sh(epstopdf `d'`n'.eps) */
/* sh(xpdf `d'`n'.pdf &)  */

/* gr combine c1.gph c2.gph, col(1) scheme(s2mono)xsize(20) */
/* gr export `d'`n'.eps, as(eps)  replace mag(40) */
/* sh(epstopdf `d'`n'.eps) */
/* sh(xpdf `d'`n'.pdf &)  */




/*****************/
/* aok_hist2      */
/*****************/
*//drop the shading hor lines in the background have numbers at bins cutoffc for continous var, drop
*//axes divide freq by 1k and put that in note drop th eoutline
capture program drop aok_hist2
program define aok_hist2
syntax  ,x(string) d(string asis)f(string asis)  

sh(rm `d'`f'.tex)
sh(echo " \begin{figure}[H] \centering "   >> `d'`f'.tex)

cd `d'
macro drop _gr
preserve
local i=1
foreach v of varlist `x'{
cap qui tab `v'
if  _rc != 0  { //it would complain if too many val
local bin "bin(5)"
local note "[categories classified into 5 bins]"
}
else if `r(r)'>10 {
*/*   xtile q`v' = `v'<., nq(4) */
*/*   la var q`v' "`v' quartiles"  */
*/*   drop `v' */
*/*   ren q`v' `v' */
  local bin "bin(5)"
local note "[categories classified into 5 bins]"
}
else{
  local bin "discrete"
  qui sum `v'
  local ylab_val "`r(min)'(1)`r(max)'"
}

qui histogram `v',`bin'  frequency horizontal  ylabel(`ylab_val', labsize(large) angle(horizontal) valuelabel) xlabel(#4, labsize(large)format(%9.0fc))scheme(s2mono)legend(off) ytitle("")xtitle(, size(large))saving(g`i',replace)title("`:var lab `v''",span size(large))note(" `note' ",size(medium)) aspectratio(1,placement(right))plotregion(margin(l=0 r=0 t=3b=0))graphregion(margin(l=0 r=10 t=0 b=0))
qui gr export `f'`i'.eps, as(eps)  replace
qui sh(epstopdf `f'`i'.eps)
*sh(xpdf `f'`i'.pdf &)
macro drop _bin
macro drop _ylab*
macro drop _note
di "hi adam, it's me stata, running variable `i'"
if `i'==3 |`i'==6 |`i'==9 |`i'==12 |`i'==15|`i'==18{
  sh(echo "\subfloat{\includegraphics[width=1.7in]{`d'`f'`i'.pdf}}\\\\ \vspace{-.15in}">> `d'`f'.tex )
   }
   else {
  sh(echo "\subfloat{\includegraphics[width=1.7in]{`d'`f'`i'.pdf}}" >> `d'`f'.tex)
   }
local i=`i'+1
}

qui sh(echo "  \vspace{.15in} \caption{Variables' distribution.} \label{hist} \end{figure} " >> `d'`f'.tex )
restore
end 
*//combibing with gr merge sucks!   
   
*//imargin(l=0 r=0 t=0 b=0) graphregion(margin(l=1 r=1 t=1 b=1)) xsize(6)iscale(.8 aspectratio(.6)
/* gr combine g1.gph g2.gph g3.gph, col(3) scheme(s2mono)iscale(1)graphregion(margin(l=1 r=1 t=1 b=1))xsize(20)saving(c1, replace) */
/* gr export `d'`n'.eps, as(eps)  replace mag(40) */
/* sh(epstopdf `d'`n'.eps) */
/* sh(xpdf `d'`n'.pdf &)  */

/* gr combine g4.gph g5.gph g6.gph, col(3) scheme(s2mono)iscale(1)graphregion(margin(l=1 r=1 t=1 b=1))xsize(20)saving(c2, replace) */
/* gr export `d'`n'.eps, as(eps)  replace mag(40) */
/* sh(epstopdf `d'`n'.eps) */
/* sh(xpdf `d'`n'.pdf &)  */

/* gr combine c1.gph c2.gph, col(1) scheme(s2mono)xsize(20) */
/* gr export `d'`n'.eps, as(eps)  replace mag(40) */
/* sh(epstopdf `d'`n'.eps) */
/* sh(xpdf `d'`n'.pdf &)  */


/*****************/
/* aok_estout    */
/*****************/
  
capture program drop aok_estout
program define aok_estout

syntax , e(string asis) f(string asis) c(string asis)  o(string asis) order(string asis)

estout `e'  using `f' , replace style(tex)prehead(\begin{spacing}{1}\begin{table}[H]\centering\begin{scriptsize}\begin{tabular}{lllllllllllll} \hline\hline {Variable})posthead(\hline) prefoot(\hline) postfoot(\hline\hline \end{tabular}\end{scriptsize} \caption{`c'}\end{table}\end{spacing}) collabels(, none) `o' cells(b(star fmt(%9.3f))) stats(N ll aic bic, labels( "N" ) fmt(%9.0f %9.0f))varlabels(_cons Constant) starl(* .05 ** .01 *** .001) label  order(`order')
*//drop( _cons*  )
end

/*****************/
/* aok_estout2    */
/*****************/
  
capture program drop aok_estout2
program define aok_estout2

syntax , e(string asis) f(string asis) c(string asis)  o(string asis) order(string asis)

estout `e'  using `f' , replace style(tex)prehead(\begin{spacing}{1}\begin{table}[H]\centering\begin{scriptsize}\begin{tabular}{lllllllllllll} \hline\hline {Variable})posthead(\hline) prefoot(\hline) postfoot(\hline\hline \end{tabular}\end{scriptsize} \caption{`c'}\end{table}\end{spacing}) collabels(, none) `o' cells(b(star fmt(%9.3f))) stats(N ll aic bic, labels( "N" ) fmt(%9.0f %9.0f))varlabels(_cons Constant) starl(* .1 ** .05 *** .01) label  order(`order')
*//drop( _cons*  )
end

/************/
/* aok_grep */
/************/
//todo: faster to bulild one big text file with all vars and then just grep it :)

cap program drop aok_grep
program define aok_grep
syntax, where(string asis) key(string asis)
*//loc where /home/aok/desk/papers/root/data/wvs
*//loc key sat

loc list /home/aok/Desktop/junk/a

file close _all
set more off

! find `where' \(  -name "*.dta" \) -type f>`list'

file open f using `list', read
file read f line
while r(eof)==0{
  use using `line' if _n==1, clear
di "`line'"
  lookfor `key'
*//  d `r(varlist)'
*//can add more stuff

file read f line
  
}
end

/*************/
/* aok_phase */
/*************/
cap program drop aok_phase
program define aok_phase
syntax [if] [in], time_var(string asis) dv(string asis) range(string asis) filter_type(string asis) filter(string asis) d(string asis) gr_name(string asis)ti(string asis)
cap drop *__*

  preserve
  if "`if'"~="" | "`in'"~="" {
        keep `if'  `in'
  }
  
collapse `dv',by(`time_var')  
tokenize `range', parse("()")

tsset `time_var'

tostring `time_var', gen(__yy)

gen __yr=substr(__yy,3,2)

tssmooth `filter_type' `dv'__ma =`dv', `filter'
drop `dv'
ren `dv'__ma `dv'

gen `dv'__lag1=`dv'[_n-1]

l `time_var' __yy __yr `dv'*

count
  
*// 1 and 5 are the tokens from tokenize  
gen __l=`1' in 1
replace __l = `5' in `r(N)'


twoway (connected `dv' `dv'__lag1, lwidth(vvthin)msize(zero) msymbol(point) mlabel(__yr) mlabposition(2) mlabgap(tiny))(line __l __l,lcolor(gs12)), ytitle("time t;`filter_type':`filter'") xtitle("time t-1;`filter_type':`filter'")scheme(s1mono)ysize(5.5)scale(.58)xscale(range(`range'))yscale(range(`range'))legend(off)ti(`ti')
graph export `d'out/base/`gr_name'.eps, as(eps)  replace
sh(epstopdf `d'out/base/`gr_name'.eps)
! xpdf `d'out/base/`gr_name'.pdf &

cap drop *__*
  restore
end


/***************************/
/* rr: run r from stata :) */
/***************************/
global Rterm_options `"--vanilla --quiet"'
global Rterm_path `"/usr/bin/R"'

cap program drop rr
program define rr
*//this is good just for one graph...
*//can output interesting stuff with say  write.csv
version 10
syntax anything

file open rscript using /tmp/run_r_from_stata.r, write replace
*//here i am adding some useful stuff that i may want to modify...
save /tmp/dta_for_r.dta, replace
file write rscript `"library(foreign)"' _n
file write rscript `"dta<-read.dta("/tmp/dta_for_r.dta")"' _n
file write rscript `"attach(dta)"' _n
file write rscript `"pdf("/tmp/run_r_from_stata.pdf")"' _n
*// whatever i want to pass on...
file write rscript `"`anything'"' _n

*//nah, don't need that
*//file write rscript `"dev.off()"' _n

file close rscript
*//type /tmp/run_r_from_stata.r
rsource using /tmp/run_r_from_stata.r

! xpdf /tmp/run_r_from_stata.pdf &

end

//let's test it...
//sysuse auto
//sum mpg
//reg mpg price
//and from R
//rr summary(mpg); hist(mpg); lm(mpg ~ price) 


/***************/
/* aok_listtex */
/***************/

cap program drop aok_listtex
program define aok_listtex

 syntax varlist  [if] [in],path(string asis) cap(string asis)

  preserve
  if "`if'"~="" | "`in'"~="" {
        keep `if'  `in'
  }

cap macro drop _1
cap macro drop _2
cap macro drop _3
cap macro drop _4
cap macro drop _5
cap macro drop _6
cap macro drop _7
cap macro drop _8
tokenize `varlist'

listtex `1' `2' `3' `4' `5' `6' `7' `8'  using `path',type rstyle(tabular) head(" \begin{scriptsize}  \begin{center} \begin{longtable}{llllllllllllll} \caption{"`cap'"} \label{ls} \\ \hline \multicolumn{1}{p{.75in}}{"`: var label `1''"} & \multicolumn{1}{p{.75in}}{"`: var label `2''"}       & \multicolumn{1}{p{.75in}}{"`: var label `3''"}& \multicolumn{1}{p{.75in}}{"`: var label `4''"}& \multicolumn{1}{p{.75in}}{"`: var label `5''"}& \multicolumn{1}{p{.75in}}{"`: var label `6''"}& \multicolumn{1}{p{.75in}}{"`: var label `7''"} & \multicolumn{1}{p{.75in}}{"`: var label `8''"} \\ \hline \endfirsthead \multicolumn{3}{p{.75in}} {{\bfseries \tablename\ \thetable{} -- continued from previous page}} \\ \hline \multicolumn{1}{p{.75in}}{"`: var label `1''"} & \multicolumn{1}{p{.75in}}{"`: var label `2''"} & \multicolumn{1}{p{.75in}}{"`: var label `3''"}& \multicolumn{1}{p{.75in}}{"`: var label `4''"}& \multicolumn{1}{p{.75in}}{"`: var label `5''"} & \multicolumn{1}{p{.75in}}{"`: var label `6''"}& \multicolumn{1}{p{.75in}}{"`: var label `7''"} & \multicolumn{1}{p{.75in}}{"`: var label `8''"}\\ \hline \endhead \hline \multicolumn{5}{r}{{Continued on next page}} \\  \endfoot \hline \endlastfoot ") foot("\end{longtable} \end{center} \end{scriptsize}   ")replace
restore
end


/***************/
/* aok_listtex2 */
/***************/

cap program drop aok_listtex2
program define aok_listtex2
syntax, cap(string asis)fname(string asis)var(varlist) 

preserve
foreach v in `var'{ //if no var label, label it wth name
if ("`:var lab `v''" == ""){
la var `v' "`v'"
}
}

loc vv ""
foreach v in `var'{ //produce latex header
if ("`vv'" ==  ""){ 
loc vv `:var lab `v''
}
else{
loc vv `" `vv' & `:var lab `v'' "'
}
}

listtex `var'  using `fname',type rstyle(tabular) head(" \begin{scriptsize}  \begin{center} \begin{longtable}{lp{.18in}p{.18in}p{.18in}p{.18in}p{.18in}p{.18in}p{.18in}p{.18in}p{.18in}p{.18in}p{.18in}p{.18in}p{.18in}p{.18in}p{.18in}p{.18in}p{.18in}p{.18in}p{.18in}} \caption{`cap'} \label{hilo} \\ \hline `vv' \\ \hline \endfirsthead \multicolumn{3}{l} {{\bfseries \tablename\ \thetable{} -- continued from previous page}} \\ \hline `vv' \\ \hline \endhead \hline \multicolumn{5}{r}{{Continued on next page}} \\  \endfoot \hline \endlastfoot ") foot("\end{longtable} \end{center} \end{scriptsize}   ")replace
restore
end


/***************/
/* aok_listtex3 */
/***************/

//just have 2 wide fields for naming at the beginningh

cap program drop aok_listtex3
program define aok_listtex3
syntax, cap(string asis)fname(string asis)var(varlist) 

preserve
foreach v in `var'{ //if no var label, label it wth name
if ("`:var lab `v''" == ""){
la var `v' "`v'"
}
}

loc vv ""
foreach v in `var'{ //produce latex header
if ("`vv'" ==  ""){ 
loc vv `:var lab `v''
}
else{
loc vv `" `vv' & `:var lab `v'' "'
}
}

listtex `var'  using `fname',type rstyle(tabular) head(" \begin{scriptsize}\begin{center}\begin{longtable}{llp{.44in}p{.44in}p{.44in}p{.44in}p{.44in}p{.44in}p{.44in}p{.44in}p{.44in}}\caption{`cap'}\label{hilo}\\ \hline `vv'\\ \hline \endfirsthead \multicolumn{3}{l} {{\bfseries \tablename\ \thetable{} -- continued from previous page}} \\ \hline `vv' \\ \hline \endhead \hline \multicolumn{5}{r}{{Continued on next page}} \\  \endfoot \hline \endlastfoot ") foot("\end{longtable} \end{center} \end{scriptsize}   ")replace
restore
end




/***************/
/* aok_listtex4 */
/***************/

//just have 2 wide fields for naming at the beginningh

cap program drop aok_listtex4
program define aok_listtex4
syntax, cap(string asis)fname(string asis)var(varlist) 

preserve
foreach v in `var'{ //if no var label, label it wth name
if ("`:var lab `v''" == ""){
la var `v' "`v'"
}
}

loc vv ""
foreach v in `var'{ //produce latex header
if ("`vv'" ==  ""){ 
loc vv `:var lab `v''
}
else{
loc vv `" `vv' & `:var lab `v'' "'
}
}

listtex `var'  using `fname',type rstyle(tabular) head(" \begin{scriptsize}\begin{center}\begin{longtable}{p{2.44in}p{.44in}p{.44in}p{.44in}p{.44in}p{.44in}p{.44in}p{.44in}p{.44in}p{.44in}p{.44in}}\caption{`cap'}\label{hilo}\\ \hline `vv'\\ \hline \endfirsthead \multicolumn{3}{l} {{\bfseries \tablename\ \thetable{} -- continued from previous page}} \\ \hline `vv' \\ \hline \endhead \hline \multicolumn{5}{r}{{Continued on next page}} \\  \endfoot \hline \endlastfoot ") foot("\end{longtable} \end{center} \end{scriptsize}   ")replace
restore
end





/**************/
/* aok_forest */
/**************/

capture program drop aok_forest
program define aok_forest
 syntax  [if] [in], gr_name(string)  d(string asis) all(string) by(string) t(string asis) 
  preserve
  if "`if'"~="" | "`in'"~="" {
        keep `if'  `in'
  }
*-----------------------------------------
loc c `by'

local var_count : word count `all'

capture drop _s*
capture drop _lo_s* 
capture drop _hi_s*

   
foreach v in `all'{   
local varlab_`v' : var lab `v'
capture drop _s`v' 
capture drop _lo_s`v' 
capture drop _hi_s`v'

egen _s`v'=std(`v')
bys `c':egen _sd_s`v'=sd(_s`v')

bys `c':egen _n`v'=count(_s`v')
bys `c':egen _m`v'=mean(_s`v')
 
bys `c':gen _lo_s`v'=_m`v'-invttail(_n`v'-1,0.025)*_sd_s`v'/sqrt(_n`v')
bys `c':gen _hi_s`v'=_m`v'+invttail(_n`v'-1,0.025)*_sd_s`v'/sqrt(_n`v')

drop _m*
drop _n*
drop _sd*
*//double check if looks same as from ci command
*//bys `c': tab _lo_s`v'
*//bys `c': ci(_s`v')   
}

*// double check if the same as in ciplot   
*/* ciplot male cath2  , by(`c') hor saving(g2, replace) */
*/* graph export `d'out/base/test.eps, as(eps)  replace  */
*/* sh(epstopdf `d'out/base/test.eps) */
*/* sh(xpdf `d'out/base/test.pdf &) */

*/*   ciplot `all'  , by(`c') hor saving(g2, replace)   legend(size(tiny)) */
   
drop if `c'==.
collapse _s*  _lo_s* _hi_s* , by(`c')
des _*
reshape long _s _lo_s _hi_s, i(`c') j(_var) string

 *//try to put them in order  
tab  _var
tab _var, nola  mi
local i=1
gen ord=.   
foreach v in `all'{
  replace ord=`i' if _var=="`v'"
local i=`i'+1
}

 *// possibly try to do it here at once  ord instead of _var
local i=1

foreach v in `all'{   
replace _var="`varlab_`v''" if _var=="`v'"

label define aok1 `i' "`varlab_`v''" , add
local i=`i'+1
}
la val ord aok1
   
encode _var, gen(_n_var)

tab ord
tab _n_var   

*//this is wrong--doesn't work:( as seen with list   
*replace _n_var=_n_var+1000*ord
*l
*l, nola
*label variable  ord : _n_var

   
*//http://www.stata.com/help.cgi?graph_display    
twoway (scatter   ord _s, mcolor(white) msize(zero) msymbol(point) mlabel(`c') mlabcolor(black) mlabposition(12)) (rcap _lo_s _hi_s  ord, hor lcolor(gs7))  , xlabel(-.6(.2).4)ylabel(1(1)`var_count',valuelabels angle(horizontal) labs(medsmall)) legend(off) saving(g1, replace) ytitle("") aspectratio(, placement(east)) yscale(reverse) ti(`t')ytitle(,size(medsmall))xtitle(,size(medsmall))xlabel(, labsize(medsmall))scheme(s2mono)graphregion(margin(l=2 r=2 t=2 b=2))ysize(4)xtitle("standarized value")plotregion(margin(l=2 r=2 t=2 b=2))scale(.75)
graph export `d'out/base/`gr_name'.eps, as(eps)  replace mag(100)
sh(epstopdf `d'out/base/`gr_name'.eps)
sh(xpdf `d'out/base/`gr_name'.pdf &)

restore

end


capture program drop ggg
program define ggg

gr export g1.eps, replace
! okular g1.eps &
end

//BUG tyhre is sth buggy here! see circle_of_trust.do: aok_barcap, dv(trust)by(god_pray4)s(/tmp/god_pray4) or aok_barcap, dv(trust)by(sc4)s(/tmp/sc4)
 
cap program drop aok_barcap
program define aok_barcap
syntax, dv(varlist) by(varlist)s(string asis)[noo(string asis)][sav(string asis)]
preserve

loc dvlab :var lab `dv'
loc bylab :var lab `by'
qui ta `by'
loc nrows `r(r)'
loc lab : var lab `dv'
collapse (mean) m = `dv' (sd) sd = `dv' (count) n=`dv', by(`by')
generate hi = m + invttail(n-1,0.025)*(sd / sqrt(n))
generate lo = m - invttail(n-1,0.025)*(sd / sqrt(n))
/* encode var, gen(_njou) */
/* codebook _* */
/* recode _njou (8=8 "social science")(4=7 "humanities")(3=6 "history")(5=5 "law")(1=4 "arts")(2=3 "business, economics")(6=2 "medicine")(7=1 "natural science, math") , gen(njou) */

if (`"`sav'"' == ""){ 
loc sav g1
}
twoway (bar m `by', horizontal) (rcap hi lo `by',  horizontal), ytitle(" ") ylabel(#`nrows', labels angle(horizontal) valuelabel) legend(off) scheme(s2mono)note("`noo'")saving(`sav'.gph,replace)
//dump the note
if (`"`noo'"' == "0"){ 
twoway (bar m `by', horizontal) (rcap hi lo `by',  horizontal), xtitle(`dvlab')ytitle(`bylab') ylabel(#`nrows', labels angle(horizontal) valuelabel) legend(off) scheme(s2mono)
}
gr export `s'.eps, replace
! epstopdf `s'.eps
//! acroread `s'.pdf
/* ! emacs /tmp/bar.eps */
restore
end

cap program drop aok_barcapBIG
program define aok_barcapBIG
syntax, dv(varlist) by(varlist)s(string asis)[noo(string asis)][sav(string asis)]
preserve

loc dvlab :var lab `dv'
loc bylab :var lab `by'
qui ta `by'
loc nrows `r(r)'
loc lab : var lab `dv'
collapse (mean) m = `dv' (sd) sd = `dv' (count) n=`dv', by(`by')
generate hi = m + invttail(n-1,0.025)*(sd / sqrt(n))
generate lo = m - invttail(n-1,0.025)*(sd / sqrt(n))
/* encode var, gen(_njou) */
/* codebook _* */
/* recode _njou (8=8 "social science")(4=7 "humanities")(3=6 "history")(5=5 "law")(1=4 "arts")(2=3 "business, economics")(6=2 "medicine")(7=1 "natural science, math") , gen(njou) */

if (`"`sav'"' == ""){ 
loc sav g1
}
twoway (bar m `by', horizontal) (rcap hi lo `by',  horizontal), ytitle(" ") ylabel(#`nrows', labels angle(horizontal) valuelabel) legend(off) scheme(s2mono)note("`noo'")saving(`sav'.gph,replace)ylabel(, labsize(huge))xlabel(, labsize(huge))ytitle(, size(huge))xtitle(, size(huge))
//dump the note
if (`"`noo'"' == "0"){ 
twoway (bar m `by', horizontal) (rcap hi lo `by',  horizontal), xtitle(`dvlab')ytitle(`bylab') ylabel(#`nrows', labels angle(horizontal) valuelabel) legend(off) scheme(s2mono)ylabel(, labsize(huge))xlabel(, labsize(huge))ytitle(, size(huge))xtitle(, size(huge))
}
gr export `s'.eps, replace
! epstopdf `s'.eps
//! acroread `s'.pdf
/* ! emacs /tmp/bar.eps */
restore
end


cap program drop aok_var_des
program define aok_var_des
syntax , ff(varlist) fname(string asis)
file open f using `fname', write replace
file write f "\begin{table}[H]\centering\footnotesize" _n
file write f " \caption{\label{var_des} Variable definitions.}" _n
file write f "\begin{tabular} {p{1.5in}p{4.5in}}   \hline"  _n
file write f "name & description   \\ \hline" _n
foreach v in `ff' {
if (`"`: char `v'[note1]'"' ==`""'){ //if no note do nothing
di "no note for `v'"
}
else {
file write f `"  `: variable label `v'' & `: char `v'[note1]' \\"' _n
}
}
file write f "\hline\end{tabular}\end{table}"
file close f
//! sed -i  '/^\s\s\\%\suninsured/i\ \\hline' /tmp/var.tex
//! sed -i 's|\"||g' `fname'
*//"
end

cap program drop sr
program define sr
syntax , y(varlist)x(varlist)dir(string asis)[f(string asis)pdf(string asis)]
if (`"`f'"' == "") local  f "lfit"
if (`"`pdf'"' == "") local  pdf "yes"
qui corr `y' `x'
local r : di %9.2f `r(rho)'
tw(scatter  `y' `x',mlabel(ctr)msymbol(none)mlabsiz(vsmall))(`f' `y' `x'), ytitle(`:var lab `y'')xtitle(`:var lab `x'') legend(off)scheme(s2mono)note(correlation:`r')
 
gr export `dir'`y'_`x'.eps,replace
! epstopdf `dir'`y'_`x'.eps
if (`"`pdf'"' == "yes"){
! acroread `dir'`y'_`x'.pdf
}
end


cap program drop aok_vlab
program define aok_vlab
//puts numeric val label infront of string, sep by .  
syntax varlist
foreach var in `varlist'{
local labname : value label `var'

codebook `var', tab(100)
sum `var'
local first_tab `r(min)'
local last_tab `r(max)' 

forval a=`first_tab'/`last_tab'{  
local label_`a' : label `labname' `a'
if ( "`label_`a''" !="`a'" ){ //don't label if no val label
la define `labname' `a' "`a'.`label_`a''", modify 
}
la val `var' `labname'
}
}
end


capture program drop aok_drop  
program define aok_drop
syntax ,vv(varlist)by(string asis)
foreach v of varlist `vv' { 
drop if `v'==.
}
di "----------now dropping by stratum---------------------"
bys `by': gen stratum_obs_counter=_n
drop if stratum_obs_counter==1 & stratum_obs_counter[_n+1] !=2
drop stratum_obs_counter
end




cap program drop aCorr
program define aCorr
syntax varlist
preserve
//http://www.statalist.org/forums/forum/general-stata-discussion/general/127392-exporting-pwcorr-matrix-i-m-confused
local vbles `varlist'
//local vbles: list sort vbles
capture postutil clear
tempfile corrs
postfile handle str32 var1 str32 var2 float r_ using `corrs'
local nvars: word count `vbles'

foreach v of var * {
local l`v' : variable label `v'
      if `"`l`v''"' == "" {
	local l`v' "`v'"
	}
}

forvalues i = 1/`nvars' {
     local v1: word `i' of `vbles'
     forvalues j = `=`i'+1'/`nvars' {
          local v2: word `j' of `vbles'
          corr `v1' `v2'
          post handle  ("`v1'") ("`v2'") (`r(rho)')
     }
}
postclose handle
//  AT THIS POINT THE POST FILE CONTAINS ALL OF THE OFF-DIAGONAL CORRELATIONS, BUT IN LONG FORMAT
//  MAY WANT TO PRESERVE EXISTING DATA BEFORE PROCEDING
use `corrs', clear
reshape wide r_, i(var1) j(var2) string
rename r_* *  // REQUIRES A MODERN STATA THAT HAS GROUP RENAMING
/* order _all, alphabetic */
/* order var1, first */
/* sort var1 */
//  THE DATA IN MEMORY IS NOW THE UPPER DIAGONAL CORRELATION MATRIX
// FEEL FREE TO -export excel- or -export delimited- or -save-, or whatever.

//make it in the right order as the input had it
local vbles1 `: word 1 of `vbles''
//di "`vbles1'"
gen `vbles1' =.
order var1 `vbles'

local vblesL `: word `nvars' of `vbles''


expand 2 in l
replace var1="`vblesL'" in l
replace `vblesL'=. in l

loc i=1
gen ordC=.
foreach e in `vbles'{
replace ordC = `i' if var1=="`e'"
loc i=`i'+1
}

foreach v of var * {
label var `v' "`l`v''"
}

sort ordC
format `vbles' %9.2f
drop `vbles1' ordC
drop in l
l
ren var1 variable

//logout, save(/tmp/corr) tex replace: list
dataout, save(/tmp/corr)  tex replace dec(2)

! cat /tmp/corr.tex
! sed -i '1,4d' /tmp/corr.tex
//right align and fixed width:)))
! sed -i '1 i\ \\newcolumntype{R}{>{\\raggedleft\\arraybackslash}p{.6in}} \\begin{tabular}{@{} lRRRRRRRR @{}} \\hline' /tmp/corr.tex

! sed -i '$ d' /tmp/corr.tex

gen `vbles1'=.
label var `vbles1' "`l`vbles1''"

foreach e in `vbles'{
! sed -i -e 's/`e'/`:var lab `e''/g' /tmp/corr.tex
}


//texsave  using /tmp/corr, loc(h) headlines("\begin{center}" ) footlines( "\end{center}") replace
restore
//remember almosty always sig, just say in footnte
pwcorr `vbles', star(.05)
pwcorr `vbles', sig
end



* program generating stata dic from eurostat labels files
//made this into program
/*
insheet using /home/aok/data/eurostat/dic/2016-12-11-/en/indic_ur.dic, clear tab
ren v1 var
ren v2 lab
file open dic using 2016-12-11-urb_percep.dic, write replace
qui count
forval i=1/`r(N)' {
loc var = var[`i']
loc lab =  lab[`i']
file  write dic  "cap la var" _tab  "`var'" _tab `" " "' "`lab'" `" " "' _n
}
file close dic
*/
cap program drop aLab
program define aLab
preserve
syntax, in(string asis)out(string asis)
insheet using `in', clear tab
ren v1 var
ren v2 lab
file open dic using `out', write replace
qui count
forval i=1/`r(N)' {
loc var = var[`i']
loc lab =  lab[`i']
file  write dic  "cap la var" _tab  "`var'" _tab `" " "' "`lab'" `" " "' _n
}
file close dic
restore
end
