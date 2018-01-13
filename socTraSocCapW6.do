

//TODOand have commands in the template like aokhist listtex stepwise etc

/* have most of the stuff outputted to online appendix:)--start with that and then */
/* select stuff to paper--have brief narrative describng patterns in online app too */

/* have one section for data mining that outputs graphs scatterplots lfits forrestplots dotplots and */
/* regressions to res in pdf and one that actually does stuff for the paper !!! */

/* and have commands in the template like aokhist listtex stepwise etc */

/* *E.G. many additional vars commented out that can be used in the future   */
/* * double check everything with "assert" inspect etc */

/* rememebr to run!! */
/* git add *.do *.tex  *.org -n */
stata
clear                                  
capture set maxvar 10000
version 14                             
set more off                           
run ~/papers/root/do/aok_programs.do

loc d = "/home/aok/misc/grants/poland/leszekMorawskiVistula/"       

capture mkdir "`d'scr"
capture mkdir "/tmp/shareElderly"
loc pap shareElderly
loc tmp "/tmp/`pap'/"

//file open tex using `d'out/tex, write text replace
//file write tex `"%<*ginipov>`:di  %9.2f `r(rho)''%</ginipov>"' 

file open f using `d'notes.txt, write replace
file write f  "a note...." _n _n


! time stata -b do  /home/aok/misc/grants/poland/leszekMorawskiVistula/creW6.do  &
//! time stata -b do /home/aok/misc/grants/poland/leszekMorawskiVistula/creW4.do  & 
//meh just do w6; dofiule for w4 out of date
use `tmp'allW6,clear
ren swb ls
la var ls "life satisfaction"


** weights:  see per weights in data/share n.org
//!!just use pwesight as per share email


**** descriptive stats


sum ub sa disab,det //phmmm very small!!
foreach v of varlist ub sa disab{
count if `v'==0
}


codebook ls, ta(100)
codebook ghto

corr ls swbAC 

tabstat ghto volCha, by(cs) format(%9.2f) //good checks out!! like what we know

//svy: reg ls ghto
reg ls ghto, robust cluster(country)

bys cs: reg ls ghto, robust 

ta emp
sum volCha* ghto
pwcorr volCha* ghto
sum ypen*

d y*

//ghto rhfo

loc x ls casp volCha volChaO ac035d4 ac035d5 ac035d7 ac035d8 ac035d9 ac035d10 ypen1 ypen2 yreg1 pen disab ub sa labInc hnetw yedu age male  health mar emp ngrchild
sum `x'

cap file close f

aok_var_des , ff(ls casp)fname(`tmp'var_des-1.tex)
! sed -i 's/Variable definitions/Variable definitions: dependent variables./g' `tmp'var_des-1.tex
aok_var_des , ff(volCha volChaO ac035d4 ac035d5 ac035d7 ac035d8 ac035d9 ac035d10)fname(`tmp'var_des-2.tex)
! sed -i 's/Variable definitions/Variable definitions: social activities./g' `tmp'var_des-2.tex
aok_var_des , ff(ypen1 ypen2 yreg1 pen disab  ub sa)fname(`tmp'var_des-3.tex)
! sed -i 's/Variable definitions/Variable definitions: social transfers./g' `tmp'var_des-3.tex
aok_var_des , ff(labInc hnetw yedu age male health disabled mobility mar emp ngrchild)fname(`tmp'var_des-4.tex)
! sed -i 's/Variable definitions/Variable definitions: control variables./g' `tmp'var_des-4.tex


/* ! sed -i '/^  voluntary or charity work/i\\\hline {\\bf social capital:}&\\\\' `tmp'var_des.tex */
/* ! sed -i '/^  annual old age/i\\\hline {\\bf social transfers:}&\\\\' `tmp'var_des.tex */
/* ! sed -i '/^  labor income/i\\\hline {\\bf control variables:}&\\\\' `tmp'var_des.tex */

 

aok_hist2,x(ls casp)d(`tmp')f(hist-1)
aok_hist2,x(volCha volChaO ac035d4 ac035d5 ac035d7 ac035d8 ac035d9 ac035d10)d(`tmp')f(hist-2)
aok_hist2,x(ypen1 ypen2 yreg1 pen disab ub sa )d(`tmp')f(hist-3)
aok_hist2,x(labInc hnetw yedu age male  health disabled mobility  mar emp ngrchild)d(`tmp')f(hist-4)



**** regressions

*** first playing




*** quasi final regressions


** social transfers

//LATER:thereis also looking after grandchildern,but guess not in imputed and many many missing


/*A:
1 income:
2 labor
3 replacement (instead of labor, emerytury, renty etc)
4 transfers 
*/

//inc--guess would count tings again
reg ls labInc i.country [pw=cciw_w6], beta robust
estadd beta
est sto a1

d pen ub sa disab
corr pen ub sa disab 
sum pen ub sa disab
count
count if pen>0
count if ub>0
count if sa>0
count if disab>0

reg ls labInc  pen i.country [pw=cciw_w6], beta robust
estadd beta
est sto a2

//neg even contrlling for hea and emp
//reg ls labInc inc pen ub sa disab hea emp, beta robust

//liab otrf--guess already incl in hnetw
reg ls labInc  pen ub sa disab    hnetw i.country [pw=cciw_w6], beta robust //missing for many perho
estadd beta
est sto a3

reg ls labInc pen ub sa disab   hnetw male mar emp age age2  yedu ngrchild disabled mobility health i.country [pw=cciw_w6], beta robust //missing for many perho
estadd beta
est sto a4

reg casp labInc i.country [pw=cciw_w6], beta robust
estadd beta
est sto a5

reg casp labInc  pen i.country [pw=cciw_w6], beta robust
estadd beta
est sto a6

reg casp labInc  pen ub sa disab    hnetw i.country [pw=cciw_w6], beta robust 
estadd beta
est sto a7

reg casp labInc pen ub sa disab   hnetw male mar emp age age2  yedu ngrchild disabled mobility health i.country [pw=cciw_w6], beta robust 
estadd beta
est sto a8



estout a* ,style(tab)  cells(beta(star fmt(%9.2f))) replace  collabels(, none) stats(N, labels("N")fmt(%9.0f))

estout a1 a2 a3 a4 a5 a6 a7 a8 using `tmp'regAw6.tex,style(tex)  cells(beta(star fmt(%9.2f))) replace  collabels(, none) stats(N, labels("N")fmt(%9.0f))drop(*country*)varlabels(_cons constant) label starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001)
//in w6 pensions negative and only later flip to positive--explanation--collinearity--they pick up from other bad stuff like bad health etc!! and once control for these all good then  
//! sed -i '/^constant/i\country dummies&no&no&no&no&yes\\\\' `tmp'regAw6.tex
! sed -i '1 i\&\\multicolumn{4}{c}{Life satisfaction}&\\multicolumn{4}{c}{CASP}\\\\' `tmp'regAw6.tex


** social capital


/*B:
1 volunteering only
2 soc cap ac vars
3 income
4 other controls
*/

//no ac035d6 in w6!!

reg ls VCO2-VCO5 i.country [pw=cciw_w6], robust beta
estadd beta
est sto b1
reg ls VCO2-VCO5 ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 i.country [pw=cciw_w6], robust beta 
estadd beta
est sto b2

d ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10

reg ls VCO2-VCO5 ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 emp i.country [pw=cciw_w6], robust beta
estadd beta
est sto b3


reg ls VCO2-VCO5 ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age age2  yedu ngrchild disabled mobility health i.country [pw=cciw_w6], beta robust 
estadd beta
est sto b4


reg casp VCO2-VCO5 i.country [pw=cciw_w6], robust beta
estadd beta
est sto b5
reg casp VCO2-VCO5 ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 i.country [pw=cciw_w6], robust beta 
estadd beta
est sto b6

d ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10


reg casp VCO2-VCO5 ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 emp i.country [pw=cciw_w6], robust beta
estadd beta
est sto b7

reg casp VCO2-VCO5 ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age age2  yedu ngrchild disabled mobility health i.country [pw=cciw_w6], beta robust 
estadd beta
est sto b8



estout b* ,style(tab)  cells(beta(star fmt(%9.2f))) replace  collabels(, none) stats(N, labels("N")fmt(%9.0f))

estout b* using `tmp'regBw6.tex,style(tex)  cells(beta(star fmt(%9.2f))) replace  collabels(, none) stats(N, labels("N")fmt(%9.0f))drop(_cons *country*)varlabels(_cons constant) label starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001)

//! sed -i '/^constant/i\country dummies&no&no&no&yes\\\\' `tmp'regBw6.tex
! sed -i '/^volChaOft==less often/i\\{voluntary or charity work:}&&&&\\\\' `tmp'regBw6.tex
! sed -i 's/volChaOft==/\\hspace{.25in}/g' `tmp'regBw6.tex
! sed -i '1 i\&\\multicolumn{4}{c}{Life satisfaction}&\\multicolumn{4}{c}{CASP}\\\\' `tmp'regBw6.tex



**C: substytucja

 
reg ls VCO2-VCO5 pen ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 i.country [pw=cciw_w6], robust beta
estadd beta
est sto c1


reg ls VCO2-VCO5 pen ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab   hnetw male mar emp age age2  yedu ngrchild disabled mobility health i.country [pw=cciw_w6], beta robust  
estadd beta
est sto c2

corr ls casp

reg casp VCO2-VCO5 pen ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 i.country [pw=cciw_w6], robust beta
estadd beta
est sto c3


reg casp VCO2-VCO5 pen ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab   hnetw male mar emp age age2  yedu ngrchild disabled mobility health i.country [pw=cciw_w6], beta robust  
estadd beta
est sto c4

estout c* ,style(tab)  cells(beta(star fmt(%9.3f))) replace  collabels(, none) stats(N, labels("N")fmt(%9.0f))

estout c1 c2 c3 c4 using `tmp'regCw6.tex,style(tex)  cells(beta(star fmt(%9.2f))) replace  collabels(, none) stats(N, labels("N")fmt(%9.0f))drop(_cons *country*) label starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001)
//yay so actually stronger effect than in w4 :) so no more 2/3 of effect if anything stronger on volunteering!!

! sed -i '1 i\&\\multicolumn{2}{c}{Life satisfaction}&\\multicolumn{2}{c}{CASP}\\\\' `tmp'regCw6.tex
//! sed -i '/^constant/i\country dummies&no&no&yes&yes&no&no&yes&yes\\\\' `tmp'regCw6.tex
! sed -i '/^volChaOft==less often/i\\{voluntary or charity work:}&&&&&&&&\\\\' `tmp'regCw6.tex
! sed -i 's/volChaOft==/\\hspace{.25in}/g' `tmp'regCw6.tex


** checking nonlinearlity!

gen labInc2=labInc^2

>>>di _b[labInc]/(2*_b[labInc2])
etc etc
and margins

reg casp VCO2-VCO5 pen   ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 c.labInc##c.labInc ub sa disab   hnetw male mar emp age age2  yedu ngrchild disabled mobility health i.country [pw=cciw_w6], beta robust  
margins, at(labInc=(0(10)100)) //duh way outside of range of data!
marginsplot
dy

reg casp VCO2-VCO5 labInc  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 c.pen##c.pen ub sa disab   hnetw male mar emp age age2  yedu ngrchild disabled mobility health i.country [pw=cciw_w6], beta robust  
margins, at(pen=(0(10)200)) //duh way outside of range of data!
marginsplot
dy

reg casp VCO2-VCO5 labInc pen ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 c.hnetw##c.hnetw ub sa disab  male mar emp age age2  yedu ngrchild disabled mobility health i.country [pw=cciw_w6], beta robust  
margins, at(hnetw=(0(200)4000)) //duh way outside of range of data!
marginsplot
dy

reg casp VCO2-VCO5 pen   ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc labInc2 ub sa disab   hnetw male mar emp age age2  yedu ngrchild disabled mobility health i.country [pw=cciw_w6], beta robust  

reg casp VCO2-VCO5 pen   ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc labInc2 ub sa disab   hnetw male mar emp age age2  yedu ngrchild disabled mobility health i.country [pw=cciw_w6], beta robust  

reg casp  volCha##c.inc  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10    hnetw male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6], beta robust  
//interesting!! thexp
margins volCha, at(inc=(0(20000)100000))
marginsplot, x(inc)  //plot1opts(lpattern(dot))title(" ")legend(off)
dy

!!!TODO
>>>??? different from the graph in paper! why??

and do it with other measures and check where it flips and do picture


//MEH DO NOT REPORT W4 in paper, skip it meh
/* ** copmbine both tables  */



/* reg ls VCO2-VCO5 pen  [pw=cciw_w6], robust beta */
/* estadd beta */
/* est sto c1W6 */


/* reg ls VCO2-VCO5 pen ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab   hnetw male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6], beta robust   */
/* estadd beta */
/* est sto c2W6 */

/* corr ls casp */

/* reg casp VCO2-VCO5 pen  [pw=cciw_w6], robust beta */
/* estadd beta */
/* est sto c3W6 */


/* reg casp VCO2-VCO5 pen ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab   hnetw male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6], beta robust   */
/* estadd beta */
/* est sto c4W6 */


/* //! time stata -b do  /home/aok/misc/grants/poland/leszekMorawskiVistula/creW4.do */
/* use `tmp'allW4,clear */


/* reg ls VCO2-VCO5 pen  [pw=cciw_w4], robust beta */
/* estadd beta */
/* est sto c1W4 */


/* reg ls VCO2-VCO5 pen ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab   hnetw male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w4], beta robust   */
/* estadd beta */
/* est sto c2W4 */

/* corr ls casp */

/* reg casp VCO2-VCO5 pen  [pw=cciw_w4], robust beta */
/* estadd beta */
/* est sto c3W4 */


/* reg casp VCO2-VCO5 pen ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab   hnetw male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w4], beta robust   */
/* estadd beta */
/* est sto c4W4 */

/* estout c1W6 c1W4 c2W6 c2W4 c3W6 c3W4 c4W6 c4W4 using `tmp'regCw6w4.tex,style(tex)  cells(beta(star fmt(%9.2f))) replace  collabels(, none) stats(N, labels("N")fmt(%9.0f))drop(*country*)varlabels(_cons constant) label starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001) */

/* ! sed -i '1 i\&\\multicolumn{4}{c}{Life satisfaction}&\\multicolumn{4}{c}{CASP}\\\\' `tmp'regCw6w4.tex */
/* ! sed -i '/^constant/i\country dummies&no&no&yes&yes&no&no&yes&yes\\\\' `tmp'regCw6w4.tex */
/* ! sed -i '/^volChaOft==less often/i\\{voluntary or charity work:}&&&&&&&&\\\\' `tmp'regCw6w4.tex */
/* ! sed -i 's/volChaOft==/\\hspace{.25in}/g' `tmp'regCw6w4.tex */



//---------------------------------------------------------------------------
*** interacting!


use `tmp'allW6,clear


** perhaps at some point enough; look at quintiles quadratics etc of pensions

sum pen, det

//meh either 120k with quadratics or like 20k,25k with quintiles

//many zeros so did quadratics
xtile penQ = pen, nq(5)
_pctile pen, nq(5)
return list

_pctile pen, nq(10)
return list
recode pen (0=0)(0/5000=1)(5000/10000=2)(10000/20000=3)(20000/100000000=4),gen(penC)
ta penC, gen(PC)

//replace pen=pen/1000000

gen pen2=pen^2



reg ls labInc i.penQ [pw=cciw_w6], beta robust
reg ls labInc i.penQ ub sa disab    hnetw male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6], beta robust 

reg ls labInc pen pen2 [pw=cciw_w6], beta robust
di (_b[pen]/(-2* _b[pen2])) //*1000000
//so flips like at 120k


reg ls labInc pen pen2 ub sa disab    hnetw male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6], beta robust 
di (_b[pen]/(-2* _b[pen2])) //*1000000


reg ls labInc pen i.penC [pw=cciw_w6], beta robust
reg ls labInc i.penCAT ub sa disab  male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6], beta robust 


reg ls labInc PC2-PC4 ub sa disab  hnetw male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6], beta robust 

reg casp VCO2-VCO5 PC2-PC4 ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab   hnetw male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6], beta robust  


** yeah pen is very low, most ppl dont have it so guess need to take whole income into acct

//meh too!

//LATER can look at thinc2!!

xtile incQ = inc, nq(5)
_pctile inc, nq(5)
return list

xtile incD = inc, nq(10)
_pctile inc, nq(10)
return list

count if pen>inc //weird!!


//may also dummy out hnetw
reg casp VCO2-VCO5 PC2-PC4 i.incQ  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab  hnetw male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6], beta robust  





//----------------------------------------------------------------------

*** for whom? by income! :)


** playing

so the more income/wealth you have, the less volunteering matter!
so guess explanatyion is that volunteering can help more if one hersolf doesnt
have muchg 


but the more pension you have, the more it matters--not having any pension and
volunteering doesnt help a lot but having some pendion and voluntering is
great!; but onece one reaches high pension then again it doesnt help that much

se yeah need some money for volunteering to help but having a lot of money
it doesnt help! 


TODO!!!! (from gmail)
see vol for ppl with 0 pension, maybe interactions , maybe subset on other transfers and soc capitals too!


reg casp  i.volCha##c.pen ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab   hnetw male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6], beta robust  
//yeah, so need some money to be able to volunteer!


//!! TOPAPER: and say tried nonliner but pretty linear the higher $ the less vol useful!
reg casp  volCha##c.inc  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10    hnetw male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6], beta robust  
//interesting!! thexp
margins volCha, at(inc=(0(20000)100000))
marginsplot, x(inc)  //plot1opts(lpattern(dot))title(" ")legend(off)
dy

//say that same for theinc2
reg casp  i.volCha##c.thinc2  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  hnetw male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6], beta robust  

//!! TOPAPER and say tried nonliner but pretty linear the higher $ the less vol useful!
reg casp  volCha##c.hnetw ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 inc  male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6], beta robust  
//interesting!! thexp
margins volCha, at(hnetw=(0(200000)1000000))
marginsplot, x(hnetw)  //plot1opts(lpattern(dot))title(" ")legend(off)
dy


reg casp VCO2-VCO5 pen ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab   hnetw male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6], beta robust  



reg casp  i.volCha##i.penC ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab   hnetw male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6], beta robust  

reg casp  i.volCha##i.incD ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab   hnetw male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6], beta robust  

reg casp  i.volCha##i.incD ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 pen ub sa disab   hnetw male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6], beta robust  


xtile hnetwQ = hnetw, nq(5)
_pctile hnetw, nq(5)
return list

reg casp  i.volCha##hnetwQ ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab   hnetw male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6], beta robust  


xtile hnetwD = hnetw, nq(10)
_pctile hnetw, nq(10)
return list

reg casp  i.volCha##hnetwD ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab    male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6], beta robust  


sum inc, det
sum hnetw, det
sum pen, det
sum liab, det
sum  otrf, det

count if inc<1000 & hnetw < 5000 & pen<1000



** combine income by hand and corr


use `tmp'allW6,clear

gen aokInc=labInc+ypen1+ypen2+ypen3+ypen4+ypen5+ypen6+ylsp1+ylsp2+ylsp3+ylsp4+ylsp5+ylsp6+yreg1+yreg2+ylsr1+ylsr2+ysrent+yaohm+ybabsmf+yincnrp

//TODO: think if substract theexp and if maybe reuse some of these items in regressions earlier as opposed to just pen ub sa etc--if using hust these few them motivate that these are most important and for parisomony just keep them but then do robuistness check using all later


corr aokInc inc thinc2



** variou sscenarios

//super poor
reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6] if inc<500 & hnetw < 2500 & pen<1000, beta robust  

//poor
reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6] if inc<1000 & hnetw < 5000 & pen<1000, beta robust  

//medium
reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6] if (inc<5000 & hnetw < 25000 & pen<5000)&((hnetw > 5000 |inc>5000 | pen>5000)& liab<10000), beta robust  

reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6] if (inc<10000 & hnetw < 50000 & pen<10000)&((hnetw > 10000 |inc>5000 | pen>5000)& liab<10000), beta robust  


//rich: here those that volunteer a lot are happy!! others less!
 reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6] if ((hnetw > 50000 &(inc>10000 | pen>5000)& liab<10000)), beta robust 

//very rich
 reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6] if ((hnetw > 100000 &(inc>20000 | pen>10000)& liab<10000)), beta robust 


//super rich
 reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6] if ((hnetw > 250000 &(inc>20000 | pen>10000)& liab<10000)), beta robust 


//------
//!! MAYBE TOPAPER: assume 2 scenarios:
//bottom line: the rich benefit 2x less from volunteering than middle class and poor!

//rich
 reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6] if ((hnetw > 300000 & liab<10000))|(inc>100000&thexp<20000), beta robust 

//middle class only
reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6] if (inc<10000 & hnetw < 50000 & pen<10000)&((hnetw > 10000 |inc>5000 | pen>5000)& liab<10000), beta robust  

//non-rich
 reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6] if ((hnetw < 100000))&(inc<50000), beta robust 

 reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6] if ((hnetw < 50000)), beta robust 


//middle class 
reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6] if (inc<50000 & hnetw < 100000)&(inc>1000 & hnetw > 5000), beta robust  

reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age age2  yedu ngrchild health i.country [pw=cciw_w6] if (inc<50000 & hnetw < 100000), beta robust  


** PAPER


use `tmp'allW6,clear


reg ls volCha##c.inc  hnetw ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age age2  yedu ngrchild mobility disabled health i.country [pw=cciw_w6], beta robust  //thexp
estadd beta
est sto d1

margins volCha, at(inc=(0(20)100))
marginsplot, x(inc)  plot2opts(lpattern(dot))title(" ")legend(off)saving(d1.gph,replace)ysc(range(7.4(.2)8))ylab(7.4(.2)8)ytit("predicted life satisfaction")
//dy


reg ls volCha##c.hnetw inc  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age age2  yedu ngrchild  mobility disabled health i.country [pw=cciw_w6], beta robust  //thexp
estadd beta
est sto d2

margins volCha, at(hnetw=(0(200)1000))
marginsplot, x(hnetw)  plot2opts(lpattern(dot))title(" ")legend(off)saving(d2.gph,replace)ysc(range(7.4(.2)8))ylab(7.4(.2)8)ytit("predicted life satisfaction")
//dy

reg ls volCha##c.pen inc hnetw ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age age2  yedu ngrchild  mobility disabled health i.country [pw=cciw_w6], beta robust  //thexp
estadd beta
est sto d3

margins volCha, at(pen=(0(5)30))
marginsplot, x(pen)  plot2opts(lpattern(dot))title(" ")legend(off)saving(d3.gph,replace)ysc(range(7.4(.2)8))ylab(7.4(.2)8)ytit("predicted life satisfaction")
//dy


reg casp volCha##c.inc  hnetw ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age age2  yedu ngrchild  mobility disabled health i.country [pw=cciw_w6], beta robust  //thexp
estadd beta
est sto d4

margins volCha, at(inc=(0(20)100))
marginsplot, x(inc)  plot2opts(lpattern(dot))title(" ")legend(off)saving(d4.gph,replace)ysc(range(-.1(.1).3))ylab(-.1(.1).3)ytit("predicted CASP")
//dy

reg casp volCha##c.hnetw inc  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age age2  yedu ngrchild  mobility disabled health i.country [pw=cciw_w6], beta robust  //thexp
estadd beta
est sto d5

margins volCha, at(hnetw=(0(200)1000))
marginsplot, x(hnetw)  plot2opts(lpattern(dot))title(" ")legend(off)saving(d5.gph,replace)ysc(range(-.1(.1).3))ylab(-.1(.1).3)ytit("predicted CASP")
//dy

reg casp volCha##c.pen inc hnetw ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age age2  yedu ngrchild  mobility disabled health i.country [pw=cciw_w6], beta robust  //thexp
estadd beta
est sto d6

margins volCha, at(pen=(0(5)30))
marginsplot, x(pen)  plot2opts(lpattern(dot))title(" ")legend(off)saving(d6.gph,replace)ysc(range(-.1(.1).3))ylab(-.1(.1).3)ytit("predicted CASP")
//dy


estout d1 d2 d3 d4 d5 d6 using `tmp'regDw6.tex,style(tex)  cells(b(star fmt(%9.4f))) replace  collabels(, none) stats(N, labels("N")fmt(%9.0f))drop(*country*)varlabels(_cons constant) label starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001)
//yay so actually stronger effect than in w4 :) so no more 2/3 of effect if anything stronger on volunteering!!

! sed -i '/^no vol/d' `tmp'regDw6.tex


! sed -i '1 i\&\\multicolumn{3}{c}{Life satisfaction}&\\multicolumn{3}{c}{CASP}\\\\' `tmp'regDw6.tex
//! sed -i '/^constant/i\country dummies&yes&yes&yes&yes&yes&yes&yes&yes\\\\' `tmp'regDw6.tex
/* ! sed -i '/^volChaOft==less often/i\\{voluntary or charity work:}&&&&&&&&\\\\' `tmp'regDw6.tex */
/* ! sed -i 's/volChaOft==/\\hspace{.25in}/g' `tmp'regDw6.tex */


gr combine d1.gph d2.gph d3.gph d4.gph d5.gph d6.gph, row(2)
dy
gr export `tmp'regDmarg.pdf, replace

* if bigger for retired ppl: can check is effect of volunteering stronger among retired persons? and by pension



** dec 2017 trying other things

//meh can just have 0/1 vol 
/* reg casp i.volChaOft##c.inc  hnetw ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age age2  yedu ngrchild  mobility disabled health i.country [pw=cciw_w6], beta robust  //thexp */
/* estadd beta */
/* est sto d6 */

/* margins volChaOft, at(inc=(0(200)1000)) */
/* marginsplot, x(inc) ytit("predicted CASP") */
/* gr export /tmp/a.pdf */
/* ! scp  /tmp/a.pdf akozaryn@rce.hmdc.harvard.edu:~/public_html/tmp/ */




***TO APPENDIX THESE TWO:


reg ls volCha##emp i.country [pw=cciw_w6], robust beta
estadd beta
est sto e1

reg ls volCha##emp inc  hnetw ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar  age age2  yedu ngrchild  mobility disabled health i.country [pw=cciw_w6], beta robust  //thexp
estadd beta
est sto e2

reg ls volCha##c.pen i.country [pw=cciw_w6], robust beta
estadd beta
est sto e3

reg ls volCha##c.pen inc ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age age2  yedu ngrchild  mobility disabled health i.country [pw=cciw_w6], beta robust  // thexp
estadd beta
est sto e4

/* margins volCha, at(hnetw=(0(200)1000)) */
/* marginsplot, x(hnetw)  plot2opts(lpattern(dot))title(" ")legend(off)saving(d4.gph,replace)ysc(range(7.4(.2)8))ylab(7.4(.2)8)ytit("predicted life satisfaction") */
/* //dy */


reg casp volCha##c.emp i.country [pw=cciw_w6], robust beta
estadd beta
est sto e5

reg casp volCha##c.emp  hnetw ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age age2  yedu ngrchild  mobility disabled health i.country [pw=cciw_w6], beta robust  //thexp
estadd beta
est sto e6

reg casp volCha##c.pen i.country [pw=cciw_w6], robust beta
estadd beta
est sto e7

reg casp volCha##c.pen inc  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age age2  yedu ngrchild  mobility disabled health i.country [pw=cciw_w6], beta robust  //thexp
estadd beta
est sto e8

/* margins volCha, at(hnetw=(0(200)1000)) */
/* marginsplot, x(hnetw)  plot2opts(lpattern(dot))title(" ")legend(off)saving(d8.gph,replace)ysc(range(-.1(.1).3))ylab(-.1(.1).3)ytit("predicted CASP") */
/* //dy */


estout e1 e2 e3 e4 e5 e6 e7 e8 using `tmp'regEw6.tex,style(tex)  cells(b(star fmt(%9.4f))) replace  collabels(, none) stats(N, labels("N")fmt(%9.0f))drop(*country*)varlabels(_cons constant) label starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001)
//yay so actually stronger effect than in w4 :) so no more 2/3 of effect if anything stronger on volunteering!!

! sed -i '/^no vol/d' `tmp'regEw6.tex

! sed -i '1 i\&\\multicolumn{4}{c}{Life satisfaction}&\\multicolumn{4}{c}{CASP}\\\\' `tmp'regEw6.tex
//! sed -i '/^constant/i\country dummies&yes&yes&yes&yes&yes&yes&yes&yes\\\\' `tmp'regEw6.tex
/* ! sed -i '/^volChaOft==less often/i\\{voluntary or charity work:}&&&&&&&&\\\\' `tmp'regDw6.tex */
/* ! sed -i 's/volChaOft==/\\hspace{.25in}/g' `tmp'regDw6.tex */




*----------------------------GIT CP-----------------------
mkdir leszekGit
cp dofiles tex, not n org!!
git push https://github.com/theaok/socTraSocCap
