* TEN UŻYWAJ
* Dodałem zmienne z imputations pozwalające na dodatkową selekcję próby do regresji.
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
* stata // LM
clear                                  
capture set maxvar 10000
version 14                             
set more off                           
* run ~/papers/root/do/aok_programs.do
do "C:\projekty\NCN_AdamOK\wyniki\github\new_121217\aok_programs.do" // LM

* loc d = "/home/aok/misc/grants/poland/leszekMorawskiVistula/"       
loc d = "C:\projekty\NCN_AdamOK\wyniki\AOK1\"
capture mkdir "`d'scr"
capture mkdir "/tmp/shareElderly"
loc pap shareElderly
loc tmp "`d'/tmp//`pap'/"
di "`tmp'"

//file open tex using `d'out/tex, write text replace
//file write tex `"%<*ginipov>`:di  %9.2f `r(rho)''%</ginipov>"' 
cap file close f // LM: dodałem bo się zawieszało przy powtórnej próbie uruchomienia

file open f using `d'notes.txt, write replace
file write f  "a note...." _n _n

/*
! time stata -b do  /home/aok/misc/grants/poland/leszekMorawskiVistula/creW6.do  &
! time stata -b do  /home/aok/misc/grants/poland/leszekMorawskiVistula/creW4.do  &
*/
use `tmp'allW6,clear

** weights:  see per weights in data/share n.org
//!!just use pwesight as per share email


**** descriptive stats


sum ub sa disab,det //phmmm very small!!
foreach v of varlist ub sa disab{
count if `v'==0
}


codebook swb, ta(100)
codebook ghto

corr swb swbAC 

tabstat ghto volCha, by(cs) format(%9.2f) //good checks out!! like what we know

//svy: reg swb ghto
reg swb ghto, robust cluster(country)

bys cs: reg swb ghto, robust 

ta emp
sum volCha* ghto
pwcorr volCha* ghto
sum ypen*

d y*

//ghto rhfo

loc x swb casp volCha volChaO ac035d4 ac035d5 ac035d7 ac035d8 ac035d9 ac035d10 ypen1 ypen2 yreg1 pen disab ub sa labInc hnetw yedu age male  health mar emp nchild
sum `x'

cap file close f

/* LM - temporary
aok_var_des , ff(swb casp)fname(`tmp'var_des-1.tex)
aok_var_des , ff(volCha volChaO ac035d4 ac035d5 ac035d7 ac035d8 ac035d9 ac035d10)fname(`tmp'var_des-2.tex)
aok_var_des , ff(ypen1 ypen2 yreg1 pen disab ub sa)fname(`tmp'var_des-3.tex)
aok_var_des , ff(labInc hnetw yedu age male  health mar emp nchild)fname(`tmp'var_des-4.tex)




! sed -i '/^  voluntary or charity work/i\\\hline {\\bf social capital:}&\\\\' `tmp'var_des.tex
! sed -i '/^  annual old age/i\\\hline {\\bf social transfers:}&\\\\' `tmp'var_des.tex
! sed -i '/^  labor income/i\\\hline {\\bf control variables:}&\\\\' `tmp'var_des.tex

 

aok_hist2,x(swb casp)d(`tmp')f(hist-1)
aok_hist2,x(volCha volChaO ac035d4 ac035d5 ac035d7 ac035d8 ac035d9 ac035d10)d(`tmp')f(hist-2)
aok_hist2,x(ypen1 ypen2 yreg1 pen disab ub sa)d(`tmp')f(hist-3)
aok_hist2,x(labInc hnetw yedu age male  health mar emp nchild)d(`tmp')f(hist-4)

*/

**** regressions

*** first playing

*** quasi final regressions

** social transfers (TABLE 2. OLS of life satisfaction on pensions and other transfers inclusing income and wealth. Beta (fully standarized) coefficients reported.)

/*A:
1 income:
2 labor
3 replacement (instead of labor, emerytury, renty etc)
4 transfers 
*/

/*** LM (19.12.2017)  Zmienne z SHARE (MRPiPS) */

/* Zmienne kontrolne : */ 
	/* własność domu : perho (Percentage of house owned)  */	
						cap drop d_perho
						gen d_perho = perho
						recode d_perho (-99=0) (0/39.9=0) (40/100=1)
	/*      owner, tenant or rent free					*/
						su otrf

/* DEMOGRAFIA */
	/* miast0/wieś */  gen rural = ( areabldgi==5)   // mo: areabldgi w gv_housing. Zwykle 0/1 dzielimy kategorie np.: 
						  // 1: A rural area or village, 
						  // 0: A big city, The suburbs or outskirts of a big city, A large town,  A small town
/* ZDROWIE */
	/* fizyczna nieaktywność */ 	su phinact
	/* adl */ 						su adl 
	/* iadl */ 						su iadl
	/* uścisk */ 					su maxgrip
	/* mobility */					su mobility
	/* niepełnosprawność */ 		su eurod 
									// depr = (eurod>=4) // W gv_health są dwie zmienne: eurodcat - 0/1 czy ma depresję (dokładnie czy ma 4 lub więcej z 12 symptomów depresji)
										 // oraz eurod - zmienna ciągła od 0 do 12 - identyfikująca 12 symptomów depresji - im więcej tym gorzej
										 // polecam eurodcat	
	
/* SYTUACJA MATERIALNA I ZAWODOWA */
	/* mem=fdistress */ su fdistress // nie ma
			                         // mm: złożyć 1_mem I 2_mem w jedną zmienną (=1 jak 1_mem lub 2_mem ==1)?


*/


/* LM: SAMPLE SELECTION : chcemy mieć "volunteering flexible people" */
/* wstępnie : wiek, mobility [20% ma co najmniej 3 objawy - duża utrata], depresja (eurod) i iadl */
cap drop sample0 
gen sample0=1
replace sample0 = 0 if (age>90) 		// na podstawie MO
replace sample0 = 0 if  mobility>=5 	// restrykcyjne ograniczenie - mobility>=3 wycina 17000 obs
replace sample0 = 0 if  eurod>=4 
replace sample0 = 0 if  iadl>1


//inc--guess would count tings again
reg swb labInc i.country [pw=cciw_w6], beta robust // LM: dodałem i.country aby było zgodnie z tekstem
estadd beta
est sto a1

/* LM : Model a1 */
reg swb labIncppp i.country [pw=cciw_w6], beta robust // LM: dodałem i.country aby było zgodnie z tekstem
	estadd beta
	est sto a1ppp_lm
	
	cap drop labIncppp01
	gen labIncppp01 = (labIncppp>0 & labIncppp~=.)

	cap drop int_labIncppp01
	gen int_labIncppp01=labIncppp01*labIncppp

	reg swb labIncppp01 int_labIncppp01 i.country [pw=cciw_w6] , beta robust // LM: dodałem fixed-effect dla pracujacych
	est sto a1pppInt_lm

	reg swb labIncppp01 int_labIncppp01 i.country [pw=cciw_w6] if sample0==1, beta robust // LM: dodałem fixed-effect dla pracujacych
	est sto a1pppIntS_lm


d pen ub sa disab			// LM: max(disab) = 2851620 (!!!) - outlier ?
corr pen ub sa disab 
sum pen ub sa disab
count
count if pen>0
count if ub>0
count if sa>0
count if disab>0

reg swb labInc  pen [pw=cciw_w6], beta robust
estadd beta  
est sto a2  //a2

/* LM: Model a2 */
	cap drop penppp01
	gen penppp01 = (penppp>0 & penppp~=.)

	cap drop int_penppp01
	gen int_penppp01=penppp01*penppp

	reg swb labIncppp  penppp [pw=cciw_w6], beta robust
	estadd beta  
	est sto a2ppp_lm  //a2

	reg swb labIncppp01 int_labIncppp01 penppp01 int_penppp01 i.country [pw=cciw_w6], beta robust
	estadd beta  
	est sto a2pppInt_lm  //a2

	reg swb labIncppp01 int_labIncppp01 penppp01 int_penppp01 i.country if sample0==1 [pw=cciw_w6], beta robust
	estadd beta  
	est sto a2pppIntS_lm  //a2



reg swb labInc pen ub sa disab [pw=cciw_w6], beta robust
estadd beta
est sto a3 //a3

//neg even contrlling for hea and emp
//reg swb labInc inc pen ub sa disab hea emp, beta robust

/* LM: Model a3 */
	cap drop ubppp01
	gen ubppp01 = (ubppp>0 & ubppp~=.)

	cap drop int_ubppp01
	gen int_ubppp01=ubppp01*ubppp

	cap drop sappp01
	gen sappp01 = (sappp>0 & sappp~=.)

	cap drop int_sappp01
	gen int_sappp01=sappp01*sappp

	cap drop disabppp01
	gen disabppp01 = (disabppp>0 & disabppp~=.)

	cap drop int_disabppp01
	gen int_disabppp01=disabppp01*disabppp

	reg swb labIncppp01 int_labIncppp01 penppp01 int_penppp01 ubppp01 int_ubppp01 sappp01 int_sappp01 disabppp01 int_disabppp01 ///
	     i.country if sample0==1 [pw=cciw_w6], beta robust
	estadd beta  
	est sto a3pppIntS_lm  //a2

	
	
//liab otrf--guess already incl in hnetw
reg swb labInc  pen ub sa disab    hnetw [pw=cciw_w6], beta robust //missing for many perho
estadd beta
est sto a4 //a4



reg swb labInc pen ub sa disab   hnetw male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust //missing for many perho
estadd beta
est sto a5 //a5

/* LM: Model a5 - pełny wypas */
gen age2 = age*age/100

reg swb labIncppp01 int_labIncppp01 penppp01 int_penppp01 ubppp01 int_ubppp01 sappp01 int_sappp01 disabppp01 int_disabppp01 ///  
hnetw male mar emp age age2 yedu nchild health i.country  if sample0==1 [pw=cciw_w6], beta robust //missing for many perho
estadd beta
est sto a5pppIntS_lm //a5


estout a* ,style(tab)  cells(beta(star fmt(%9.2f))) replace  collabels(, none) stats(N, labels("N")fmt(%9.0f))

estout a* using `tmp'regAw6.tex,style(tex)  cells(beta(star fmt(%9.2f))) replace  collabels(, none) stats(N, labels("N")fmt(%9.0f))drop(*country*)varlabels(_cons constant) label starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001)
//in w6 pensions negative and only later flip to positive--explanation--collinearity--they pick up from other bad stuff like bad health etc!! and once control for these all good then  
! sed -i '/^constant/i\country dummies&no&no&no&no&yes\\\\' `tmp'regAw6.tex



** social capital (Table 3: OLS of life satisfaction on volunteerring and pensions. Beta (fully standarized) coefficients reported.)


/*B:
1 volunteering only
2 soc cap ac vars
3 income
4 other controls
*/
exit 

//no ac035d6 in w6!!

reg swb VCO2-VCO5 [pw=cciw_w6], robust beta
estadd beta
est sto b1

/* LM */
reg swb VCO2-VCO5 [pw=cciw_w6] if sample0==1, robust beta
estadd beta
est sto b1S_lm


reg swb VCO2-VCO5 ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 [pw=cciw_w6], robust beta 
estadd beta
est sto b2 // b2

d ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10

/* LM : współliniowość między wolontariatem a aktywnościami */
	reg swb VCO2-VCO5 ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 [pw=cciw_w6] if sample0==1, robust beta
	estadd beta
	est sto b2S_lm

	corr VCO2-VCO5 ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 if sample0==1
	/* LM - 77% osób w próbie czyta książki, więc to mało dyskryminuje */
	
	cap drop activity
	egen activity = anymatch(ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10), values(1)
	for num 2/5: tab activity VCOX if sample0==1, row  // widać powiązanie aktywność-wolontariat. Jak się wyżuci "czytanie książek" to jest to jeszcze wyraźniejsze.

reg swb VCO2-VCO5 ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 emp  [pw=cciw_w6], robust beta
estadd beta
est sto b3 // b3

reg swb VCO2-VCO5 ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 emp  [pw=cciw_w6], robust beta
estadd beta
est sto b3 // b3 (po co powtórzone ?)

/* LM */
reg swb VCO2-VCO5 ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 emp [pw=cciw_w6] if sample0==1, robust beta
estadd beta
est sto b3S_lm


reg swb VCO2-VCO5 ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust 
estadd beta
est sto b4 // b4

/* LM */
reg swb VCO2-VCO5 ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age age2 yedu nchild health i.country  if sample0==1 [pw=cciw_w6], beta robust 
estadd beta
est sto b4S_lm // b4


estout b* ,style(tab)  cells(beta(star fmt(%9.2f))) replace  collabels(, none) stats(N, labels("N")fmt(%9.0f))

estout b* using `tmp'regBw6.tex,style(tex)  cells(beta(star fmt(%9.2f))) replace  collabels(, none) stats(N, labels("N")fmt(%9.0f))drop(*country*)varlabels(_cons constant) label starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001)

! sed -i '/^constant/i\country dummies&no&no&no&yes\\\\' `tmp'regBw6.tex
! sed -i '/^volChaOft==less often/i\\{voluntary or charity work:}&&&&\\\\' `tmp'regBw6.tex
! sed -i 's/volChaOft==/\\hspace{.25in}/g' `tmp'regBw6.tex



**C: substytucja (Table 4: OLS of SWB (life satisfaction and CASP) on volunteerring and pensions. Beta (fully standarized) coefficients reported.)



reg swb VCO2-VCO5 pen  [pw=cciw_w6], robust beta
estadd beta
est sto c1 //c1w6


reg swb VCO2-VCO5 pen ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab   hnetw male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  
estadd beta
est sto c2 //c2w6

corr swb casp

reg casp VCO2-VCO5 pen  [pw=cciw_w6], robust beta
estadd beta
est sto c3 //c3w6


reg casp VCO2-VCO5 pen ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab   hnetw male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  
estadd beta
est sto c4 //c4w6

/* LM */
reg casp VCO2-VCO5 ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 ///
         labIncppp01 int_labIncppp01 penppp01 int_penppp01 ubppp01 int_ubppp01 sappp01 int_sappp01 disabppp01 int_disabppp01  ///
		 hnetw male mar emp age age2 yedu nchild health i.country if sample0==1 [pw=cciw_w6], beta robust  
estadd beta
est sto c4pppIntS_lm //c4w6

/* LM - wersja bez aktywności - ale przy CASP to nie ma znaczenia, można zostawić aktywności bo wolontariat pozostaje istotny. */
reg casp VCO2-VCO5  ///
         labIncppp01 int_labIncppp01 penppp01 int_penppp01 ubppp01 int_ubppp01 sappp01 int_sappp01 disabppp01 int_disabppp01  ///
		 hnetw male mar emp age age2 yedu nchild health i.country if sample0==1 [pw=cciw_w6], beta robust  
estadd beta
est sto c4pppIntS1_lm //c4w6

estout c* ,style(tab)  cells(beta(star fmt(%9.3f))) replace  collabels(, none) stats(N, labels("N")fmt(%9.0f))

estout c* using `tmp'regCw6.tex,style(tex)  cells(beta(star fmt(%9.2f))) replace  collabels(, none) stats(N, labels("N")fmt(%9.0f))drop(*country*)varlabels(_cons constant) label starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001)
//yay so actually stronger effect than in w4 :) so no more 2/3 of effect if anything stronger on volunteering!!




** copmbine both tables



reg swb VCO2-VCO5 pen  [pw=cciw_w6], robust beta
estadd beta
est sto c1W6 // identyczne jak est sto c1 (po co ?)


reg swb VCO2-VCO5 pen ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab hnetw male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  
estadd beta
est sto c2W6 // jw

corr swb casp

reg casp VCO2-VCO5 pen  [pw=cciw_w6], robust beta
estadd beta
est sto c3W6 // jw


reg casp VCO2-VCO5 pen ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab   hnetw male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  
estadd beta
est sto c4W6 // jw


//! time stata -b do  /home/aok/misc/grants/poland/leszekMorawskiVistula/creW4.do
use `tmp'allW4,clear


reg swb VCO2-VCO5 pen  [pw=cciw_w4], robust beta
estadd beta
est sto c1W4


reg swb VCO2-VCO5 pen ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab   hnetw male mar emp age yedu nchild health i.country [pw=cciw_w4], beta robust  
estadd beta
est sto c2W4

corr swb casp

reg casp VCO2-VCO5 pen  [pw=cciw_w4], robust beta
estadd beta
est sto c3W4


reg casp VCO2-VCO5 pen ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab   hnetw male mar emp age yedu nchild health i.country [pw=cciw_w4], beta robust  
estadd beta
est sto c4W4

estout c1W6 c1W4 c2W6 c2W4 c3W6 c3W4 c4W6 c4W4 using `tmp'regCw6w4.tex,style(tex)  cells(beta(star fmt(%9.2f))) replace  collabels(, none) stats(N, labels("N")fmt(%9.0f))drop(*country*)varlabels(_cons constant) label starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001)

! sed -i '1 i\&\\multicolumn{4}{c}{Life satisfaction}&\\multicolumn{4}{c}{CASP}\\\\' `tmp'regCw6w4.tex
! sed -i '/^constant/i\country dummies&no&no&yes&yes&no&no&yes&yes\\\\' `tmp'regCw6w4.tex
! sed -i '/^volChaOft==less often/i\\{voluntary or charity work:}&&&&&&&&\\\\' `tmp'regCw6w4.tex
! sed -i 's/volChaOft==/\\hspace{.25in}/g' `tmp'regCw6w4.tex



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



reg swb labInc i.penQ [pw=cciw_w6], beta robust
reg swb labInc i.penQ ub sa disab    hnetw male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust 

reg swb labInc pen pen2 [pw=cciw_w6], beta robust
di (_b[pen]/(-2* _b[pen2])) //*1000000
//so flips like at 120k


reg swb labInc pen pen2 ub sa disab    hnetw male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust 
di (_b[pen]/(-2* _b[pen2])) //*1000000


reg swb labInc pen i.penC [pw=cciw_w6], beta robust
* reg swb labInc i.penCAT ub sa disab  male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust // variable penCAT not found
  reg swb labInc i.penC ub sa disab  male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust 


reg swb labInc PC2-PC4 ub sa disab  hnetw male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust 

reg casp VCO2-VCO5 PC2-PC4 ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab   hnetw male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  


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
reg casp VCO2-VCO5 PC2-PC4 i.incQ  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab  hnetw male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  





//----------------------------------------------------------------------

*** for whom? by income! :)


** playing
/*
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
*/

reg casp  i.volCha##c.pen ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab   hnetw male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  
//yeah, so need some money to be able to volunteer!


//!! TOPAPER: and say tried nonliner but pretty linear the higher $ the less vol useful!
reg casp  volCha##c.inc thexp ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10    hnetw male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  
//interesting!!
margins volCha, at(inc=(0(20000)100000))
marginsplot, x(inc)  //plot1opts(lpattern(dot))title(" ")legend(off)
* dy // patrz aok_programs.do line: 44


//say that same for theinc2
reg casp  i.volCha##c.thinc2  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  hnetw male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  

//!! TOPAPER and say tried nonliner but pretty linear the higher $ the less vol useful!
reg casp  volCha##c.hnetw ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 inc thexp male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  
//interesting!!
margins volCha, at(hnetw=(0(200000)1000000))
marginsplot, x(hnetw)  //plot1opts(lpattern(dot))title(" ")legend(off)
* dy


reg casp VCO2-VCO5 pen ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 labInc ub sa disab   hnetw male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  



reg casp  i.volCha##i.penC ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab   hnetw male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  

reg casp  i.volCha##i.incD ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab   hnetw male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  

reg casp  i.volCha##i.incD ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 pen ub sa disab   hnetw male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  


xtile hnetwQ = hnetw, nq(5)
_pctile hnetw, nq(5)
return list

reg casp  i.volCha##hnetwQ ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab   hnetw male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  


xtile hnetwD = hnetw, nq(10)
_pctile hnetw, nq(10)
return list

reg casp  i.volCha##hnetwD ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab    male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  


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

// LM: Skąd te scenariusze ? Jak ustalone tresholds: 500, 1000, 5000, 10000, ... ?
// LM: Dla "rich: here those that volunteer a lot are happy!! others less!" 
// su hnetw if casp~=. : max value of hnetw=30059.09

//super poor
reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age yedu nchild health i.country [pw=cciw_w6] if inc<500 & hnetw < 2500 & pen<1000, beta robust  

//poor
reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age yedu nchild health i.country [pw=cciw_w6] if inc<1000 & hnetw < 5000 & pen<1000, beta robust  

//medium
reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age yedu nchild health i.country [pw=cciw_w6] if (inc<5000 & hnetw < 25000 & pen<5000)&((hnetw > 5000 |inc>5000 | pen>5000)& liab<10000), beta robust  

reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age yedu nchild health i.country [pw=cciw_w6] if (inc<10000 & hnetw < 50000 & pen<10000)&((hnetw > 10000 |inc>5000 | pen>5000)& liab<10000), beta robust  

/* LM
	//rich: here those that volunteer a lot are happy!! others less!
	 reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age yedu nchild health i.country [pw=cciw_w6] if ((hnetw > 50000 &(inc>10000 | pen>5000)& liab<10000)), beta robust 

	//very rich
	 reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age yedu nchild health i.country [pw=cciw_w6] if ((hnetw > 100000 &(inc>20000 | pen>10000)& liab<10000)), beta robust 


	//super rich
	 reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age yedu nchild health i.country [pw=cciw_w6] if ((hnetw > 250000 &(inc>20000 | pen>10000)& liab<10000)), beta robust 
*/

//------
//!! MAYBE TOPAPER: assume 2 scenarios:
//bottom line: the rich benefit 2x less from volunteering than middle class and poor!

/* LM
	//rich
	 reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age yedu nchild health i.country [pw=cciw_w6] if ((hnetw > 300000 & liab<10000))|(inc>100000&thexp<20000), beta robust 

	//middle class only
	reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age yedu nchild health i.country [pw=cciw_w6] if (inc<10000 & hnetw < 50000 & pen<10000)&((hnetw > 10000 |inc>5000 | pen>5000)& liab<10000), beta robust  

	//non-rich
	 reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age yedu nchild health i.country [pw=cciw_w6] if ((hnetw < 100000))&(inc<50000), beta robust 

	 reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age yedu nchild health i.country [pw=cciw_w6] if ((hnetw < 50000)), beta robust 


	//middle class 
	reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age yedu nchild health i.country [pw=cciw_w6] if (inc<50000 & hnetw < 100000)&(inc>1000 & hnetw > 5000), beta robust  

	reg casp  VCO2-VCO5  ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10  ub sa disab  male mar emp age yedu nchild health i.country [pw=cciw_w6] if (inc<50000 & hnetw < 100000), beta robust  
*/

** PAPER (Table 5: OLS of SWB (life satisfaction and CASP) on volunteerring and pensions. Unstandarized coefficients reported.)


use `tmp'allW6,clear 

reg swb volCha##c.inc i.country [pw=cciw_w6], robust beta
estadd beta
est sto d1  // d1

reg swb volCha##c.inc thexp hnetw ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  
estadd beta
est sto d2 // d2

margins volCha, at(inc=(0(20)100))
marginsplot, x(inc)  plot2opts(lpattern(dot))title(" ")legend(off)saving(d2.gph,replace)ysc(range(7.4(.2)8))ylab(7.4(.2)8)ytit("predicted life satisfaction")
//dy


reg swb volCha##c.hnetw i.country [pw=cciw_w6], robust beta
estadd beta
est sto d3 // d3

reg swb volCha##c.hnetw inc thexp ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  
estadd beta
est sto d4 // d4



margins volCha, at(hnetw=(0(200)1000))
marginsplot, x(hnetw)  plot2opts(lpattern(dot))title(" ")legend(off)saving(d4.gph,replace)ysc(range(7.4(.2)8))ylab(7.4(.2)8)ytit("predicted life satisfaction")
//dy




reg casp volCha##c.inc i.country [pw=cciw_w6], robust beta
estadd beta
est sto d5 // d5

reg casp volCha##c.inc thexp hnetw ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  
estadd beta
est sto d6 // d6

margins volCha, at(inc=(0(20)100))
marginsplot, x(inc)  plot2opts(lpattern(dot))title(" ")legend(off)saving(d6.gph,replace)ysc(range(-.1(.1).3))ylab(-.1(.1).3)ytit("predicted CASP")
//dy


reg casp volCha##c.hnetw i.country [pw=cciw_w6], robust beta
estadd beta
est sto d7 // d7

reg casp volCha##c.hnetw inc thexp ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  
estadd beta
est sto d8 // d8

margins volCha, at(hnetw=(0(200)1500))
marginsplot, x(hnetw)  plot2opts(lpattern(dot))title(" ")legend(off)saving(d8.gph,replace)ysc(range(-.1(.1).3))ylab(-.1(.1).3)ytit("predicted CASP")
//dy


/* LM : bogactwo & emerytura - próba - tak jak poprzednio + tylko emeryci */
cap eret clear
		reg casp i.volChaOft##c.hnetw  int_penppp01 /// // VCO2##c.hnetw VCO3##c.hnetw VCO4##c.hnetw VCO5##c.hnetw
				ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 ///
				male mar  age age2 yedu nchild health i.country [pw=cciw_w6] if sample0==1 & penppp01==1 , beta robust  // TYLKO EMERYCI 
		estadd beta
		est sto d8S_lm // d4

		*     emp  thexp  inc   labIncppp01 int_labIncppp01 penppp01 int_penppp01 ubppp01 int_ubppp01 sappp01 int_sappp01 disabppp01 int_disabppp01  ///
		*     VCO2-VCO5
		*     volCha##c.hnetw

		margins volChaOft, at(hnetw=(0(200)1500))   // warunek "if" sprawia, że porównujemy VCO`n' z tymi niebędącymi w wolontariacie
		marginsplot, x(hnetw)  plot2opts(lpattern(dot))title(" ")saving(d8lm.gph,replace)ysc(range(-.1(.1).6))ylab(-.1(.1).6)ytit("predicted CASP")
		

		
/* LM : bogactwo & emerytura - próba - tak jak poprzednio + tylko emeryci */
cap eret clear
		reg casp i.volChaOft##c.penppp  /// // VCO2##c.hnetw VCO3##c.hnetw VCO4##c.hnetw VCO5##c.hnetw
				ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 ///
				male mar  age age2 yedu nchild health i.country [pw=cciw_w6] if sample0==1 & penppp01==1 , beta robust  // TYLKO EMERYCI  
		estadd beta
		est sto d8S_lm // d4

		*     emp  thexp  inc   labIncppp01 int_labIncppp01 penppp01 int_penppp01 ubppp01 int_ubppp01 sappp01 int_sappp01 disabppp01 int_disabppp01  ///
		*     VCO2-VCO5
		*     volCha##c.hnetw

		margins volChaOft, at(penppp=(0(1000)15000))   // warunek "if" sprawia, że porównujemy VCO`n' z tymi niebędącymi w wolontariacie
		marginsplot, x(penppp)  plot2opts(lpattern(dot))title(" ")saving(d8lm.gph,replace)ysc(range(-.1(.1).6))ylab(-.1(.1).6)ytit("predicted CASP")
		
		
		
		
		
		
/* Bogactwo (net worth) a wpływ wolontariatu na CASO */		
		foreach n of numlist 2(1)5 {
		margins VCO`n' if (VCO`n'==1 | volCha==0), at(hnetw=(0(1000)15000))   // warunek "if" sprawia, że porównujemy VCO`n' z tymi niebędącymi w wolontariacie
		marginsplot, x(hnetw)  plot2opts(lpattern(dot))title(" ")legend(off)saving(d8lm`n'.gph,replace)ysc(range(-.1(.1).6))ylab(-.1(.1).6)ytit("predicted CASP")
		}

		/* LM : lepszy rysunek - jeden wykres z linią dla "no volunteering", "less often", ...., "almost every day" *
				można skorzystać z macierzy e(b) zachowanej po margins - skomplikowane, ale wykonalne. Da się prościej ? 
				Uwaga ! "post" zapisuje wyniki do e(), ale nowe margins wymaga wyczyszczenia modelu (eret clear) i nowego oszacowania. 
				Inaczej "margins cannot work with its own posted results" */
		gr combine d8lm2.gph d8lm3.gph  d8lm4.gph d8lm5.gph // LM: Do refleksji

		
/* Emerytura a wpływ wolontariatu na CASO */		
		foreach n of numlist 2(1)5 {
		margins VCO`n' if (VCO`n'==1 | volCha==0), at(int_penppp01=(0(1000)15000))  // warunek "if" sprawia, że porównujemy VCO`n' z tymi niebędącymi w wolontariacie
		marginsplot, x(int_penppp01)  plot2opts(lpattern(dot))title(" ")legend(off)saving(d8Penlm`n'.gph,replace)ysc(range(-.1(.1).6))ylab(-.1(.1).6)ytit("predicted CASP")
		}
		gr combine d8Penlm2.gph d8Penlm3.gph  d8Penlm4.gph d8Penlm5.gph // LM: Do refleksji

		
		
//TODO here and table e sed rm thiose lines with base case
estout d* using `tmp'regDw6.tex,style(tex)  cells(b(star fmt(%9.4f))) replace  collabels(, none) stats(N, labels("N")fmt(%9.0f))drop(*country*)varlabels(_cons constant) label starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001)
//yay so actually stronger effect than in w4 :) so no more 2/3 of effect if anything stronger on volunteering!!

! sed -i '1 i\&\\multicolumn{4}{c}{Life satisfaction}&\\multicolumn{4}{c}{CASP}\\\\' `tmp'regDw6.tex
! sed -i '/^constant/i\country dummies&yes&yes&yes&yes&yes&yes&yes&yes\\\\' `tmp'regDw6.tex
/* ! sed -i '/^volChaOft==less often/i\\{voluntary or charity work:}&&&&&&&&\\\\' `tmp'regDw6.tex */
/* ! sed -i 's/volChaOft==/\\hspace{.25in}/g' `tmp'regDw6.tex */


gr combine d2.gph d4.gph d6.gph d8.gph
// dy
gr export `tmp'regDmarg.pdf, replace

* if bigger for retired ppl: can check is effect of volunteering stronger among retired persons? and by pension


**TO APPENDIX THESE TWO:


reg swb volCha##emp i.country [pw=cciw_w6], robust beta
estadd beta
est sto e1

reg swb volCha##emp inc thexp hnetw ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar  age yedu nchild health i.country [pw=cciw_w6], beta robust  
estadd beta
est sto e2

reg swb volCha##c.pen i.country [pw=cciw_w6], robust beta
estadd beta
est sto e3

reg swb volCha##c.pen inc thexp ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  
estadd beta
est sto e4

/* margins volCha, at(hnetw=(0(200)1000)) */
/* marginsplot, x(hnetw)  plot2opts(lpattern(dot))title(" ")legend(off)saving(d4.gph,replace)ysc(range(7.4(.2)8))ylab(7.4(.2)8)ytit("predicted life satisfaction") */
/* //dy */


reg casp volCha##c.emp i.country [pw=cciw_w6], robust beta
estadd beta
est sto e5

reg casp volCha##c.emp thexp hnetw ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  
estadd beta
est sto e6

reg casp volCha##c.pen i.country [pw=cciw_w6], robust beta
estadd beta
est sto e7

reg casp volCha##c.pen inc thexp ac035d4 ac035d5  ac035d7 ac035d8 ac035d9 ac035d10 male mar emp age yedu nchild health i.country [pw=cciw_w6], beta robust  
estadd beta
est sto e8

/* margins volCha, at(hnetw=(0(200)1000)) */
/* marginsplot, x(hnetw)  plot2opts(lpattern(dot))title(" ")legend(off)saving(d8.gph,replace)ysc(range(-.1(.1).3))ylab(-.1(.1).3)ytit("predicted CASP") */
/* //dy */


estout e* using `tmp'regEw6.tex,style(tex)  cells(b(star fmt(%9.4f))) replace  collabels(, none) stats(N, labels("N")fmt(%9.0f))drop(*country*)varlabels(_cons constant) label starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001)
//yay so actually stronger effect than in w4 :) so no more 2/3 of effect if anything stronger on volunteering!!

! sed -i '1 i\&\\multicolumn{4}{c}{Life satisfaction}&\\multicolumn{4}{c}{CASP}\\\\' `tmp'regEw6.tex
! sed -i '/^constant/i\country dummies&yes&yes&yes&yes&yes&yes&yes&yes\\\\' `tmp'regEw6.tex
/* ! sed -i '/^volChaOft==less often/i\\{voluntary or charity work:}&&&&&&&&\\\\' `tmp'regDw6.tex */
/* ! sed -i 's/volChaOft==/\\hspace{.25in}/g' `tmp'regDw6.tex */



/* LM
*----------------------------GIT CP-----------------------
mkdir leszekGit
cp dofiles tex, not n org!!
git push https://github.com/theaok/socTraSocCap
*/
