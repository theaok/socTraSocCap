//new: time /usr/local/stata13_ru/stata-mp -b do  /home/aok/misc/grants/poland/leszekMorawskiVistula/cre.do

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


**** dat_man

/*
use ~/data/share_Survey-Health-Ageing-Retirement-Europe/w4/sharew4_rel5-0-0_ep.dta 
d ep110* ep097* ep098*
sum ep110* ep097* ep098*
d ep071* //for everyone :)
sum ep324*

use  ~/data/share_Survey-Health-Ageing-Retirement-Europe/w4/sharew4_rel5-0-0_hh.dta ,clear

sum hh011*

there is imputed money values for
 old age income
and disability unemployment

use  ~/data/share_Survey-Health-Ageing-Retirement-Europe/w4/sharew4_rel5-0-0_ac.dta ,clear
lookfor volun
*/


**2017: release6.0:  4, 6 and coverscreens

//using imputed, per leszek everybody is using imputed essp for income
cd ~/data/share_Survey-Health-Ageing-Retirement-Europe
cd sharew4
! unzip sharew4_rel6-0-0_ALL_datasets_stata*
//just do imputed one--more obs on every var i guess! and then add from modules what missing
/* use sharew4_rel6-0-0_ac.dta,clear */
/* ta ac012 */
use sharew4_rel6-0-0_gv_imputations, clear


** technical vars

d mergeidp coupleid4 implicat htype fam_resp  

** get rid of duplicates, implicats

loc x y* liab  hnetw nchild perho otrf  thexp gali em* ms* sphus gender thinc thinc2 lifesat lifehap*  implicat htype fam_resp age ghto* pppx* country

keep `x' mergei* currency coupleid4

foreach v of var * {
local l`v' : variable label `v'
       if `"`l`v''"' == "" {
local l`v' "`v'"
 }
 }

collapse `x' (first) currency (first) coupleid4, by(mergeid)
/* keep if implicat ==1 //every guy multiplied by 5 to impute differnt 5 values */

foreach v of var * {
label var `v' "`l`v''"
 }

keep if age>=50



** key vars

sum lifesat //ac012
ta lifehap_f

ren lifesat swb
la var swb "swb"

note swb: "On a scale from 0 to 10 where 0 means completely dissatisfied and 10 means completely satisfied, how satisfied are you with your life?" [imputed]



lookfor income
corr thinc thinc2 //!! leszek: which one
ren thinc inc

la var inc "total hh income"
note inc: "Total household net income - version A" [imputed]

recode gender (2=0), gen(male)

la var age "age"
note age: Age of respondent (based on interview year)  "In which month and @byear@b were you born?" [imputed]
la var male "male"
note male: OBSERVATION Note sex of respondent from observation (ask if unsure)


ren sphus health
revrs health, replace
la var health "self reported health"
note health: "Would you say your health is..." "Poor"..."Excellent" [imputed]

recode mstat (1=1)(nonm=0),gen(mar)
la var mar "married and living together"
note mar: "What is your marital status?"  [imputed]

//gen age2=age^2

lookfor employ
//TODO check with codebook, assuming that na is not employed! or get from other modules
codebook empstat
recode empstat (-99=0)(nonm=1), gen(emp)
replace emp=. if emp!=0 & emp!=1
la var emp "employed"
note emp: The following questions are about your current main job. "In this job were you a private-sector employee, a public sector employee or self-employed?"  [imputed]


//TODO do notes from codebook


** soc transfers

d ylsp* ypen*
sum ylsp* ypen*

d ylsp1 //ok so actual payments are here :_)
sum ylsp1
ta ylsp1 in 1/100
count if ylsp1==0 //but for very few obs :(

ta ypen1 in 1/1000
count if ypen1==0
count if ypen1<. //ok this one is better

* *f are flags
ta ylsp1_f,mi
ta ypen1_f,mi
ta ylsp1_f ypen1_f

//TODO: policzyc udzial swiadczen w dochodzie osoby
//there is purchasing power partity
//TODO: get quintiles or quadratics
//lm: thre is var made by share that is total income, and will see how big share of thta is social transfers
//control for wages; look at empstat

** volunteering --none; merge from AC


lookfor vol
d ghto_f ghto //sp008,11,13
//flag says that like 99perc non misssing

//NOT USING--NOT SURE ABOUT CODING! just use sp008 from sp
/* la var ghto "personal helping" //helping others with personal stuff */
/* //within one's socila netweork; not very braod */
/* replace ghto=. if ghto<0 */

/* note ghto:  "Now I would like to ask you about the help you have @bgiven@b to others. Please look at card 27. In the last twelve months, have you @bpersonally@b given any kind of help listed on this card to a family member from outside the household, a friend or neighbour?" */

/* "In the last twelve months, how often altogether have you given such help to */
/* this person? Was it..." "1. About daily" to "3. About every month" */

/* 1. About daily */
/* 2. About every week */
/* 3. About every month */
/* 4. Less often */

//remember to control for other social capital like church, grandkids etc
lookfor rel child kid frien

//SHARE_generic_wave4_main_questionnaire.pdf sp008

//sp008:Now I would like to ask you about the help you have @bgiven@b to others. In the last twelve months, have you personally given personal care or practical household help to a family member living outside your household, a friend or neighbour?

d ydip thexp gali 
sum ydip thexp gali 
corr ydip yind 

//can also look at model as (assest)

//this is according to leszek!;; see release guide per gv_exrates
//leszek: if euro then multiply by nominal and divide by ppp as in guide
//a see http://data.worldbank.org/indicator/PA.NUS.PPP
ta currency
foreach v of varlist inc ydip  yind ylsp* ypen* yreg1 liab  hnetw{
replace `v'=`v'/pppx2011 //assume ppp for 10 and 12 same as 11
la var `v'  `"`:var lab `v'' PPP"' 
}

foreach v of varlist perho otrf{
replace `v'=. if `v'<0
}

d perho otrf
la var otrf "owner, tenant or rent free"
note otrf: "Please look at card 31. Is your household occupying the dwelling you live in as"[imputed]

la var perho "percentage of house owned"
note  perho: calculated variable--see Release Guide 6.0.0 [imputed] //no clue how!

d nchild
la var nchild "number of children"
note  nchild: "Now I will ask some questions about your children. How many children do you have that are still alive? Please count all natural children, fostered, adopted and stepchildren [ , including those of/ , including those of/ , including those of/ , including those of] [ your husband/ your wife/ your partner/ your partner] [ {Name of partner/spouse}]." [imputed]

//NOTE: not using these lump sums like ylsp*; these are just like one time

//lm: traditional way to group them in income analysis eg as in euromod--model podatkowo dochodowy dla europy TODO: use these in regressions!

d liab  hnetw
la var liab "financial liabilities"
note liab: "Not including mortgages or money owed on land, property or firms, how much do you [ and/ and/ and/ and] [your/ your/ your/ your] [ husband/ wife/ partner/ partner] owe in total?"[imputed]

la var hnetw "household net worth" //no label :(
note hnetw: calculated variable--see Release Guide 6.0.0 [imputed] //no clue how!


/* Only permanent sources */
gen labInc = ydip + yind
la var labInc "labor income"

note labInc: "After any taxes and contributions, what was your approximate annual income from employment in the year [STR (Year - 1)]? Please include any additional or extra or lump sum payment, such as bonuses, 13 month, Christmas or Summer pays." AND "After any taxes and contributions and after paying for any materials, equipment or goods that you use in your work, what was your approximate annual income from self-employment in the year [STR (Year - 1)]?" [imputed]

la var ypen1 "annual old age, early retirement pensions, survivor and war pension"
note ypen1: EP078\_1-2-3-7-8-9 (1-2-3-9-10-11 in w6)  "After taxes, about how large was a typical payment of [ your public old age pension/ your public old age supplementary pension or public old age second pension/ your public early retirement or pre-retirement pension/ your main public sickness benefits/ your main public disability insurance pension/ your secondary public disability insurance pension/ your Secondary public sickness benefits/ your public unemployment benefit or insurance/ your main public survivor pension from your spouse or partner/ your secondary public survivor pension from your spouse or partner/ your public war pension/ your public long-term care insurance/ your social assistance] in [STR (Year - 1)]?" [imputed]

la var ypen2 "annual private occupational pensions"
note ypen2: "After taxes, what was the approximate annual amount received from all your occupational pensions in [STR (Year - 1)]?" [imputed]

la var yreg1 "other regular payments from private pernsions"
note yreg1: "After any taxes and contributions, about how large was the average payment of [ you life insurance payments from a private insurance company/ your private annuity or private personal pension payments/ your alimony/ your regular payments from charities/ your long-term care insurance payments] in [STR (Year - 1)]?" [imputed]

gen pen = ypen1 + ypen2 + yreg1
la var pen "pension"
note pen: EP078\_1-2-3-7-8-9 (1-2-3-9-10-11 in w6)  `: char ypen1[note1]' AND `: char ypen2[note1]' AND `: char yreg1[note1]'  [imputed]

//d ypen3  ypen6  ypen36 //in wave4 just ypen36p in wave6: ypen36=ypen3+ypen6
gen disab  =  ypen36
la var disab "disability benefits"
note disab: EP078\_5-6  [from question in "annual old age, early retirement pensions, survivor and war pension"] [imputed]
gen ub = ypen4
la var ub "unemployment benefits"
note ub: EP078_6 (8 in w6) [from question in "annual old age, early retirement pensions, survivor and war pension"] [imputed]

gen sa = ypen5  
la var sa "social assistance" 
note sa: EP078\_10 (12-13 in w6) [from question in "annual old age, early retirement pensions, survivor and war pension"] [imputed]


d yedu
la var yedu "years of education"
note yedu: "How many years have you been in full-time education?" full-time education * includes: receiving tuition, engaging in practical work or supervised study or taking examinations * excludes: full-time working, home schooling, distance learning, special on-the-job training, evening classes, part-time private vocational training, flexible or part-time higher education studies, etc  [imputed]


//TODO have transfers as perc of tot income definetly

loc x swb inc male health mar age  emp ylsp* ypen*   country implicat perho otrf liab  hnetw yedu nchild  labInc yreg1 pen ub sa disab
keep `x' mergeid currency


gen cs=""
replace cs="Austria" if country==            11 	 
replace cs="Germany" if country==	 		    12
replace cs="Sweden" if country==	 		    13
replace cs="Netherlands" if country==	 	    14
replace cs="Spain" if country==		 		    15
replace cs="Italy" if country==		 		    16
replace cs="France" if country==	 		    17
replace cs="Denmark" if country==	 		    18
replace cs="Switzerland" if country==	 	    20
replace cs="Belgium" if country==	 		    23
replace cs="Czech Republic" if country==     28
replace cs="Poland" if country==	 		    29
replace cs="Hungary" if country==	 		    32
replace cs="Portugal" if country==	 		    33
replace cs="Slovenia" if country==	 		    34
replace cs="Estonia" if country==            35

gen iso=""
replace iso="AUT" if cs=="Austria"             	 
replace iso="DEU" if cs=="Germany"	 		    
replace iso="SWE" if cs=="Sweden"	 		    
replace iso="NLD" if cs=="Netherlands"	 	    
replace iso="ESP" if cs=="Spain"		 		    
replace iso="ITA" if cs=="Italy"		 		    
replace iso="FRA" if cs=="France"	 		    
replace iso="DNK" if cs=="Denmark"	 		    
replace iso="CHE" if cs=="Switzerland"	 	    
replace iso="BEL" if cs=="Belgium"	 		    
replace iso="CZE" if cs=="Czech Republic"     
replace iso="POL" if cs=="Poland"	 		    
replace iso="HUN" if cs=="Hungary"	 		    
replace iso="PRT" if cs=="Portugal"	 		    
replace iso="SVN" if cs=="Slovenia"	 		    
replace iso="EST" if cs=="Estonia"            

/*
kountry cs, from(other)m
ta MARKER
ta NAMES_STD cs
kountry cs, from(other) to(iso2c)
*/



save `tmp'imputedW4, replace


**** sp


use sharew4_rel6-0-0_sp.dta, clear
d sp008 sp011* sp013*
lookfor help // alot!

codebook sp008_
recode sp008_ (1=1)(5=0)(nonm=.), gen(ghto)
la var ghto "personal helping" //helping others with personal stuff */
/* //within one's socila netweork; not very braod */
note ghto: "In the last twelve months, have you @bpersonally@b given any kind of help listed on this card to a family member from outside the household, a friend or neighbour?" [sp]

codebook sp002
recode sp002_ (1=1)(5=0)(nonm=.), gen(rhfo)
la var rhfo "receiving personal help" //helping others with personal stuff */
/* //within one's socila netweork; not very braod */
note rhfo: "Please look at card 27. Thinking about the last twelve months, has any family member from outside the household, any friend or neighbour given you any kind of help listed on this card?" [sp]

keep mergeid ghto rhfo
save `tmp'spW4, replace



**** ac


use sharew4_rel6-0-0_ac.dta, clear


sum ac014_ ac015_ ac016_ ac017_ ac018_ ac019_ ac020_ ac021_ ac022_ ac023_ ac024_ ac025_ 
codebook ac014_ ac015_ ac016_ ac017_ ac018_ ac019_ ac020_ ac021_ ac022_ ac023_ ac024_ ac025_ 

foreach v of varlist ac036_* ac014_ ac015_ ac016_ ac017_ ac018_ ac019_ ac020_ ac021_ ac022_ ac023_ ac024_ ac025_ {
replace `v'=. if `v'<0
revrs `v', replace
}

set linesize 120

alpha ac014_ ac015_ ac016_ ac017_ ac018_ ac019_ ac020_ ac021_ ac022_ ac023_ ac024_ ac025_
factor ac014_ ac015_ ac016_ ac017_ ac018_ ac019_ ac020_ ac021_ ac022_ ac023_ ac024_ ac025_,fa(1) //bl(.4)
rotate, varimax
predict casp
la var casp "casp"
note casp: casp scale: see table \\ref{casp} [ac]

d ac014_ ac015_ ac016_ ac017_ ac018_ ac019_ ac020_ ac021_ ac022_ ac023_ ac024_ ac025_
set linesize 78


//in health module there is casp already!! but ranges 12-48, so they must have just added that!

/* ac014_          byte    %11.0g     often      Age prevents from doing things */
/* ac015_          byte    %11.0g     often      Out of control */
/* ac016_          byte    %11.0g     often      Feel left out of things */
/* ac017_          byte    %11.0g     often      Do the things you want to do */
/* ac018_          byte    %11.0g     often      Family responsibilities prevent */
/* ac019_          byte    %11.0g     often      Shortage of money stops */
/* ac020_          byte    %11.0g     often      Look forward to each day */
/* ac021_          byte    %11.0g     often      Life has meaning */
/* ac022_          byte    %11.0g     often      Look back on life with happiness */
/* ac023_          byte    %11.0g     often      Feel full of energy */
/* ac024_          byte    %11.0g     often      Full of opportunities */
/* ac025_          byte    %11.0g     often      Future looks good */

/* My age prevents me from doing the things I would like to\\ */
/* I feel that what happens to me is out of my control\\ */
/* I feel left out of things\\ */
/* I can do the things that I want to do\\ */
/* Family responsibilities prevent me from doing what I want to do\\ */
/* Shortage of money stops me from doing the things I want to do\\ */
/* I look forward to each day\\ */
/* I feel that my life has meaning\\ */
/* On balance, I look back on my life with a sense of happiness\\ */
/* I feel full of energy these days\\ */
/* I feel that life is full of opportunities\\ */
/* I feel that the future looks good for me\\\hline */


foreach v of varlist ac035d1 ac036_1 ac035d*{
replace `v'=. if `v'<0
}

codebook ac035d1
codebook ac036_1
replace ac036_1=. if ac036_1<0

//revrs ac036_1, replace
//replace ac036_1=0 if ac035d1==0
loc i 1
foreach i in 1 4 5 7 8 9 10{
d ac036_*
d ac035d*
di ">>> `i'"
replace ac036_`i'=0 if ac035d`i'==0
}

ta ac035dno

ren ac035d1 volCha
la var volCha "voluntary or charity work"
note volCha: "Please look at card 38: which of the activities listed on this card - if any - have you done in the past twelve months?" "Done voluntary or charity work" [ac]
replace volCha=. if volCha<0

recode ac036_1 (0=0 "none")(1=1 "less often")(2=2 "almost every month")(3=3 "almost every week")(4=4 "almost every day"),gen(volChaOft)

la var volChaOft "how often done voluntary or charity work"
note volChaOft: "How often in the past twelve months did you [do voluntary or charity work/cared for a sick or disabled adult/provided help to friends or neighbors/attended an educational or training course/go to a sport, social or other kind of club/taken part in the activities of a religious organization (church, synagogue, mosque etc.)/taken part in a political or community-related organization/read books, magazines or newspapers/do word or number games such as crossword puzzles or Sudoku/play cards or games such as chess]?" [ac]

ta volChaOft,gen(VCO)

//TODO essp for cross country paper other  forms of social capital suchas relationships with other people: family members, friends, neighbors, or other acquaintances

d ac035d*
corr  ac035d*

la var ac035d1  "done voluntary or charity work"
la var ac035d4  "attended an educational or training course"
la var ac035d5  "gone to a sport, social or other kind of club"
la var ac035d7  "taken part in a political or community-related organization"
la var ac035d8  "read books, magazines or newspapers"
la var ac035d9  "did word or number games (crossword puzzles/Sudoku...)"
la var ac035d10 "played cards or games such as chess"
la var ac035dno "none of these"

note  ac035d1: `: char volCha[note1]'  [ac]
note  ac035d4: `: char volCha[note1]'  [ac]
note  ac035d5: `: char volCha[note1]'  [ac]
note  ac035d7: `: char volCha[note1]'  [ac]
note  ac035d8: `: char volCha[note1]'  [ac]
note  ac035d9: `: char volCha[note1]'  [ac]
note ac035d10: `: char volCha[note1]'  [ac]
note ac035dno: `: char volCha[note1]'  [ac]


sum ac036_*


d ac021
codebook ac021
ren ac021 pil
la var pil "life has meaning"
ren ac012_ swbAC
la var swbAC "life satisfaction from AC modeule"
ta swbAC
replace swbAC=. if swbAC<0

keep mergeid volCha* casp ac035d*  swbAC pil VCO* //ac036_*
save `tmp'acW4, replace



**** technical vars

use sharew4_rel6-0-0_technical_variables.dta, clear
d
codebook mn024_
keep mergeid mn024_
save `tmp'techW4,replace

use sharew4_rel6-0-0_cf.dta,clear
keep mergeid cf010_
ta cf010_, mi
save `tmp'cfW4, replace

//TODO:
//excluding those with severe pscyhical amd mental illness--who cannot volunteer



**** merge with other datasets

use `tmp'imputedW4,clear
merge 1:1 mergeid using sharew4_rel6-0-0_gv_weights.dta
drop if _merge ==2
drop _merge

merge 1:1 mergeid using `tmp'techW4
drop if mn024==2
drop if _merge==2
drop _merge

merge 1:1 mergeid using `tmp'cfW4
drop if cf010_==. //drop proxies--na okroglo bo nie ma pytania o proxies-- a to pytanie jest tylko zadawane participants,not proxies per monika from sczecin from share
drop if _merge==2
drop _merge

merge 1:1 mergeid using `tmp'acW4
drop if _merge==2
drop _merge


merge 1:1 mergeid using `tmp'spW4
drop if _merge==2
drop _merge


foreach v of varlist volCha* casp ac035d*  swbAC pil swb inc male health mar age emp ylsp* ypen*   country implicat perho otrf liab  hnetw yedu nchild rhfo labInc yreg1 pen ub sa disab ac*{
codebook `v', ta(100)
}

save `tmp'allW4,replace




! rm *.dta *.pdf


