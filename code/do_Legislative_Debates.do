**The Politics of Legislative Debates**
**November 2019**
**jorge.fernandes@ics.ulisboa.pt**

*Please read carefully what follows:
*We strongly suggest that you run the do-file using Stata 16, not least because we use frames (equivalent to objects in R). This will make your life much easier. Note that you can, of course, run the figures using Stata 15, as long as you prepare the data matrix manually.

*Working Mac
capture: cd  "/Users/jorgefernandes/Dropbox/Projectos/Fernandes_Won_Chapter/Data_Final/"
use "Data_Fernandes_Won_OUP.dta", replace


label variable gender "Gender"
label variable Seniority "Seniority"
label variable comchair "Committee Chair"
label variable government "Government Party"
label variable leader_PPG "PPG Leadership"
label variable PartySize "Party Size"
label variable age "Age"
label variable age2 "Age (Sqd.)"

ssc install coefplot

**Figure 1**
* Gender Participation in Legislative Debates
* Prepare data
frame put _all, into(party_gender)
frame party_gender{
	collapse (count) MP_ID (sum) Speeches Words, by(party_family gender)

	reshape wide MP_ID Speeches Words, i(party_family) j(gender)
	rename (MP_ID0 Speeches0 Words0) (men Speeches_men Words_men)
	rename (MP_ID1 Speeches1 Words1) (women Speeches_women Words_women)

	egen mp = rowtotal(men women)
	egen speeches_total = rowtotal(Speeches_*)
	replace men = men / mp
	replace women = women / mp
	replace Speeches_men = Speeches_men / speeches_total
	replace Speeches_women = Speeches_women / speeches_total

	egen words_total = rowtotal(Words_*)
	replace Words_women = Words_women / words_total

	* Figure 1
	graph bar Speeches_women Words_women women, over(party_family) ///
		legend(order(1 "% Speeches" 2 "% Words" 3 "% of Women in Legislative Party") row(1)) ///
		ytitle("Percentage") ///
		bar(1, `baropt') bar(2, `baropt') bar(3, `baropt') ///
		b1title("Party family") plotregion(margin(medium))
}

** Seniority and Participation in Legislative Debates
* Prepare data
frame put gender Speeches Seniority, into(part_seniority)
frame part_seniority{
	gen c = "."
	replace c = "0" if(Seniority == 0)
	replace c = "1" if(Seniority == 1)
	replace c = "2" if(Seniority == 2)
	replace c = "3" if(Seniority == 3)
	replace c = "4+" if(Seniority >= 4)

	collapse (mean) Speeches, by(c gender)
	reshape wide Speeches, i(c) j(gender)
	rename (Speeches0 Speeches1) (Men Women)

	* Figure 2
	graph bar (sum) Men Women, ///
	legend(order(1 "Men" 2 "Women")) ///
	over(c) plotregion(margin(medium)) b1title("Seniority") ///
	bar(1, `baropt') bar(2, bstyle(p3bar) fcolor("70 70 70") `baropt')
}


**IMPORTANT: the following analysis contains covariates that are meant to be used as an example. Please the covariates according to the general document of instructions that we have provided previously**

** Negative Binomial Regression
nbreg Speeches gender Seniority comchair government leader_PPG PartySize age age2 i.party_family_n exposure_log i.Leg, cluster(MP_ID)
coefplot, ci(95) aspect(1) keep(gender || Seniority || comchair || government || leader_PPG || PartySize) xline(0, lpattern(shortdash) lcolor(black)) rename( /// 
	gender  = "Gender" ///
	Seniority = "Seniority" ///
	comchair = "Committee Chair" ///
	government = "Government Party" ///
	leader_PPG = "Legislative Party Leadership" ///
	PartySize = "Party Size" ///
	) `display' `ci'	
est store nbreg
graph save "speeches.gph", replace


**Gen DV for OLS regression

gen words_exp=(Words/exposure)

** OLS*
reg words_exp gender Seniority comchair government leader_PPG PartySize age age2 i.party_family_n i.Leg, cluster(MP_ID)
coefplot, ci(95) aspect(1) keep(gender || Seniority || comchair || government || leader_PPG || PartySize) xline(0, lpattern(shortdash) lcolor(black)) `display' `ci'	
est store ols
graph save "speeches_ols.gph", replace


**If you decide to make further figures in your country-specific section, please make sure that you follow the style guide in this do-file.
