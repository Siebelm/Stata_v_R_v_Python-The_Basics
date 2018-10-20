set more off

set more off, permanently
set line 200
capture log close
log using Stata_WClog.smcl, replace

************************
**Load data set
*set working directory
cd "C:\Users\siebe\Documents\4 FMG\Coding Presentation"

*Import CSV & drop variables
import delimited using results.csv
save results.dta, replace
drop city country

*Check first few rows
list * in 1/10

*Check variables
describe

*Check missings (dots or ".")
findit mdesc
mdesc

*Subset dataset to only include Germany matches
keep if home_team=="Germany" | away_team=="Germany"

list * in 1/10


************************
**Recodes and Data Manpulation
*Simple recode (each observation equals 1)
gen match = 1

list * in 1/10

*Conditional recode (Define Goals Scored)
gen goals = home_score if home_team=="Germany"
replace goals = away_score if away_team=="Germany"

*Conditional recode (Define Goals Conceded)
gen conceded = away_score if home_team=="Germany"
replace conceded = home_score if away_team=="Germany"

list home_team away_team home_score away_score goals conceded in 921/931

*Simple recode (Define Goal Differential)
gen goaldiff = goals - conceded

list home_score away_score goals conceded goaldiff in 921/931

*Dummy recode (Define Friendly v Competitive match)
gen friendly = tournament == "Friendly"

list tournament friendly in 921/931

*Categorical recode (home vs away vs neutral matches)
gen home = "home" if home_team=="Germany"
replace home = "away" if away_team=="Germany"
replace home = "neutral" if neutral=="TRUE"
encode home, gen(home_num)
label list home_num

list home_team away_team neutral home home_num in 921/931

*Conditional recode (opponent)
gen opponent = away_team if home_team=="Germany"
replace opponent = home_team if away_team=="Germany"

list home_team away_team opponent in 921/931

*String variables (Define year)
gen year = substr(date,1,4)
destring year, replace

list date year in 921/931

*Drop redundant variables
drop home_score away_score tournament neutral home_team away_team

list * in 921/931


************************
**Descriptive Statistics
*Tabs
tab home friendly

*Unweighted proportions
prop home_num friendly

*Goal difference table
table home friendly, contents(mean conceded)
table home friendly, contents(mean goaldiff)
table home friendly, contents(mean goals)
	*or*
forval frdly=0/1 {
	foreach hme in away home neutral {
		foreach var in conceded goaldiff goals {
			local slabel: value label friendly
			local vlabel: label `slabel' `frdly'
			di as res _n "`var' if home is `hme' and friendly is `vlabel'" 
			mean `var' if home=="`hme'" & friendly==`frdly'
		}
	}
}

*Opponent table
mean goaldiff if opponent=="Korea Republic"
tab match if opponent=="Korea Republic"


************************
**Graphs
*Histogram
hist goaldiff, bin(25) freq ///
	title(Histogram of Goal Differences) ///
	ytitle(Count) xtitle(Goal Differential) ///
	title(Histogram of Goal Differential)

*Line graphs
bysort year: egen m_goals = mean(goals)
twoway (line m_goals year, lcolor(green)), xlabel(1908(20)2018) ///
	title(Goals Scored by Year)

bysort year: egen m_conceded = mean(conceded)
replace m_conceded = m_conceded*-1
twoway (line m_conceded year, lcolor(red)), xlabel(1908(20)2018) ///
	title(Goals Conceded by Year)

bysort year: egen m_goaldiff = mean(goaldiff)
twoway (line m_goaldiff year, lcolor(navy)), xlabel(1908(20)2018) ///
	title(Goal Differential by Year)

twoway (line m_goaldiff year, lcolor(navy)) ///
	(line m_goals year, lcolor(green) lwidth(thin)) ///
	(line m_conceded year, lcolor(red) lwidth(thin)), ///
	xlabel(1908(30)2018) yline(0, lcolor(grey) lwidth(thin)) ///
	title(Goal Differential by Year) subtitle(Overlay Chart) ///
	ytitle(Year)

translate Stata_WClog.smcl Stata_WClog.pdf, rmargin(.5)
translate Stata_WClog.smcl Stata_WClog.txt , replace





