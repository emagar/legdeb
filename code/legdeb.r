#################################################################################
## USE data-prep.r FIRST TO PREPARE DATA FRAMES WITH DIPUTADO-BY-DIPUTADO DATA ##
#################################################################################

library(lubridate) # date handling

rm(list = ls())

# paths
dd <- "/home/eric/Downloads/Desktop/data/leg-debate/data/"
wd <- "/home/eric/Downloads/Desktop/data/leg-debate/"
setwd(dd)

# LOAD DATA
load(file = "speech-periodo-60.RData")
load(file = "speech-periodo-62.RData")
load(file = "speech-periodo-64.RData")
ls()

# merge into single data frame
data <- data.frame()
data <- rbind(data, data.periodo.60)
data <- rbind(data, data.periodo.62)
data <- rbind(data, data.periodo.64)

# descriptive stats



####################################
## PREPARE VARIABLES FOR ANALYSIS ##
####################################

DONE 1. drop speeches of less than 50 words

DONE 2. include speeches regardless of their nature in the analysis. In some countries, there are reasons to include only a certain type of speech (e.g., bill debates). We are happy to accommodate chapters where the authors do not use all debates, provided that there is a good justification in the text.

DONE 3. In terms of window of observation/time period under study: we don’t have a particular guideline for this. Please use the window of observation that you believe is more representative of the politics of legislative debate in your country. Ideally we would like each chapter to include several legislative periods, but we are pragmatic here, considering data availability.

EMM: Terminology
- A Legislature (with Roman numerals for reasons I ignore) is an elected chamber for a legislative term, called a Congress in the U.S. Concurrent with presidential elections the chamber of deputies renovates in whole, and again at the presidential mid-term. Diputados remain three years in office and were single term-limited up to 2021. The 2021 mid-term election will be the first since 1932 to allow incumbents on the ballot, a major change in Mexican legislative politics.
- Legislative years break into two "ordinary periods", one covering the months of September through December, inclusive, another February through April, also inclusive. "Extraordinary periods" may be convened during the recess in order to consider a specific bill. Analysis aggregates each member's speeches in the duration of a given period (merging together all extraordinary periods that year, if any). So members in a legislative year like 2012-13 (that had no extraordinary periods) have two word aggregates in the dataset, one for each ordinary period; in a year like 2013-14 (that did), they have three word aggregates in the data. Periods are the units of aggregation in the analysis. 
- A session is a specific date in the calendar when diputados met. During ordinary periods, sessions are usually held on Tuesdays and Thursdays, and may be scheduled in other weekdays if the Jucopo so decides. Diputados met on forty and thirty-one days in the first and second ordinary periods of 2013-14, respectively, and nine days in extraordinary periods, for a yearly total of eighty session days. (A session in North-American legislative parlance is a Mexican period.)

DONE 4. What counts as a debate? Please concatenate all interventions that MPs make in a debate, even if the MP speaks multiple times in the said debate.


###########################
## MULTIVARIATE ANALYSIS ##
###########################

VAR DONE, ESTIMATE MODELS 1. Number of speeches that a legislator delivered in the time unit you defined (presumably, for most of you, the legislative term). In doing so, use a negative binomial regression.

VAR DONE, ESTIMATE MODELS 2. Number of words divided by exposure (see below how to operationalize) that a legislator delivered in the time unit you defined (presumably, for the most of you, the legislative term). In doing so, use an OLS.
3. For both cases, please included fixed-effects for the time period of interest (e.g., for the legislative term or the legislative session, depending on your choice). 
4. Please include standard errors clustered at the legislator level.

How to build the DV for the OLS models: 

DONE Where the outcome is the number of Words, you should use Exposure as the denominator to create a ratio. The said ratio should consist of the total number of words legislator i delivered during legislative term t/percentage of time legislator i sat in legislative term t.

The rationale behind this measure is that we want to capture the time that each legislator sits in parliament during a given session. Obviously, a legislator who sits for the duration of the terms has higher chances of taking the floor than a legislator that takes her sit in the middle of the term.

Don’t forget to include Term FE, plus clustered standard errors at the MP level.


################
## COVARIATES ##
################

DONE 1. Gender – dummy variable that takes a value of 1 for Women and 0 for Men
DONE 2. Party Size – continuous variable that measures the absolute number of members of the legislative party
DONE 3. Seniority – continuous variable that measures the number of years the legislator has been in the parliament
DONE, NEEDS MORE DATA 4. Age
DONE 5. Age Squared
DONE 6. Party Family (Dummy variables, using one of party families as reference category)
DONE 7. Committee Chair – dummy variable that takes a value of 1 if the MP holds a committee chair and 0 for all others
DROPPED 8. Minister – dummy variable that takes a value of 1 if the MP is a minister and 0 otherwise
DONE 9. Government party member – dummy variable that takes a value of 1 if the MP belongs to a legislative party that belongs the government and 0 otherwise. Note that we only consider parties that are formally in a coalition (i.e., have members in the executive). Supporting parties, e.g. contract parliamentarism, do not count towards government parties.
DONE 10. Legislative Party Leadership – dummy variable that takes a value of 1 if the MP belongs to the leadership of the parliamentary party group
DROPPED 11. Party Leader – dummy variable that takes a value of 1 if the MP is the party leader and 0 otherwise
DONE 12. Exposure (logged) – continuous variable that measures the percentage of time in which the MP held to her seat in parliament during the unit of time defined in your chapter. For example, if you are using a MP-legislative term unit of observation, in this variable you need to include the percentage of time during the legislative term in which the MP was in the parliament. If MP was in parliament for whole session that would be 1. If the MP joined the parliament later, it could be .7 or .8. If you are using month as the time unit, the same rationale applies. The logged version should *only* be included in the count models (negative binomial). 


################
## NEGBIN OLS ##
################

Please produce a table including both the negative binomial models and the OLS. For negative binomial models, please report the AIC.

Please include up to 5 models in the tables. Consider using a step-wise approach to regression by including covariates into the equation that make most sense in your context. 
Ultimately, we need 2 final models, where all variables are included – one where the dependent variable is the Number of Speeches and the other where the dependent variable is the Number of Words.

As a default we consider the following variables as explanatory:
1. Gender
2. Seniority
3. Committee Chairs
4. Minister
5. Government party member
6. Legislative Party Leadership
7. Party Leader

The following variables are considered controls:

1. Age
2. Age Squared
3. Party Family
4. Exposure (logged)

Please feel free to use variables interchangeably between the two categories depending on the context. 

Please plot marginal effects using the full specification of the negative binomial model. In the said plot, please include explanatory variables only. Controls variables can be omitted.


