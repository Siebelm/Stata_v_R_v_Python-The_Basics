# Coding Presentation (Stata v R v Python)
# R Syntax
# Fors Marsh Group
# July 8, 2018
# Author: Michael Siebel

#######################
##Load data set
#set working directory
setwd('C:\\Users\\siebe\\Documents\\4 FMG\\Coding Presentation')

##Load data set
#Import CSV & drop variables
df = read.csv('results.csv') 
df$city = NULL
df$country = NULL

#Check first few rows
head(df)

#Check variables
sapply(df, class)

#Check missings (NA's)
sapply(df, function(x)sum(is.na(x)))

#Subset dataset to only include Germany matches 
df_ger = df[df$home_team=='Germany' | df$away_team=='Germany',]

head(df_ger)


##############################
##Recodes and Data Manpulation
#
df_ger$match = 1

tail(df_ger)

#Conditional recode (Define Goals Scored)
df_ger$goals    = ifelse(df_ger$home_team=='Germany', df_ger$home_score, 
                         ifelse(df_ger$away_team=='Germany', df_ger$away_score, NA))

#Conditional recode (Define Goals Conceded)
df_ger$conceded = ifelse(df_ger$home_team=='Germany', df_ger$away_score, 
                         ifelse(df_ger$away_team=='Germany', df_ger$home_score, NA))

tail(df_ger)

#Simple recode (Define Goal Differential)
df_ger$goaldiff = df_ger$goals-df_ger$conceded

tail(df_ger)

#Dummy recode (Define Friendly v Competitive match)
df_ger$friendly = as.logical(df_ger$tournament=='Friendly')

tail(df_ger)

#Categorical recode (home vs away vs neutral matches)
df_ger$home[df_ger$home_team=='Germany'] = 'home'
df_ger$home[df_ger$away_team=='Germany'] = 'away'
df_ger$home[df_ger$neutral=='TRUE']      = 'neutral'

tail(df_ger)

#Conditional recode (opponent)
df_ger$opponent = ifelse(df_ger$home_team=='Germany', as.character(df_ger$away_team), 
                         ifelse(df_ger$away_team=='Germany', as.character(df_ger$home_team), NA))

tail(df_ger)

#String variables (Define year)
df_ger$year = as.numeric(substr(df_ger$date,1,4))

tail(df_ger)

#Drop redundant variables
df_ger = subset(df_ger, select = -c(home_score, away_score, tournament,
               neutral, home_team, away_team))

tail(df_ger)


########################
##Descriptive Statistics
#Tabs
tabHome = table(df_ger$home, df_ger$friendly)
tabHome

#Unweighted proportions
propHome = prop.table(tabHome, 2)*100
round(propHome, 2)

#Goal difference table
library('plyr')
library('dplyr')
goaldiffTable = group_by(df_ger, home, friendly)
goaldiffTable = summarize(goaldiffTable,
                       conceded = mean(conceded),
                       goaldiff = mean(goaldiff),
                       goals = mean(goals))
goaldiffTable

#Opponent table
threeOpponents = df_ger[df_ger$opponent=='Mexico' | df_ger$opponent=='Sweden' | df_ger$opponent=='Korea Republic',]
opponentTable = group_by(threeOpponents, opponent)
opponentTable = summarize(opponentTable, 
                          goaldiff = mean(goaldiff),
                          match = sum(match))
opponentTable


#######################
##Graphs
#Histogram
library('ggplot2')
ggplot(df_ger, aes(goaldiff)) +
  geom_histogram(fill = 'blue', binwidth=.75, position='dodge') + 
  ylab('Count') + xlab('Goal Differential') + ggtitle('Histogram of Goal Differences')

ggplot(df_ger, aes(goaldiff, fill = friendly)) +
  geom_histogram(binwidth=.75, position='dodge') + 
  ylab('Count') + xlab('Goal Differential') + ggtitle('Histogram of Goal Differences') + 
  theme(legend.position='bottom')

#Line Graphs
df_ger_byyear = ddply(df_ger, .(year), summarize,  goals=mean(goals), 
                      conceded=mean(conceded), goaldiff=mean(goaldiff))
df_ger_byyear$conceded = df_ger_byyear$conceded*-1
tail(df_ger_byyear)

df_goals_byyear = ggplot(df_ger_byyear) + geom_line(aes(x=year, y=goals), color='dark green') +
  ylab('Goals') + xlab('Year') + ggtitle('Goals Scored by Year')
df_goals_byyear

df_conceded_byyear = ggplot(df_ger_byyear) + geom_line(aes(x=year, y=conceded), color='maroon') +
  ylab('Conceded') + xlab('Year') + ggtitle('Goals Conceded by Year')
df_conceded_byyear

df_goaldiff_overlay_byyear = ggplot(df_ger_byyear) + geom_line(aes(x=year, y=goaldiff), color='blue') +
  ylab('Goal Differential') + xlab('Year') + ggtitle('Goal Differential by Year') + labs(subtitle='Overlay Chart') +
  geom_line(aes(x=year, y=goals), color='dark green') + geom_line(aes(x=year, y=conceded), color='maroon') +
  geom_hline(yintercept = 0, color='dark grey')
df_goaldiff_overlay_byyear




