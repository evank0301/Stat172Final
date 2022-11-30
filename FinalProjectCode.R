#Installing packages as we need them
rm(list = ls())


#Read in the data and examine the columns
fp = read.csv("Data/pbp-2021.csv", stringsAsFactors = TRUE)
str(fp)



#Cleaning Data--------------------------------------------
#The original csv file included some blank columns in between the columns containing
#actual values, these were removed. We also removed the game, description, identifier, and 
#date because all of the games took place in the same season. TeamWin just had all 0s in it and was removed
fp_cleaned = subset(fp, select = -c(X, X.1, X.2, X.3, GameId, GameDate, Description, TeamWin))

#We also decided to combine the quarter, minute, and second variables into one, larger,
#time variable. This variable takes the form of Minutes.seconds
TimeOfPlay = ((fp_cleaned$Quarter -1) * 15) + (fp_cleaned$Minute) + (fp_cleaned$Second / 60)
fp_cleaned$TimeOfPlay = TimeOfPlay
fp_cleaned = subset(fp_cleaned, select = -c(Quarter, Minute, Second))

#Ask about Offense and Defense team in class 11/30, unsure if it would be wise to include them
#Run plays don't have pass vars and vice versa, what do we do?

#Some of the observations in this data set are taken even though no play was called. These observations
#are timeouts and such. We chose to remove timeouts and other play stoppage from the data because they were not 
#relevant to our analysis
fp_cleaned = subset(fp_cleaned, OffenseTeam != "")


#We are choosing to look at IsTouchdown for our y variable, after deliberation, we decided to just look at scoring plays

