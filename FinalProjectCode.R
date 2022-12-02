#Installing packages as we need them
rm(list = ls())
library(dplyr)

#Read in the data and examine the columns
fp = read.csv("Data/pbp-2021.csv", stringsAsFactors = TRUE)
str(fp)



#Cleaning Data--------------------------------------------

#Some of the observations in this data set are taken even though no play was performed. These observations
#are timeouts and such. We chose to remove timeouts and other play stoppage from the data because they were not 
#relevant to our analysis. All forms of play stoppage don't have an offense team  and can be filtered out based on that
fp_cleaned = subset(fp, OffenseTeam != "")

#The original csv file included some blank columns in between the columns containing
#actual values, these were removed. We also removed the game description, identifier, and 
#date because all of the games took place in the same season. TeamWin just had all 0s in it and was removed.
#The challenger column only had NAs in it and was dropped as well
#NextScore Had all 0s and was removed
#IsMeasurement is all 0s and was removed
#We determined that OffenseTeam and DefenseTeam weren't relevant and were removed.
#We dropped IsRush and IsPass because it was redundant information with the PlayType column

fp_cleaned = subset(fp_cleaned, select = -c(X, X.1, X.2, X.3, GameId, GameDate, SeasonYear, Description, TeamWin, 
                                            Challenger, NextScore, OffenseTeam, DefenseTeam, IsRush, IsPass,
                                            PenaltyTeam, PenaltyType, YardLineFixed,YardLineDirection, IsMeasurement, IsTwoPointConversion))

#We also decided to combine the quarter, minute, and second variables into one, larger,
#time variable. This variable takes the form of Minutes.seconds
TimeOfPlay = ((fp_cleaned$Quarter -1) * 15) + (fp_cleaned$Minute) + (fp_cleaned$Second / 60)
fp_cleaned$TimeOfPlay = TimeOfPlay
fp_cleaned = subset(fp_cleaned, select = -c(Quarter, Minute, Second))


#We decided to impute values of NOPASS when there was a blank in PassType and NORUSH when there was a blank in RushDirection
fp_cleaned$RushDirection = as.character(fp_cleaned$RushDirection)
#modify blanks in RushDirection to NORUSH
fp_cleaned$RushDirection[fp_cleaned$RushDirection == ""] = "NORUSH"
#see if it worked
table(fp_cleaned$RushDirection)
#let's turn it into a factor - Ordered Factor
#there is an inherent ordering in kitchen qualities that we'll want
#to show up in plots, let's respect that
fp_cleaned$RushDirection = factor(fp_cleaned$RushDirection,
                            levels = c("NORUSH", "LEFT END", "LEFT TACKLE", "LEFT GUARD","CENTER", "RIGHT GUARD", "RIGHT END", "RIGHT TACKLE"), 
                            labels = c( "No Rush", "Left End", "Left Tackle", "Left Guard", "Center", "Right Guard", "Right End", "Right Tackle"))
table(fp_cleaned$RushDirection)

#Modify blanks in pass type to NOPASS


#We are choosing to look at IsTouchdown for our y variable, after deliberation, we decided to just look at scoring plays

#Make TD var a factor Include 2pt convs in this as well 

fp_cleaned = fp_cleaned %>%
  mutate(EndzoneScore = ifelse(IsTwoPointConversionSuccessful == 1 | IsTouchdown == 1, 1, 0))

fp_cleaned$EndzoneScore = factor(fp_cleaned$EndzoneScore,
                                 levels = c(1, 0),
                                 labels = c("Score" ,"No Score"))
table(fp_cleaned$EndzoneScore)                                 
         
#Drop IsNoPlay where it is 1 and then remove the column
fp_cleaned = subset(fp_cleaned, select = -c(IsTwoPointConversionSuccessful, IsTouchdown))

fp_cleaned$PassType = as.character(fp_cleaned$PassType)
fp_cleaned$PassType[fp_cleaned$PassType == ""] = "NOPASS"
fp_cleaned$PassType[fp_cleaned$PassType == "BACK TO"] = "NOPASS"
fp_cleaned$PassType[fp_cleaned$PassType == "INTENDED FOR"] = "NOPASS"
fp_cleaned$PassType[fp_cleaned$PassType == "LEFT TO"] = "SHORT LEFT"
fp_cleaned$PassType[fp_cleaned$PassType == "MIDDLE TO"] = "SHORT MIDDLE"
fp_cleaned$PassType[fp_cleaned$PassType == "NOT LISTED"] = "NOPASS"
fp_cleaned$PassType[fp_cleaned$PassType == "RIGHT TO"] = "SHORT RIGHT"
fp_cleaned$PassType[fp_cleaned$PassType == "RIGHT. PENALTY"] = "SHORT RIGHT"
table(fp_cleaned$PassType)
fp_cleaned$PassType = factor(fp_cleaned$PassType,
                                  levels = c("NO PASS", "DEEP LEFT", "SHORT LEFT", "DEEP MIDDLE", "SHORT MIDDLE", "DEEP RIGHT", "SHORT RIGHT"),
                                  labels = c("No Pass", "Deep Left", "Short Left", "Deep Middle", "Short Middle", "Deep Right", "Short Right")
                                  )
table(fp_cleaned$PassType)
#Left TO becomes SHORT LEFT, it was an 8 yard gain to the left
#INTENDED FOR becomes NO PASS, the play was a fumble
#BACK TO is not a pass it was a backwards lateral during a punt based on reading the description, it becomes no pass
#NOT LISTED WAS a fumble and bad play -> no pass
#MIDDLE TO was a pass for 14 yards, not very long short middle
#Right Penalty -> short right, we know it was a pass to the right based on play description, we went with the more common of the two
#Right TO -> short right, play description said it was a two yard touchdown