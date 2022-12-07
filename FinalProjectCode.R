
# Stats 172 Final #

#Installing packages as we need them
rm(list = ls())

library(ggplot2) # 
library(glmnet) # 
library(pROC) # 
library(dplyr)
#Read in the data and examine the columns
fp = read.csv("~/Desktop/Stat 172/pbp-2021.csv",
              stringsAsFactors = TRUE)



#Cleaning Data--------------------------------------------
#Some of the observations in this data set are taken even though no play was performed. These observations
#are timeouts and such. We chose to remove timeouts and other play stoppage from the data because they were not 
#relevant to our analysis. All forms of play stoppage don't have an offense team  and can be filtered out based on that
fp_cleaned = subset(fp, OffenseTeam != "")
fp_cleaned = fp_cleaned %>%
  rename("PassType" = "X.4")
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
#Right TO -> short right, play description said it was a two yard touchdown


#Plots with Yards
#Scores based on how many yards the play gained
ggplot(data = fp_cleaned)+
  geom_histogram(aes(x=Yards, fill = EndzoneScore), position = "fill", binwidth = 5)+
  labs(x = "Yards Gained on the Play", y = "Play scored")+
  ggtitle("Endzone scores on how long the play was")+
  scale_fill_brewer("Scored", palette = "YlOrRd")
#Most interesting thing on this histogram is that there are plays that scored
#touchdowns with negative yards gained on the play. Is this counting defensive touchdwons?
#if that is the case then a play of more than -20 yards would have happened

#plot of yards gained over pass play type
ggplot(data = fp_cleaned)+
  geom_histogram(aes(x=Yards, fill = PassType), position = "fill", binwidth = 5)+
  labs(x = "Yards Gained on Pass Plays", y = "Pass Type")+
  ggtitle("Yards gained based on Pass play type")+
  scale_fill_brewer("Pass Type", palette = "YlOrRd")
#Of all the deep pass pays, it seems like deep right is the most effective
#in some areas deep sleep also seems to work well. Ironically from about 77-85
#yard lines, short left passes seem oddly effective. Although that may be due to a 
#small sample size and the only pass play(s) to score from around 100 yards is 
#short right

#plot of yards gained over rush direction 
ggplot(data = fp_cleaned)+
  geom_histogram(aes(x=Yards, fill = RushDirection), position = "fill", binwidth = 5)+
  labs(x = "Yards Gained", y = "Rush Direction")+
  ggtitle("Yards gained based on Rush Direction")+
  scale_fill_brewer("Rush \nDirection", palette = "YlOrRd")
#It seems that the majority of plays for each amount of yards gained are not rushes
#at all. It also seems the majority of rush plays are about 3-5 yards.The longest
#runs (55+ seem to come from rushes following the left side)

#PlayType
#Score by play type
ggplot(data = fp_cleaned)+
  geom_bar(aes(x=PlayType, fill = EndzoneScore), position = "fill")+
  labs(x = "Type of Play", y = "Play scored")+
  coord_flip()+
  ggtitle("Endzone scores based on Play Type")+
  scale_fill_brewer("Scored", palette = "YlOrRd")
#Two-point conversion are nearly 50% successful in getting into the end zone,
#Of all pass and rush plays, about 5% truly end in a score. Exception and *blank*
#both have interestily high score percentages, would like to maybe figure out what that
#is about


#YardLine
#Score by yard line
ggplot(data = fp_cleaned)+
  geom_histogram(aes(x=YardLine, fill = EndzoneScore), position = "fill", binwidth = 5)+
  labs(x = "Yard Line Play Started On", y = "Play scored")+
  ggtitle("Endzone Scores Based on Where the Play Started")+
  scale_fill_brewer("Scored", palette = "YlOrRd")
#The interesting aspect of this graph is that a large portion of plays run from
#inside about the 10 yard line score. There is also an interesting increase at about the 
#20 yard line compared to about the 15 yard line, the trend almost seems exponential

#Time
#Score by time during the game
ggplot(data = fp_cleaned)+
  geom_histogram(aes(x=TimeOfPlay, fill = EndzoneScore), position = "fill", binwidth = 5)+
  labs(x = "Time of Play", y = "Play scored")+
  ggtitle("Endzone Scores Based on When the Play was Ran")+
  scale_fill_brewer("Scored", palette = "YlOrRd")
#There does not seem to be a certain time of the game that allows for more scores
#except for the very end of overtime where teams my be trying to just tie the game instead
#of winning it







#Plot with Score
Score <- ggplot(fp_cleaned, aes(x = EndzoneScore)) +
  geom_bar()
Score

#Plot with formation 
#Score by Formation Type 
ggplot(data = fp_cleaned)+
  geom_histogram(aes(x= Formation, fill = EndzoneScore), position = "fill", binwidth = 5)+
  labs(x = "Formation Type", y = "Play scored")+
  ggtitle("Endzone Scores Based on Formation")+
  scale_fill_brewer("Scored", palette = "YlOrRd")














