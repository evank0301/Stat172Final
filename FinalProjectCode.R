#Installing packages as we need them
rm(list = ls())
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(pROC)
library(randomForest)

#Read the data for our project into R so we can examine the columns
fp = read.csv("Data/pbp-2021.csv", stringsAsFactors = TRUE)
str(fp)



#Cleaning Data--------------------------------------------

#Some of the observations in this data set are taken even though no play was performed. These observations
#are timeouts and such. We chose to remove timeouts and other play stoppage from the data because they were not 
#relevant to our analysis. All forms of play stoppage don't have an offense team  and can be filtered out based on that
fp_cleaned = subset(fp, OffenseTeam != "")

#The original csv file included some blank columns in between the columns containing
#actual values, these columns (X, X.1, and X.3) were removed. 
#We also removed the GameId, GameDate, SeasonYear, and Description colums because they contained
#Data that was not relevant to our analysis. All of the observations are from the same season
#TeamWin had all 0s in it and was removed.
#The Challenger column only had NAs in it and was dropped as well
#NextScore Had all 0s and was removed
#IsMeasurement is all 0s and was removed
#We determined that OffenseTeam and DefenseTeam weren't relevant and were removed.
#We dropped IsRush, IsPass, and IsTwoPointConversion because it was redundant information with the PlayType column
#We dropped PenaltyTeam and PenaltyType because these are extranious pieces of information for our model
#We dropped YardLineFixed and YardLineDirection because there is another YardLine variable that contains this information
#We dropped IsSack because all of the values were 0
#Propose drop of IsIncomplete because it is redundant with pass plays that gain 0 yds.
fp_cleaned = subset(fp_cleaned, select = -c(X, X.1, X.2, X.3, GameId, GameDate, SeasonYear, Description, TeamWin, 
                                            Challenger, NextScore, OffenseTeam, DefenseTeam, IsRush, IsPass,
                                            PenaltyTeam, PenaltyType, YardLineFixed,YardLineDirection, IsMeasurement, IsTwoPointConversion, IsSack, IsIncomplete))

#We also decided to combine the quarter, minute, and second variables into one, larger,
#time variable. This variable takes the form of Minutes.seconds
TimeOfPlay = ((fp_cleaned$Quarter -1) * 15) + (fp_cleaned$Minute) + (fp_cleaned$Second / 60)
fp_cleaned$TimeOfPlay = TimeOfPlay
fp_cleaned = subset(fp_cleaned, select = -c(Quarter, Minute, Second))


#We decided to impute values of NOPASS when there was a blank in PassType and NORUSH when there was a blank in RushDirection

#modify blanks in RushDirection to NORUSH
fp_cleaned$RushDirection = as.character(fp_cleaned$RushDirection)
fp_cleaned$RushDirection[fp_cleaned$RushDirection == ""] = "NORUSH"
#see if it worked
table(fp_cleaned$RushDirection)
#Order the factors for RushDirection based on the Offensive line
fp_cleaned$RushDirection = factor(fp_cleaned$RushDirection,
                            levels = c("NORUSH", "LEFT END", "LEFT TACKLE", "LEFT GUARD","CENTER", "RIGHT GUARD", "RIGHT TACKLE", "RIGHT END"), 
                            labels = c( "No Rush", "Left End", "Left Tackle", "Left Guard", "Center", "Right Guard", "Right Tackle", "Right End"))
table(fp_cleaned$RushDirection)

#Modify blanks in pass type to NOPASS
fp_cleaned$PassType = as.character(fp_cleaned$PassType)
fp_cleaned$PassType[fp_cleaned$PassType == ""] = "NOPASS"
fp_cleaned$PassType[is.na(fp_cleaned$PassType)] = "NOPASS"
fp_cleaned$PassType[fp_cleaned$PassType == "BACK TO"] = "NOPASS"
fp_cleaned$PassType[fp_cleaned$PassType == "INTENDED FOR"] = "NOPASS"
fp_cleaned$PassType[fp_cleaned$PassType == "LEFT TO"] = "SHORT LEFT"
fp_cleaned$PassType[fp_cleaned$PassType == "MIDDLE TO"] = "SHORT MIDDLE"
fp_cleaned$PassType[fp_cleaned$PassType == "NOT LISTED"] = "NOPASS"
fp_cleaned$PassType[fp_cleaned$PassType == "RIGHT TO"] = "SHORT RIGHT"
fp_cleaned$PassType[fp_cleaned$PassType == "RIGHT. PENALTY"] = "SHORT RIGHT"
#BACK TO is not a pass it was a backwards lateral during a punt based on reading the description, it becomes NOPASS
#INTENDED FOR becomes NO PASS, the play was a fumble
#LEFT TO becomes SHORT LEFT, it was an 8 yard gain to the left
#MIDDLE TO becomes SHORT MIDDLE, it was a pass for 14 yards
#NOT LISTED becomes NO PAS, it was a fumble and bad play 
#RIGHT TO becomes SHORT RIGHT play description said it was a two yard touchdown
#RIGHT. PENALTY becomes SHORT RIGHT, we know it was a pass to the right based on play description, we went with the more common of the two

table(fp_cleaned$PassType)
fp_cleaned$PassType = factor(fp_cleaned$PassType,
                             levels = c("NOPASS", "DEEP LEFT", "SHORT LEFT", "DEEP MIDDLE", "SHORT MIDDLE", "DEEP RIGHT", "SHORT RIGHT"),
                             labels = c("No Pass", "Deep Left", "Short Left", "Deep Middle", "Short Middle", "Deep Right", "Short Right")
)
table(fp_cleaned$PassType)

#We are choosing to look at IsTouchdown for our y variable, after deliberation, we decided to just look at scoring plays in the endzone
#Make TD var a factor Include 2pt convs in this as well 
fp_cleaned = fp_cleaned %>%
  mutate(EndzoneScore = ifelse(IsTwoPointConversionSuccessful == 1 | IsTouchdown == 1, 1, 0))
fp_cleaned$EndzoneScore = factor(fp_cleaned$EndzoneScore,
                                 levels = c(1, 0),
                                 labels = c("Score" ,"No Score"))
table(fp_cleaned$EndzoneScore)                                 
fp_cleaned = subset(fp_cleaned, select = -c(IsTwoPointConversionSuccessful, IsTouchdown))

#When we were investigating our data some more in class on 12/5, we ran across some issues that would arise if we didn't
#Take action against them. The PlayType variable has many fields that no NFL OC would ever call (sack / fumble / punt) we
#Decided that these levels make no sense to include in the data because they would lead to complete separation later (it is unlikely if not impossible to score on these plays)
#We decided to remove all occurrences of plays that were not Rushes, Passes, Two Pt Conversions, or Scrambles because
fp_cleaned$PlayType = as.character(fp_cleaned$PlayType)
fp_cleaned = subset(fp_cleaned, PlayType == c("RUSH", "PASS", "SCRAMBLE", "TWO-POINT CONVERSION"))
table(fp_cleaned$PlayType)
fp_cleaned$PlayType = factor(fp_cleaned$PlayType,
                                levels = c("PASS", "RUSH", "SCRAMBLE", "TWO-POINT CONVERSION"),
                                labels = c("Pass", "Rush", "Scramble", "Two Point Conversion"))


#Exploratory Plots For Rush and Pass Type----
ggplot(data = fp_cleaned) +
  geom_bar(aes(x = PlayType))

#Make a plot of the distribution of RushDirection.
#I chose to exclude the No Rush level because it overshadowed all of the other values
fp_cleaned %>%
  filter(RushDirection != "No Rush") %>%
  ggplot(aes(x = RushDirection)) +
  geom_bar(fill = "#99d8c9") + 
  ggtitle("Distribution of Rushing Directions") + 
  labs(x = "Rush Direction", y = "Number of Plays")

#Examine the above plot again, but fill based on the play scoring in the endzone
fp_cleaned %>%
  filter(RushDirection != "No Rush") %>%
  ggplot(aes(x = RushDirection, fill = EndzoneScore)) +
  labs(x = "Rush Direction", y = "Frequency") +
  ggtitle("Frequencies of Rushers Reaching the Endzone") + 
  scale_fill_brewer("Outcome of \nthe Play", palette = "BuGn") +
  geom_bar(position = "fill")


#Get a feel for passing frequencies
fp_cleaned %>%
  filter(PassType != "No Pass") %>%
  ggplot(aes(x = PassType)) + 
  geom_bar(fill = "#99d8c9") +
  ggtitle("Distribution of Pass Types") + 
  labs(x = "Type of Pass", y = "Number of Plays")

#Group by scoring again
fp_cleaned %>%
  filter(PassType != "No Pass") %>%
  ggplot(aes(x = PassType, fill = EndzoneScore)) + 
  labs(x = "Type of Pass", y = "Frequency") +
  ggtitle("Frequencies of Scoring on Passing Plays") +
  scale_fill_brewer("Outcome of \nthe Play", palette = "BuGn") +
  geom_bar(position = "fill")




#Initial Random Forest ----
RNGkind(sample.kind = "default")
set.seed(172172) 


#I am using an 80/20 split for training and testing, this is pretty standard
train.idx = sample(x = 1:nrow(fp_cleaned), size = .8*nrow(fp_cleaned))

#Create training data
train.df = fp_cleaned[train.idx, ]
#create testing data
test.df = fp_cleaned[-train.idx, ]


### TREE FITTING ----
#This is just one tree because I wanted to see what it looked like
#set seed before we fit the tree
set.seed(172172172)

#Build the tree with the training information
initialTree = rpart(EndzoneScore ~ ., 
              data = train.df,
              method = 'class') 

rpart.plot(initialTree)

# FOREST FITTING ----
set.seed(172172)

#Construct an initial forest with 10000 trees
fpForest = randomForest(EndzoneScore ~.,
                        data = train.df,
                        ntrees = 10000)

#Examine how this initial forest performed.
fpForest
#Pretty good! There is only a 2.12% OOB error rate, which is not bad
#Attempt to optimize

#One value for each x variable
mtry = c(1:18)

#Make a dataframe for different OOB error rates
keeps = data.frame(m = rep(NA, length(mtry)),
                   OOB_error_rate = rep(NA, length(mtry)))

#Create a random forest for each of the possible mtry values
for(idx in 1:length(mtry)){
  print(paste0("Fitting m = ", mtry[idx]))
  tempforest = randomForest(EndzoneScore ~.,
                            data = train.df,
                            ntree = 10000,
                            mtry = mtry[idx])
  keeps[idx, "m"] = mtry[idx]
  keeps[idx, "OOB_error_rate"] = mean(predict(tempforest) != train.df$EndzoneScore)
}

#Plot the OOB Error Rate vs. mtry values
ggplot(data = keeps) + 
  geom_line(aes(x = m, y = OOB_error_rate))+
  labs(x = "m (Number of Features Used in Random Forest)", y = "OOB Error Rate") +
  ggtitle("Plot of OOB Error Rate vs. Number of Fetures Used")

#Using mtry = 11 resulted in the lowest OOB
min(keeps$OOB_error_rate)

#TUNED FOREST
tunedFpForest = randomForest(EndzoneScore ~.,
                             data = train.df,
                             ntree = 1000,
                             mtry = 11,
                             importance = TRUE)

#Examine the new forest
tunedFpForest


#Plot ROC Curve
pi_hat = predict(tunedFpForest, test.df, type = "prob")[,"Score"]
rocCurve = roc(response = test.df$EndzoneScore,
               predictor = pi_hat,
               levels = c("No Score", "Score"))
plot(rocCurve, print.thres = "best", print.auc = TRUE, main = "ROC Curve for Random Forest")


#Produce an importance plot of the tuned random forest
varImpPlot(tunedFpForest, type = 1)
vi = as.data.frame(varImpPlot(tunedFpForest, type = 1))
vi$Variable <- rownames(vi)
ggplot(data = vi) + 
  geom_bar(fill = "#99d8c9", aes(x = reorder(Variable,MeanDecreaseAccuracy), weight = MeanDecreaseAccuracy), 
           position ="identity") +
  coord_flip() + 
  labs( x = "Variable Name",y = "Importance") + 
  ggtitle("Variable Importance")

#Evan's Refined Plots----
#This plot is interesting, extremely skewed to the right.
#This makes sense though, PI calls can greatly impact a play's success
fp_cleaned %>%
  filter(IsPenaltyAccepted == 1) %>%
  ggplot(aes(x = PenaltyYards)) +
  geom_histogram(binwidth = 1)

#Attempt to show differences between play types and penalty yards.
#This is consistent with what I expected to see
fp_cleaned %>%
  filter(IsPenaltyAccepted == 1) %>%
  ggplot(aes(x = PenaltyYards, fill = PlayType)) +
  geom_histogram(binwidth = 5) + 
  scale_fill_brewer("Type of\n Play", palette = "BuGn") + 
  labs(x = "Penalty Yards", y = "Count") +
  ggtitle("Penalty Yards by Play Type")
#Add colorblind friendly palette, t
fp_cleaned %>%
  filter(IsPenaltyAccepted == 1) %>%
  ggplot(aes(x = YardLine, y = PenaltyYards, color = factor(PlayType))) + 
  geom_point()


fp_cleaned %>% 
  filter(IsPenaltyAccepted == 1) %>%
  ggplot(aes(x = PenaltyYards))

#I thought that this might show the penalty yards that a penalty might
#Have been worth even though it was not accepted, this is not the case
#All yards values where the penalty is not accepted are = to 0.
#This plot is not helpful
fp_cleaned %>% 
  filter(IsPenalty == 1 & IsPenaltyAccepted == 0) %>%
  ggplot(aes(x = PenaltyYards)) + 
  geom_histogram(binwidth = 5)


#Take a look at the frequency of yds to go
#This plot is not helpful, the majority of
#Observations are of plays with ToGo = 10. 
#This makes sense because when a 1st down is 
#gained, the next play starts with 10 yds to go
fp_cleaned %>%
  ggplot(aes(x = ToGo)) + 
  geom_histogram(binwidth = 1)

#Look at subsets of plays with < 10 yds to go and > 10 yds to go
fp_cleaned %>%
  filter(ToGo < 10) %>%
  ggplot(aes(x = ToGo)) + 
  geom_histogram(binwidth = 1)

#Add labels and a different color palette
fp_cleaned %>%
  filter(ToGo < 10) %>%
  ggplot(aes(x = ToGo, fill = PlayType)) + 
  geom_histogram(binwidth = 1) + 
  scale_fill_brewer("Type of\n Play", palette = "BuGn") +
  labs(x = "Distance Until 1st Down", y = "Count") +
  ggtitle("Distribution of Yards Until 1st Down Gained (< 10 Yards to Go)")

#Add labels and a different color palette
fp_cleaned %>%
  filter(ToGo > 10) %>%
  ggplot(aes(x = ToGo, fill = PlayType)) + 
  geom_histogram(binwidth = 5) + 
  scale_fill_brewer("Type of\n Play", palette = "BuGn") +
  labs(x = "Distance Until 1st Down", y = "Count") +
  ggtitle("Distribution of Yards Until 1st Down Gained (> 10 Yards to Go)")





