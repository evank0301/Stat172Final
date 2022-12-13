# Stat172Final
Final Project for Stat172 Fall 2022


  In this project, we take a deep dive into NFL play data from the 2021-2022 season. The goal was to figure out what factors of offensive plays cause more scores in the endzone. The main objective of an offensive coach is to score more points and scoring in the endzone is the most effiecient way of doing so. Instead of just looking at touchdowns, we looked at all endzone scores which included two-point conversions. Our logic was that if a play was effective at getting into the endzone from the 3 yard line after a touchdown is scored, it should be just as effective as the touchdown scoring play from the 3 yard line. 

   We completed this investigation in the latest version of R and Rstudio using the dplyr, ggplot2, rpart, rpart.plot, pROC, and randomForest. We first cleaned the data (specificed later exactly what we did) then we built a random forest and importance chart. That was followed by a glm and more exploratory analysis.

  The data we found was from nflsavant.com and is the play data from the 2021-2022 season. Before cleaning it has 42,796 observations with 45 variables.

  To clean the data, we cut out many variables because they either had all values of NA or 0. These include: X, X.1, X.3, TeamWin, Challenger, NextScore, IsMeasurement, IsSack. The next step was cutting out any variables we deemed irrelevant to our study. Many were varaibles like Season which does not help since we are only looking at 1 season of plays. These include: Game Date, GameID, Season, Description, Offensive Team, Defensive Team, Penalty Team, Penalty Type. Next we cut out variables that were redundant. Many of these were variables that had the same information as a different variable that would be more helpful. These include: (IsRush, IsPass, IsTwo-Point-Conversion) which are all in PlayType, (YardLineFixed and YardLineDirection) which make up YardLine, and (IsIncomplete) which is just Yards = 0. Finally we did other cleaning which cannot be putinot a general category. This includes condensing quarter, minute, and second into one time varailbe. We also imputed "NOPASS" and "NORUSH" into PassType and RushDirection respectively for any observaton that did not fit the other categories. Finally, we decided to only examine “Pass”, “Rush”, “Scramble”, and “Two-Point-Conversion” play types are those were the only ones that truly allowed for an endzone score on the offensive end. 

There were three people that contributed to this project. Evan Krueger, Eldrick Dossavi-Alipoeh, and Ryan Blokhuis
