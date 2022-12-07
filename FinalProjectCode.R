
fp_cleaned$score_bin = ifelse(fp_cleaned$EndzoneScore == "Score", 1, 0)
#First model, adding YardLine because it was the #1 variable on variable importance
m1 = glm(score_bin ~ YardLine, data = fp_cleaned,
         family =  binomial(link = "logit"))
AIC(m1) #1940.69


#Add SeriesFirstDown
m2 = glm(score_bin~YardLine + SeriesFirstDown, data = fp_cleaned,
         family = binomial(link = "logit"))
AIC(m2) #1331.847


#Add Yards
m3 = glm(score_bin~YardLine + SeriesFirstDown + Yards, data = fp_cleaned,
         family = binomial(link = "logit"))
AIC(m3) #1004.569


#Add IsPenalty
m4 = glm(score_bin~YardLine + SeriesFirstDown + Yards +IsPenalty, data = fp_cleaned,
         family = binomial(link = "logit"))
AIC(m4) #936.3637


#Add PenaltyYards
m5 = glm(score_bin~YardLine + SeriesFirstDown + Yards +IsPenalty +
           PenaltyYards, data = fp_cleaned,
         family = binomial(link = "logit"))
AIC(m5) #937.858 Increase from the past add


#Add ToGo
m6 = glm(score_bin~YardLine + SeriesFirstDown + Yards +IsPenalty +
           PenaltyYards +ToGo, data = fp_cleaned,
         family = binomial(link = "logit"))
AIC(m6) #915.7798 Increase from the past add


#Add PassType
m7 = glm(score_bin~YardLine + SeriesFirstDown + Yards +IsPenalty +
           PenaltyYards +ToGo+PassType, data = fp_cleaned,
         family = binomial(link = "logit"))
AIC(m7) #912.9182 Increase from the past add


#Add IsPenaltyAccepted
m8 = glm(score_bin~YardLine + SeriesFirstDown + Yards +IsPenalty +
           PenaltyYards +ToGo+PassType+IsPenaltyAccepted, data = fp_cleaned,
         family = binomial(link = "logit"))
AIC(m8) #904.94
summary(m8)

#Add IsNoPlay
m9 = glm(score_bin~YardLine + SeriesFirstDown + Yards +IsPenalty +
           PenaltyYards +ToGo+PassType+IsPenaltyAccepted +IsNoPlay, data = fp_cleaned,
         family = binomial(link = "logit"))
AIC(m9) #906.9086 #larger than model 8

m10 = glm(score_bin~YardLine + SeriesFirstDown + Yards +IsPenalty +
           PenaltyYards +ToGo+PassType+IsPenaltyAccepted +IsNoPlay + RushDirection, data = fp_cleaned,
         family = binomial(link = "logit"))
AIC(m10) #905.0016 #Both this an the previous are larger than model 8

final_model = glm(score_bin~YardLine + SeriesFirstDown + Yards +IsPenalty +
                    PenaltyYards +ToGo+PassType+IsPenaltyAccepted, data = fp_cleaned,
                  family = binomial(link = "logit"))
summary(final_model)
