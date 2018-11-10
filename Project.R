
rm(list=ls())
setwd("D:\\Masters Courses\\Game Analytics\\Assignments\\Project\\DotAlicious")
DotaData = read.csv("removedcolumns.csv")
DotaData = na.omit(DotaData)
DotaData[which(DotaData$GamesPlayed == 0),3] = NA
DotaData = na.omit(DotaData)
write.table(Temp, file = "RemovedZeros.csv", sep=",", row.names = F)

detach(Data)

Data = read.csv("Dota-FinalData.csv")
attach(Data)

Data.frame = data.frame(Data[,names(Data) %in% c(
  "WonRate"         ,
  "GLRate"          ,
  "Ditches"         ,
  "Points"          ,
  "SkillLevel"      ,
  "Reliability"     ,
  "AvgKills"        ,
  "KillsPerMin"     ,
  "AvgDeaths"       ,
  "AvgAssists"      ,
  "LongestWinSteak" ,
  "CurrentWinSteak" ,
  "AvgCK"           ,
  "AvgCD"           ,
  "AvgNK"           ,
  "AvgTD"           ,
  "AvgRD"           ,
  "AvgCouK"         ,
  "AvgCouS",
  "AvgHeroChanged"  ,
  "StatsReset"      ,
  "Posts"           ,
  "Positive.Karma"  ,
  "Negative.Karma"  ,
  "Referrals",
  "ReferralLinkHits"
  )])


Data.frame = data.frame(Data[,names(Data) %in% c(
  "WonRate"         ,
  "GLRate"          ,
  "Ditches"         ,
  "Points"          ,
  "SkillLevel"      ,
  "AvgKills"        ,
  "AvgDeaths"       ,
  "AvgAssists"      ,
  "LongestWinSteak" ,
  "CurrentWinSteak" ,
  "AvgCK"           ,
  "AvgCD"           ,
  "AvgNK"           ,
  "AvgTD"           ,
  "AvgRD"           ,
  "AvgCouK"         ,
  "AvgCouS",
  "StatsReset"      ,
  "Posts"           ,
  "Positive.Karma"  ,
  "Negative.Karma"  ,
  "Referrals",
  "ReferralLinkHits"
)])

#Forward Selection
min.model = lm(GLRate ~ 1, data=Data.frame)
biggest <- formula(lm(GLRate~., data=Data.frame))
fwd.model = step(min.model, direction='forward', scope=biggest)


summary(lm(GLRate ~ WonRate + AvgNK + SkillLevel + Points + AvgTD + AvgRD + 
             LongestWinSteak + AvgDeaths + AvgCK + StatsReset + AvgCouK + 
             AvgAssists + AvgKills + AvgCD, data = Data.frame))

summary(lm(GLRate ~ poly(WonRate,3) + AvgDeaths + AvgCK, data = Data.frame))


set.seed (2)
train=sample (1: nrow(Data.frame), 26470)
DataFrame.test = Data.frame[-train,]
lm.fit = lm(GLRate ~ poly(WonRate,3) + AvgDeaths + AvgCK 
            , data = Data.frame, subset = train)
lm.predict = predict(lm.fit, newdata = DataFrame.test)
mean((DataFrame.test$GLRate - lm.predict) ^ 2)





summary(lm(GLRate ~ WonRate + AvgDeaths + AvgCK, data = Data.frame))

summary(lm(GLRate ~ AvgHeroChanged + LongestWinSteak + SkillLevel + Ditches + 
             AvgNK + Points + WonRate + StatsReset + AvgDeaths + AvgCK + 
             AvgTD + AvgRD + AvgAssists + CurrentWinSteak + 
             AvgCouS + AvgKills + KillsPerMin + AvgCD + Positive.Karma))

summary(lm(GLRate ~ AvgHeroChanged + LongestWinSteak + SkillLevel + Ditches + 
             AvgNK + Points + WonRate + StatsReset + AvgDeaths + AvgCK + 
             AvgTD + AvgRD + AvgAssists + CurrentWinSteak + 
             AvgCouS + AvgKills + KillsPerMin + AvgCD))

summary(lm(GLRate ~ AvgHeroChanged + LongestWinSteak + SkillLevel + Ditches + 
             AvgNK + Points + WonRate + StatsReset + AvgDeaths + AvgCK + 
             AvgTD + AvgRD  + 
             AvgKills + KillsPerMin))


#Backward on result of forward

step(lm(GLRate ~ WonRate + AvgNK + SkillLevel + Points + AvgTD + AvgRD + 
  LongestWinSteak + AvgDeaths + AvgCK + StatsReset + AvgCouK + 
  AvgAssists + KillsPerMin + AvgKills + AvgCD + Ditches
  ), direction = "backward")

step(lm( GLRate ~ AvgHeroChanged + LongestWinSteak + SkillLevel + Ditches + 
           AvgNK + Points + WonRate + StatsReset + AvgDeaths + AvgCK + 
           AvgTD + AvgRD + AvgCouK + AvgAssists + CurrentWinSteak + 
           AvgCouS + AvgKills + KillsPerMin + AvgCD + Positive.Karma + 
           Posts), direction = "backward")


min.model = lm(GamesLeft ~ 1, data=Data)
biggest <- formula(lm(GamesLeft~.-PlayerCount-Position-TotalTime-AvgTime,Data))
fwd.model = step(min.model, direction='forward', scope=biggest)

summary (lm(GamesLeft ~ GamesPlayed + Ditches + Reliability + HerosPlayed + 
              GamesWon + LongestWinSteak + CurrentWinSteak + TowersDestroyed + 
              RaxsDestroyed + SkillLevel + Points + WonRate + Referrals + 
              NeutralsKilled + CouriersKilled + StatsReset + ReferralLinkHits + 
              AvgNK + Posts + Positive.Karma + Assists + AvgAssists))

#Scaling for have meaningful result

ProportionOfGamesLeft = GamesLeft/GamesPlayed
ProportionOfGamesLeft[1:10]

min.model = lm(ProportionOfGamesLeft ~ 1, data=Data)
biggest <- formula(lm(ProportionOfGamesLeft~.-PlayerCount-Position-TotalTime-AvgTime-GamesLeft-GamesPlayed,Data))
fwd.model = step(min.model, direction='forward', scope=biggest)


summary (lm(ProportionOfGamesLeft ~ Reliability + HerosPlayed + CurrentWinSteak + 
  LongestWinSteak + Ditches + TowersDestroyed + NeutralsKilled + 
  StatsReset + SkillLevel + CouriersShared))


summary (lm(ProportionOfGamesLeft ~ Reliability + HerosPlayed + CurrentWinSteak + 
              LongestWinSteak + Ditches + TowersDestroyed + NeutralsKilled + 
              StatsReset))


summary (lm(ProportionOfGamesLeft ~ Reliability + HerosPlayed + CurrentWinSteak + 
              LongestWinSteak + Ditches + TowersDestroyed + NeutralsKilled))
AvgTime[AvgTime ==  "AvgTimeNull"]

summary (lm(ProportionOfGamesLeft ~ Reliability))

ProportionOfGamesLeft[1:10]
RoundedPorportion[1:10] = round(ProportionOfGamesLeft, digits = 2)
table(RoundedPorportion[RoundedPorportion!=Reliability])


min.model = lm(ProportionOfGamesLeft ~ 1, data=Data)
biggest <- formula(lm(ProportionOfGamesLeft~.-PlayerCount-Position-TotalTime-AvgTime-GamesLeft-GamesPlayed-Reliability,Data))
fwd.model = step(min.model, direction='forward', scope=biggest)


summary (lm(ProportionOfGamesLeft ~ WonRate + AvgNK + HerosPlayed + NeutralsKilled + 
  Points + AvgTD + AvgRD + CreepsKilled + 
  StatsReset + AvgAssists + TowersDestroyed +
  SkillLevel + Assists + GamesWon, scale = TRUE))


#Running PCA
DataMatrix = data.frame(Data[,!names(Data) %in% c(
  "PlayerCount","Position","TotalTime","AvgTime")])
pca1 <- princomp(Data.frame, scores=TRUE, cor=TRUE)
summary(pca1)
screeplot(pca1, type="line", main="Scree Plot")

pca1var = pca1$sdev^2
pve1 = pca1var/sum(pca1var)
plot(pca1var[1:10], xlab="Principal Component", ylab ="Variance",type ='b')
plot(pve1[1:10], xlab="Principal Component", ylab ="Proportion of Variance Explained",type ='b')
plot(cumsum (pve1), xlab=" Principal Component", ylab ="Cumulative Proportion of Variance Explained", ylim=c(0,1) ,type ='b')


pc1 = pca1$scores[,1]
pc2 = pca1$scores[,2]
pc3 = pca1$scores[,3]
pc4 = pca1$scores[,4]
pc5 = pca1$scores[,5]

summary(lm(GLRate~pc1+pc2+pc3+pc4+pc5, Data=Data.frame))

F = scale(DataMatrix, center = TRUE, scale = TRUE)

DT = data.frame(F)

min.model = lm(GamesLeft ~ 1, data=DT)
biggest <- formula(lm(GamesLeft~.,DT))
fwd.model = step(min.model, direction='forward', scope=biggest)




summary(lm(GLRate ~ poly(AvgHeroChanged,4) + LongestWinSteak + SkillLevel + Ditches + 
             AvgNK + Points + WonRate + StatsReset + AvgDeaths + AvgCK + 
             AvgTD + AvgRD  + AvgKills + KillsPerMin))


fit1 = lm(GLRate ~ AvgHeroChanged + LongestWinSteak + SkillLevel + Ditches + 
            AvgNK + Points + WonRate + StatsReset + AvgDeaths + AvgCK + 
            AvgTD + AvgRD  + AvgKills + KillsPerMin)

fit2 = lm(GLRate ~ poly(AvgHeroChanged,3) + LongestWinSteak + SkillLevel + Ditches + 
            AvgNK + Points + WonRate + StatsReset + AvgDeaths + AvgCK + 
            AvgTD + AvgRD  + AvgKills + KillsPerMin)

fit3 = lm(GLRate ~ poly(AvgHeroChanged,5) + LongestWinSteak + SkillLevel + Ditches + 
            AvgNK + Points + WonRate + StatsReset + AvgDeaths + AvgCK + 
            AvgTD + AvgRD  + AvgKills + KillsPerMin)

fit4 = lm(GLRate ~ poly(AvgHeroChanged,10) + LongestWinSteak + SkillLevel + Ditches + 
            AvgNK + Points + WonRate + StatsReset + AvgDeaths + AvgCK + 
            AvgTD + AvgRD  + AvgKills + KillsPerMin)

fit5 = lm(GLRate ~ poly(AvgHeroChanged,32) + LongestWinSteak + SkillLevel + Ditches + 
            AvgNK + Points + WonRate + StatsReset + AvgDeaths + AvgCK + 
            AvgTD + AvgRD  + AvgKills + KillsPerMin)

anova(fit1,fit2,fit3,fit4,fit5)



summary(lm(GLRate ~ WonRate + AvgNK + SkillLevel + Points + AvgTD + AvgRD + 
          LongestWinSteak + AvgDeaths + AvgCK + StatsReset + AvgCouK + 
          AvgAssists + KillsPerMin + AvgKills, data = Data.frame))


summary(lm(GLRate ~ WonRate + AvgNK + SkillLevel + Points + AvgTD + AvgRD + 
             AvgDeaths + AvgCK, data = Data.frame))



fit1 = lm(GLRate ~ WonRate + AvgNK + SkillLevel + Points + AvgTD + AvgRD + 
            LongestWinSteak + AvgDeaths + AvgCK + StatsReset + AvgCouK + 
            AvgAssists + KillsPerMin + AvgKills)

fit2 = lm(GLRate ~ poly(WonRate,2) + AvgNK + SkillLevel + Points + AvgTD + AvgRD + 
            LongestWinSteak + AvgDeaths + AvgCK + StatsReset + AvgCouK + 
            AvgAssists + KillsPerMin + AvgKills)

fit3 = lm(GLRate ~ poly(WonRate,21) + AvgNK + SkillLevel + Points + AvgTD + AvgRD + 
            LongestWinSteak + AvgDeaths + AvgCK + StatsReset + AvgCouK + 
            AvgAssists + KillsPerMin + AvgKills)

fit4 = lm(GLRate ~ poly(WonRate,4) + AvgNK + SkillLevel + Points + AvgTD + AvgRD + 
            LongestWinSteak + AvgDeaths + AvgCK + StatsReset + AvgCouK + 
            AvgAssists + KillsPerMin + AvgKills)

anova(fit1,fit2,fit3,fit4)


fit1 = lm(GLRate ~ poly(WonRate,4) + AvgNK + SkillLevel + Points + AvgTD + AvgRD + 
            LongestWinSteak + AvgDeaths + AvgCK + StatsReset + AvgCouK + 
            AvgAssists + KillsPerMin + AvgKills)

fit2 = lm(GLRate ~ poly(WonRate,4) + poly(AvgNK,2) + SkillLevel + Points + AvgTD + AvgRD + 
            LongestWinSteak + AvgDeaths + AvgCK + StatsReset + AvgCouK + 
            AvgAssists + KillsPerMin + AvgKills)
fit4= lm(GLRate ~ poly(WonRate,4) + poly(AvgNK,15) + SkillLevel + Points + AvgTD + AvgRD + 
            LongestWinSteak + AvgDeaths + AvgCK + StatsReset + AvgCouK + 
            AvgAssists + KillsPerMin + AvgKills)
anova(fit1,fit2,fit4)


summary(lm(GLRate ~ poly(WonRate,4) + poly(AvgNK,15) + SkillLevel + Points + AvgTD + AvgRD + 
     LongestWinSteak + AvgDeaths + AvgCK + StatsReset + AvgCouK + 
     AvgAssists + KillsPerMin + AvgKills))

#Tree
library(tree)
tree.lefter=tree(GLRate~., data = Data.frame)
plot(tree.lefter)
text(tree.lefter, pretty = 0)
summary(tree.lefter)

cv.tree.lefter = cv.tree(tree.lefter)


set.seed (2)
train=sample (1: nrow(Data.frame), 26470)
DataFrame.test = Data.frame[-train,]

tree.lefter.train=tree(GLRate~., data = Data.frame, subset = train)
plot(tree.lefter.train)
text(tree.lefter.train, pretty = 0)
cv.tree.lefter.train = cv.tree(tree.lefter.train)
plot(cv.tree.lefter.train$size ,cv.tree.lefter.train$dev ,type='b')

prune.train =prune.tree(tree.lefter.train ,best =2)
plot(prune.train)
text(prune.train ,pretty =0)

yhat=predict (tree.lefter.train ,newdata =Data.frame[-train,])
tree.test=Data.frame[-train ,"GLRate"]
plot(yhat ,tree.test)
abline (0,1)
mean((yhat-tree.test)^2)

yhat = predict(tree.lefter.train, data = Data.frame[!train])
mean((yhat-DataFrame.test$GLRate)^2)
length(yhat)
length(DataFrame.test$GLRate)
length(DataFrame.test)
dim(DataFrame.test)
?predict()


#Classification

k.out = kmeans(Data.frame$GLRate,2, nstart = 2)
Y = k.out$cluster-1


min.model = glm(Y~1 ,data=Data.frame, family = binomial)
biggest <- formula(glm(Y~.-GLRate ,data=Data.frame, family = binomial))
fwd.model = step(min.model, direction='forward', scope=biggest)



summary(glm(Y ~ AvgKills + LongestWinSteak + AvgNK + WonRate + AvgRD + CurrentWinSteak + 
              StatsReset + Points + Ditches + SkillLevel + AvgTD + Negative.Karma + 
              AvgAssists + AvgCD, family = binomial, data = Data.frame))

LeftClass = rep("False", 52941)
LeftClass[Data$GLRate>0.5] = "True"
LeftClass[Data$GLRate>0.33] = "MED"
LeftClass[Data$GLRate>0.66] = "HIGH"
Data.frame["LeftClass"] <- LeftClass


cor(Data$Deaths, Data$Willingness)

library(MASS)



set.seed (2)
train=sample (1: nrow(Data.frame), 26470)
DataFrame.test = Data.frame[-train,]

lda.fit=lda(LeftClass~.-GLRate ,data=Data.frame, subset = train)

lda.pred=predict(lda.fit, newdata = DataFrame.test)
length(lda.pred$class)
dim(DataFrame.test)

table(lda.pred$class, DataFrame.test$LeftClass)


library(tree)
set.seed(1)
tree.class=tree(LeftClass~.-GLRate ,data = Data.frame)
summary(tree.class)
plot(tree.class)
text(tree.class, pretty = 0)
cvTree = cv.tree(tree.class, FUN=prune.misclass)




