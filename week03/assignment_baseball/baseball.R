# 1.1 - Reading the data
  baseball=read.csv("baseball.csv")
  str(baseball)
# 1.2 - how many years
  nrow(table(baseball$Year))
  
# 1.3 - Limiting to playoffs
  baseball = subset(baseball, Playoffs==1)
  str(baseball)
  
# 1.4 - playoff-ers per year
  table(baseball$Year)
  
# 2.1 
  PlayoffTable = table(baseball$Year)
  PlayoffTable
  names(PlayoffTable)
# 2.2
  PlayoffTable[c("1990", "2001")]
  
# 2.3 
  baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]
  baseball$NumCompetitors
  str(baseball)

# 2.4 - how many pairs when 8 teams played playoffs?
  table(baseball$NumCompetitors)

# 3.1 - Adding WSChamp
  baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
  table(baseball$WorldSeries)

# 3.2 - building and analyzing 12 models
          mod01 = glm(WorldSeries ~ Year , data= baseball, family=binomial)
  summary(mod01)
          mod02 = glm(WorldSeries ~ RS , data= baseball, family=binomial)
  summary(mod02)
          mod03 = glm(WorldSeries ~ RA , data= baseball, family=binomial)
  summary(mod03)
          mod04 = glm(WorldSeries ~ W , data= baseball, family=binomial)
  summary(mod04)
          mod05 = glm(WorldSeries ~ OBP , data= baseball, family=binomial)
  summary(mod05)
          mod06 = glm(WorldSeries ~ SLG , data= baseball, family=binomial)
  summary(mod06)
          mod07 = glm(WorldSeries ~ BA , data= baseball, family=binomial)
  summary(mod07)
          mod08 = glm(WorldSeries ~ RankSeason , data= baseball, family=binomial)
  summary(mod08)
          mod09 = glm(WorldSeries ~ NumCompetitors , data= baseball, family=binomial)
  summary(mod09)
          mod10 = glm(WorldSeries ~ OOBP , data= baseball, family=binomial)
  summary(mod10)
          mod11 = glm(WorldSeries ~ OSLG , data= baseball, family=binomial)
  summary(mod11)
          mod11 = glm(WorldSeries ~ League , data= baseball, family=binomial)
  summary(mod11)

# 4.1 - Significant together
  model = glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors , data= baseball, family=binomial)
  summary(model)

# 4.2 - finding correlation among them
  cor(baseball[c("Year","RA","RankSeason","NumCompetitors")])
  
# 4.3 - AICs 
  mod09 = glm(WorldSeries ~ NumCompetitors , data= baseball, family=binomial)
  summary(mod09) # 230.96
  mod08 = glm(WorldSeries ~ RankSeason , data= baseball, family=binomial)
  summary(mod08) # 238.75
  mod03 = glm(WorldSeries ~ RA , data= baseball, family=binomial)
  summary(mod03) # 237.88
  mod01 = glm(WorldSeries ~ Year , data= baseball, family=binomial)
  summary(mod01) # 232.35

  xmod01 = glm(WorldSeries ~ Year+RA , data= baseball, family=binomial)
  summary(xmod01) # 233.88
  xmod02 = glm(WorldSeries ~ Year+RankSeason , data= baseball, family=binomial)
  summary(xmod02) # 233.55
  xmod03 = glm(WorldSeries ~ Year+NumCompetitors , data= baseball, family=binomial)
  summary(xmod03) # 232.9
  xmod04 = glm(WorldSeries ~ RA+RankSeason , data= baseball, family=binomial)
  summary(xmod04) # 238.22
  xmod05 = glm(WorldSeries ~ RA+NumCompetitors , data= baseball, family=binomial)
  summary(xmod05) # 232.74
  xmod06 = glm(WorldSeries ~ RankSeason+NumCompetitors , data= baseball, family=binomial)
  summary(xmod06) # 232.52
  