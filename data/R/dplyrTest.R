library(dplyr)

Caserta=dataset %>%
  select(sub_region_1,sub_region_2,retail_and_recreation)%>%
  filter(sub_region_2=="Province of Caserta")
  


CasertaSum=dataset %>%
  select(sub_region_1,sub_region_2,retail_and_recreation)%>%
  filter(sub_region_2=="Province of Caserta") %>%
  summarise(minim=min(retail_and_recreation),media=mean(retail_and_recreation),mediana=median(retail_and_recreation))

CasertaSum=Caserta %>%
  summarise(minim=min(retail_and_recreation),media=mean(retail_and_recreation),mediana=median(retail_and_recreation),stDev=sd(retail_and_recreation),var(retail_and_recreation))




RetSum=dataset %>%
    filter(sub_region_2!="") %>%
    group_by(sub_region_2) %>%   
    summarise(minim=min(retail_and_recreation),media=mean(retail_and_recreation),mediana=median(retail_and_recreation))




datasetItaly=dataset %>%
    filter(sub_region_1=="")

datasetCaserta=dataset %>%
  filter(sub_region_2=="Province of Caserta")

plot(100:200,datasetCaserta$retail_and_recreation[100:200])
plot(datasetCaserta$parks)

plot(datasetItaly$retail_and_recreation[200:275])
hist(datasetCaserta$retail_and_recreation)
hist(datasetItaly$retail_and_recreation[200:275])

bounds=c(-97,-50,-20,0,8)
hist(datasetCaserta$retail_and_recreation,breaks=bounds)

hist(datasetCaserta$retail_and_recreation,breaks=10,freq=FALSE)
boxplot(datasetCaserta$retail_and_recreation,datasetCaserta$transit_stations)

avgCas=mean(datasetCaserta$retail_and_recreation[200:275])
avgItaly=mean(datasetItaly$retail_and_recreation[200:275])
medianCas=median(datasetCaserta$retail_and_recreation[200:275]) 
medianItaly=median(datasetItaly$retail_and_recreation[200:275])
quantCas=quantile(datasetCaserta$retail_and_recreation[200:275])
quantItaly=quantile(datasetItaly$retail_and_recreation[200:275])
boxplot(datasetItaly$retail_and_recreation,datasetCaserta$retail_and_recreation)

summaryCaserta=datasetCaserta %>%
  summarise(mini=min(retail_and_recreation),avg=mean(retail_and_recreation),med=median(retail_and_recreation),max=max(retail_and_recreation))


summaryProvince=dataset %>%
  filter(sub_region_2!="") %>%
  group_by(sub_region_2) %>%
  summarise(mini=min(retail_and_recreation),avg=mean(retail_and_recreation),med=median(retail_and_recreation),max=max(retail_and_recreation))




minParksCaserta=min(datasetCaserta$parks, na.rm=TRUE)
maxParksCaserta=max(datasetCaserta$parks, na.rm=TRUE)
tagli=seq(minParksCaserta,maxParksCaserta,20)
tag=cut(datasetCaserta$parks,tagli)

minRRCaserta=min(datasetCaserta$retail_and_recreation, na.rm=TRUE)
maxRRCaserta=max(datasetCaserta$retail_and_recreation, na.rm=TRUE)
tagli2=seq(minRRCaserta,maxRRCaserta,20)
tag2=cut(datasetCaserta$retail_and_recreation,tagli2)

chi=chisq.test(tag,tag2)
sqrt(chi$statistic/(275*4))

tab=table(tag,tag2, useNA='no')
install.packages("gplots")
library(gplots)
balloonplot(t(tab),label = FALSE, show.margins = FALSE)









coef=lm(datasetCaserta$retail_and_recreation ~ datasetCaserta$transit_stations, data = datasetCaserta)



plot(datasetCaserta$transit_stations,datasetCaserta$retail_and_recreation)
abline(coef)

plot(datasetCaserta$transit_stations,coef$fitted.values)
errors=datasetCaserta$retail_and_recreation-coef$fitted.values
errors
plot(datasetCaserta$transit_stations,coef$residuals)
plot(datasetCaserta$transit_stations,errors)
hist(errors)
var(errors)
sd(errors)
mean(errors)



coef2=lm(datasetCaserta$retail_and_recreation ~ datasetCaserta$workplaces, data = datasetCaserta)


plot(datasetCaserta$workplaces,datasetCaserta$retail_and_recreation)
abline(coef2)

plot(datasetCaserta$parks)
errors=datasetCaserta$retail_and_recreation-coef2$fitted.values
errors
plot(datasetCaserta$workplaces,coef2$residuals)
plot(datasetCaserta$workplaces,errors)
hist(errors)
var(errors)
sd(errors)
mean(errors)
R2=(cor(datasetCaserta$retail_and_recreation,datasetCaserta$workplaces))^2
R2

meanCampania=dataset %>%
  filter(sub_region_1=="Campania") %>%
  filter(sub_region_2!="") %>%
  summarise(n=n(),avgGeneral=mean(retail_and_recreation))

devTotCampania=dataset %>%
  filter(sub_region_1=="Campania") %>%
  filter(sub_region_2!="") %>%
  mutate(Sqdiff=(retail_and_recreation-meanCampania$avgGeneral)^2) %>%
  summarise(devTot=sum(Sqdiff))
  

DevCampania=dataset %>%
  filter(sub_region_1=="Campania") %>%
  filter(sub_region_2!="") %>%
  mutate(squaredRR=retail_and_recreation^2) %>%
  group_by(sub_region_2) %>%
  summarise(n=n(),avg=mean(retail_and_recreation),sumSQ=sum(squaredRR),SQsum=(avg^2*n()),devProv=sumSQ-SQsum) %>%
  mutate(devbetw=(avg-meanCampania$avgGeneral)^2*n )


Decomp=DevCampania %>%
  summarise(withinDev=sum(devProv),betweenDev=sum(devbetw))


devtot=Decomp$withinDev+Decomp$betweenDev
View(devtot-devTotCampania$devTot)
squaredEta=Decomp$betweenDev/devtot
View(squaredEta)



