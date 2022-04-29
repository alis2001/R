italySubset=vaccinations[vaccinations$location=="Italy",]



bb=vaccinations$location=="Italy"
italySubset=vaccinations[bb,]
italySubset=vaccinations[vaccinations$location=="Italy",]

avg=mean(italySubset$daily_vaccinations_raw,na.rm=TRUE)
std=sd(italySubset$daily_vaccinations_raw,na.rm=TRUE)

variance=sum((italySubset$daily_vaccinations_raw-mean(italySubset$daily_vaccinations_raw,na.rm=TRUE))^2,na.rm=TRUE)/length(!is.na(italySubset$daily_vaccinations_raw))
std=sqrt(variance)                                                                                                            
quant=quantile(italySubset$daily_vaccinations_raw,probs=c(0,0.25,0.5,0.75,1),na.rm=TRUE)
quant=quantile(italySubset$daily_vaccinations_raw,probs=seq(from=0,to=1,by=0.01),na.rm=TRUE)
