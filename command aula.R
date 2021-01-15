library (metafor)

data<-read.table("data_aula.txt", header=T)
data
attach(data)


### The log odds ratio is equal to log of (ai*di)/(bi*ci)  - effect size, variation, CI ###

es<-escalc(measure ="OR", ai=mcfr, bi=mcnf, ci=npfr, di=npnf, data = data, append = TRUE, to="none", add=0, vtype="UB")
es
es$yi
es$vi
mines<-(es$yi - 1.96*sqrt(es$vi))
maxes<-(es$yi + 1.96*sqrt(es$vi))
mines
maxes

shapiro.test(es$yi)
shapiro.test(es$vi)
hist(es$yi)

## Random effects models ##

### res <- rma(ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, measure="RR", slab=paste(author, year, sep=", "), method="REML") ###

rf1<-rma(measure = "OR", ai=mcfr, bi=mcnf, ci=npfr, di=npnf, data = data, method="REML")
confint(rf1)
rf1
forest(rf1, slab = paste(data$sp, sep = ", "))
funnel(rf1)
radial(rf1)
qqnorm(rf1)
inf_rf1<-influence(rf1)
plot(inf_rf1, plotdfb = TRUE)
regtest_rf1<-regtest(rf1)
regtest_rf1
fsn(yi=es$yi, vi=es$vi, data = data, type="Rosenthal")
fsn(yi=es$yi, vi=es$vi, data = data, type="Rosenberg")
fsn(es$yi, vi=es$vi, data = data, type="Orwin")
rtf_rf1<-trimfill(rf1)
rtf_rf1
cumul_rf1<-cumul(rf1)
cumul_rf1
rstudent_rf1<-rstudent(rf1)
rstudent_rf1

## Random effects models with moderators ##


