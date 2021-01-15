### af 126  ###

library(metafor)

data2<-read.table("data2_aula - data2_aula.csv", header=T, sep=",")
data2
attach(data2)

### Modelo aleat??rio ###

rma<-rma(yi=d, vi=vd, data=data2, vtype="UB",
         method="REML", weighted=TRUE, knha=FALSE, level=95, digits=4)
forest(rma, slab = paste(data$sp, sep = ", "))
funnel(rma)
radial(rma)
qqnorm(rma)
inf_rma<-influence(rf1)
plot(inf_rma, plotdfb = TRUE)
regtest_rma<-regtest(rf1)
regtest_rma
fsn(yi=d, vi=vd, data = data2, type="Rosenthal")
fsn(yi=d, vi=vd, data = data2, type="Rosenberg")
fsn(yi=d, vi=vd, data = data2, type="Orwin")
rtf_rma<-trimfill(rma)
rtf_rma
cumul_rma<-cumul(rma)
cumul_rma
rstudent_rma<-rstudent(rma)
rstudent_rma

### Moderators ###

### rma(yi=y, vi=v, mods= ~ factor(ms), measure="GEN",intercept=FALSE, data=af, vtype="UB", method="REML", weighted=TRUE, knha=FALSE, level=95, digits=4) ###



### moderadores com dados categ??ricos ###
### rodar modelo com intercepto para obter valor e n??vel de signific??ncia da heterogeneidade ###

rma_ms<-rma(yi=d, vi=vd, mods= ~ factor(ms), data=data2, vtype="UB", method="REML", weighted=TRUE, knha=FALSE, level=95, digits=4)
confint(rma_ms)
rma_ms

### rodar modelo sem intercepto para obter valor e n??vel de signific??ncia do tamanho do efeito para cada categoria de um moderador ###

rma_ms_1<-rma(yi=d, vi=vd, mods= ~ factor(ms) -1, data=data2, vtype="UB", method="REML", weighted=TRUE, knha=FALSE, level=95, digits=4)
confint(rma_ms_1)
rma_ms_1
forest(rma_ms_1)

### moderadores com dados cont??nuos ###

rma_log<-rma(yi=d, vi=vd, mods= ~ (log), data=data2)
confint(rma_log)
rma_log

### intera????o entre moderadores ###

rma_ms_lat<-rma(yi=d, vi=vd, mods= ~ factor(ms)*lat, data=data2, vtype="UB", method="REML", weighted=TRUE, knha=FALSE, level=95, digits=4)
confint(rma_ms_lat)
rma_ms_lat
summary(rma_ms_lat)

rma_ms_lat_1<-rma(yi=d, vi=vd, mods= ~ factor(ms)*lat -1, data=data2, vtype="UB", method="REML", weighted=TRUE, knha=FALSE, level=95, digits=4)
confint(rma_ms_lat_1)
rma_ms_lat_1


