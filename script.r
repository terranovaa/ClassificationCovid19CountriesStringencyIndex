##### PREPROCESSING #####
# covid = read.csv("owid-covid-data.csv")
# covidReduced = covid[covid$date=="2021-09-01",]
# subsetCovid<-subset(covidReduced, (!is.na(covidReduced$stringency_index)) & (!is.na(covidReduced$people_fully_vaccinated)) & (!is.na(covidReduced$new_deaths))& (!is.na(covidReduced$reproduction_rate)) & (!is.na(covidReduced$new_cases)) & (!is.na(covidReduced$people_vaccinated)) & (!is.na(covidReduced$total_cases)) & (!is.na(covidReduced$total_deaths)))
# events = read.csv("public-events-covid.csv")
# eventsReduced = events[events$Day=="2021-09-01",]
# eventsReduced = eventsReduced[, c(1,4)]
# table = subsetCovid[,c(3,5,6,8,9,17,36,37,48,49)]
# colnames(table)[1] <- "Entity"
# Merge con le informazioni contenute nella tabella relativa alle restrizioni degli eventi
# table = merge(table, eventsReduced, by = "Entity")
# table <- data.frame(table[,-1], row.names = table[,1])
# Scarto le osservazioni relative alla classe "No measures", vista la presenza di un solo Stato appartenente a questa classe
# table =  table[-which(table$cancel_public_events == 0),]
# Cambio l'etichetta associata alla classe "Required cancellations"
# table$cancel_public_events[which(table$cancel_public_events == 2)] = 0
# colnames(table)[10] <- "public_events"
#########################

table = read.csv("tabella.csv")
table <- data.frame(table[,-1], row.names = table[,1])
# Standardizzo la tabella
table[,1:9] = scale(table[,1:9])

plot(table, pch=20, col=c("indianred3","sandybrown"))
barplot(table(table$public_events), col=c("indianred3","sandybrown")) # dataset abbastanza bilanciato
source("s2_cmroc.r")

# Regressione lineare classica
# Rileggiamo le classi in termini di +-1 ai fini dell'implementazione del modello regressivo elementare
tablelr = table
tablelr$public_events = 2*tablelr$public_events-1
tablelr.lm=lm(public_events~., data = tablelr)
sum((predict(tablelr.lm)>0) ==(tablelr$public_events > 0))/length(tablelr$public_events)
response = (tablelr$public_events>0)
predictor = (predict(tablelr.lm)>0)
s2_confusion(response,predictor)

# Regressione logistica
table.glm =glm(public_events ~., data=table, family=binomial)
table.glm.p = predict(table.glm, type="response")
sum((table.glm.p>0.5) ==(table$public_events>0.5))/length(table$public_events)
s2_confusion(table$public_events, table.glm.p)
table.glm.roc = s2_roc(table$public_events, table.glm.p) 
s2_roc.plot(table.glm.roc)
s2_auc(table.glm.roc)

# Analisi discriminante lineare
library(MASS)
table.lda=lda(public_events ~.,data=table,prior=c(1/2,1/2))
table.lda.values=predict(table.lda)
table.lda.post = table.lda.values$posterior[,2] 
sum(table$public_events  == table.lda.values$class)/length(table$public_events )
s2_confusion(table$public_events, table.lda.post)
table.lda.roc = s2_roc(table$public_events, table.lda.post) 
s2_roc.plot(table.lda.roc)
s2_auc(table.lda.roc)

# Analisi discriminante quadratica
table.qda=qda(public_events ~.,data=table,prior=c(1/2,1/2))
table.qda.values=predict(table.qda)
table.qda.post = table.qda.values$posterior[,2] 
sum(table$public_events  == table.qda.values$class)/length(table$public_events )
s2_confusion(table$public_events, table.qda.post)
table.qda.roc = s2_roc(table$public_events, table.qda.post) 
s2_roc.plot(table.qda.roc)
s2_auc(table.qda.roc)

# Curve ROC
s2_roc.plot(table.qda.roc, col="green3")
s2_roc.lines(table.lda.roc, col="blue")
s2_roc.lines(table.glm.roc, col="red")
legend("bottomright",legend = c("glm", "lda", "qda"),col = c("red", "blue", "green3"),lwd=2)

# Analisi della robustezza dei modelli
idx=sample(95,95)
acclm=rep(0,95)
accglm=rep(0,95)
acclda=rep(0,95)
accqda=rep(0,95)
for(i in 1:95){
  tablef=table
  tableflr=tablelr
  tableflr$public_events[idx[1:i]]=-tableflr$public_events[idx[1:i]]
  for(j in 1:i){
    if(tablef$public_events[idx[j]]==0)
      tablef$public_events[idx[j]] = 1
    else
      tablef$public_events[idx[j]] = 0
  }
  tableflr.lm=lm(public_events~.,data=tableflr)
  acclm[i]=sum((predict(tableflr.lm)>0)==(tablelr$public_events>0))/length(tablelr$public_events)
  tablef.glm =glm(public_events ~., data=tablef, family=binomial)
  accglm[i] = sum((predict(tablef.glm)>0.5) ==(table$public_events>0.5))/length(table$public_events)
  tablef.lda=lda(public_events ~.,data=tablef,prior=c(1/2,1/2))
  tablef.lda.values=predict(tablef.lda)
  tablef.lda.post = tablef.lda.values$posterior[,2]
  acclda[i] = sum(table$public_events  == tablef.lda.values$class)/length(table$public_events )
  tablef.qda=qda(public_events ~.,data=tablef,prior=c(1/2,1/2))
  tablef.qda.values=predict(tablef.qda)
  tablef.qda.post = tablef.qda.values$posterior[,2] # probabilità delle classificazioni
  accqda[i] = sum(table$public_events  == tablef.qda.values$class)/length(table$public_events )
}
plot(acclm,type="l", col="black",xlab="Valori cambiati", ylab="Accuratezza", ylim=c(0.12,0.9))
lines(accglm,type="l", col="red")
lines(acclda,type="l", col="blue")
lines(accqda,type="l", col="green3")
legend("bottomleft",legend = c("lm", "glm", "lda", "qda"),col = c("black", "red", "blue", "green3"),lwd=2)

# Autovalutazione
l=length(table$public_events)
acc=matrix(0,10,4)
spec=matrix(0,10,4)
n = 10
for(i in 1:n){
  idx=sample(l,n)
  tablecv=table[-idx,]
  tablelrcv=table[-idx,]
  tablelrcv.lm=lm(public_events~., data = tablelrcv)
  predictor=(predict(tablelrcv.lm,table[idx,])>0)
  response=(tablelr$public_events[idx]>0)
  conf = s2_pconfusion(response,predictor)
  spec[i,1] = conf$`actual 0`[2]/sum(conf$`actual 0`)
  acc[i,1]=sum(predictor==response)/n
  table.glm=glm(public_events~.,family=binomial,data=tablecv)
  table.glm.p=predict(table.glm,table[idx,],type="response")
  conf = s2_confusion(table$public_events[idx], table.glm.p)
  spec[i,2] = conf$`actual 0`[2]/sum(conf$`actual 0`)
  acc[i,2]=sum((table.glm.p>0.5)==(table$public_events[idx]>0.5))/n
  table.lda=lda(public_events~.,data=tablecv)
  table.lda.p=predict(table.lda,table[idx,])$posterior[,2]
  conf = s2_confusion(table$public_events, table.lda.post)
  spec[i,3] = conf$`actual 0`[2]/sum(conf$`actual 0`)
  acc[i,3]=sum((table.lda.p>0.5)==(table$public_events[idx]>0.5))/n
  table.qda=qda(public_events~.,data=tablecv)
  table.qda.p=predict(table.qda,table[idx,])$posterior[,2]
  conf = s2_confusion(table$public_events, table.qda.post)
  spec[i,4] = conf$`actual 0`[2]/sum(conf$`actual 0`)
  acc[i,4]=sum((table.qda.p>0.5)==(table$public_events[idx]>0.5))/n
}
# Regressione lineare
mean(acc[,1])
sd(acc[,1])
hist(acc[,1])
mean(spec[,1])
# Regressione logistica
mean(acc[,2])
sd(acc[,2])
hist(acc[,2])
mean(spec[,2])
# Analisi Discriminante Lineare
mean(acc[,3])
sd(acc[,3])
hist(acc[,3])
mean(spec[,3])
# Analisi Discriminante Quadratica
mean(acc[,4])
sd(acc[,4])
hist(acc[,4])
mean(spec[,4])

# Trade-off tra accuratezza e specificità
l=length(table$public_events)
acc=matrix(0,50,3)
spec=matrix(0,50,3)
i = 1
for(p in seq(0.5,0.99,0.01)){
  table.lda=lda(public_events~.,data=table)
  table.lda.p=predict(table.lda,table)$posterior[,2]
  conf = s2_confusion(table$public_events, table.lda.post,p)
  spec[i,1] = conf$`actual 0`[2]/sum(conf$`actual 0`)
  acc[i,1]=sum((table.lda.p>p)==(table$public_events>p))/length(table$public_events)
  table.qda=qda(public_events~.,data=tablecv)
  table.qda.p=predict(table.qda,table)$posterior[,2]
  conf = s2_confusion(table$public_events, table.qda.post,p)
  spec[i,2] = conf$`actual 0`[2]/sum(conf$`actual 0`)
  acc[i,2]=sum((table.qda.p>p)==(table$public_events>p))/length(table$public_events)
  table.glm=glm(public_events~.,family=binomial,data=tablecv)
  table.glm.p=predict(table.glm,table,type="response")
  conf = s2_confusion(table$public_events, table.glm.p,p)
  spec[i,3] = conf$`actual 0`[2]/sum(conf$`actual 0`)
  acc[i,3]=sum((table.glm.p>p)==(table$public_events>p))/length(table$public_events)
  i = i + 1
}
plot(acc[,1],xaxt = "n",type="l", col="blue",xlab="Soglia di probabilità", ylab="Accuratezza e Specificità",ylim=c(0.6,1))
lines(acc[,2],type="l", col="green3")
lines(acc[,3],type="l", col="red")
lines(spec[,1],type="l", col="blue",lty=9)
lines(spec[,2],type="l", col="green3",lty=9)
lines(spec[,3],type="l", col="red", lty=9)
legend("bottomleft",legend = c("glm", "lda", "qda"),col = c("red", "blue", "green3"),lwd=2)
axis(1, at=1:50, labels=seq(0.5,0.99,0.01))

# Miglior classificatore: LDA con soglia di probabilità pari a 0.72
table.lda=lda(public_events~.,data=table)
table.lda.p=predict(table.lda,table)$posterior[,2]
conf = s2_confusion(table$public_events, table.lda.post,p)
spec = conf$`actual 0`[2]/sum(conf$`actual 0`)
acc = sum((table.lda.p>0.72)==(table$public_events>0.72))/length(table$public_events)
plot(table.lda.values$x, pch=8, col=as.numeric(table$public_events)+1)
points(table.lda.values$x, col=as.numeric(table.lda.p > 0.72)+1)
legend("bottomleft",legend = c("Required Cancellations", "Recommended Cancellations"),col = c("black","red"),lwd=2)

