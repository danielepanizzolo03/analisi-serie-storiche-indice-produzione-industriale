# source("Eserc2_2425.R")

# setwd("c:/discod/didattica/serie storiche/ss2425/esercitazioni_R/esercitazione_2/")

# carico libreria "sse"
source('/Users/danielepanizzolo/Downloads/SEI/Terzo anno/Primo Semestre/Serie Storiche/Lab1/sse_v4.txt')


###########################################################
#  LA SERIE MENSILE DELL'INDICE DELLA PRODUZIONE INDUSTRIALE
###########################################################


# carico il file con i dati
y=scan("indprod.txt")
plot(y,type="l")

# Ora carico la serie come oggetto della classe ts 
# Questo permette di definire l'inizio e la fine del periodo
# a cui si riferiscono i dati e anche la loro frequenza.
# E' una serie mensile quindi metto frequency=12

y=ts(scan("indprod.txt"),start=c(1990,1),end=c(2020,8),frequency=12) #con ts R riconosce le date e la frequenza
head(y)
n=length(y)
n
y
# att.ne: y *sembra* una matrice ma non è un matrice

# se faccio il grafico, sull'asse delle x c'è il tempo riferito ai mesi giusti

plot(y)
tt=seq(1,n,1)
tt=ts(tt,start=c(1990,1),end=c(2020,8),frequency=12)

# per fare il box-plot mensile metto i dati in una matrice con gli 
# anni nelle righe e i mesi nelle colonne. Dato che mancano gli ultimi 
# 4 mesi del 2020 aggiungo 4 NA.
ymat=matrix(c(y,NA,NA,NA,NA),nrow=31,ncol=12, byrow=T)
# boxplot delle distribuzioni dei 12 mesi
boxplot(ymat, col=rainbow(12))


# Ora seleziono la sottoserie fino a novembre 2011
x=window(y, start=c(1990,1), end=c(2008,11))
plot(x)
n=length(x)

# rifaccio il box-plot mensile sulla sottoserie
xmat=matrix(c(y,NA),nrow=19,ncol=12, byrow=T)
# boxplot delle distribuzioni dei 12 mesi
boxplot(xmat, col=rainbow(12))

## provo a stimare un semplice trend lineare mediante 
## una regressione lineare sul tempo **per gli anni 1990-2008**

# costruisco la variabile che definirà il trend
tempo=seq(1,n,1)   # tempo=1:n

# creo le variabili dummy con il comando make.dummy (liberia sse)
# start indica il mese da cui si parte
# Il comando produce una matrice con le dummy nelle colonne
dm=make.dummy(length(x),start=1, freq=12)
dm[1:6,] 

# regressione lineare con TREND LINEARE 
modx=lm(x~-1+tempo+dm[,1]+dm[,2]+dm[,3]+dm[,4]+dm[,5]+dm[,6]+dm[,7]+
        dm[,8]+dm[,9]+dm[,10]+dm[,11]+dm[,12])
summary(modx)

# valori stimati
x.stim=ts(modx$fitted.values,start=c(1990,1), end=c(2008,11),frequency=12)

# grafico componente di trend
plot(x)
# considero la media dei coeff stagionali grezzi e la parte dipendente dal tempo
Trend=mean(modx$coef[2:13])+modx$coef[1]*(1:n)
Trend=ts(Trend, start=c(1990,1), end=c(2008,11),frequency=12)
lines(Trend, col="red")

# grafico valori veri e stimati
plot(x)
lines(x.stim, col="red")

# grafico residui
residui=ts(modx$residuals,start=c(1990,1), end=c(2008,11),frequency=12)
plot(residui, ylim=c(-13,11))
abline(h=0)

## proviamo a considerare un TREND QUADRATICO
tempo2=tempo^2
mod2x=lm(x~-1+tempo+tempo2+dm[,1]+dm[,2]+dm[,3]+dm[,4]+dm[,5]+dm[,6]+dm[,7]+dm[,8]+dm[,9]+dm[,10]+dm[,11]+dm[,12])
summary(mod2x)

# valori stimati
x.stim=ts(mod2x$fitted.values,start=c(1990,1), end=c(2008,11),frequency=12)
# residui del modello
residui2=ts(mod2x$residuals,start=c(1990,1), end=c(2008,11),frequency=12)

# grafico valori veri e stimati
plot(x)
lines(x.stim, col="red")

# grafico residui
plot(residui2, ylim=c(-13,11))
abline(h=0)

# proviamo a considerare un TREND CUBICO
tempo3=tempo^3

mod3x=lm(x~-1+tempo+tempo2+tempo3+dm[,1]+dm[,2]+dm[,3]+dm[,4]+dm[,5]+dm[,6]+dm[,7]+dm[,8]+dm[,9]+dm[,10]+dm[,11]+dm[,12])
summary(mod3x)
# variabili non significative

# Confronto R2 ed R2corr
R2.1=summary(modx)$r.squared
R2cor.1=summary(modx)$adj.r.squared
R2.2=summary(mod2x)$r.squared
R2cor.2=summary(mod2x)$adj.r.squared
R2.3=summary(mod2x)$r.squared
R2cor.3=summary(mod3x)$adj.r.squared
c(R2.1,R2.2, R2.3)
c(R2cor.1,R2cor.2,R2cor.3)

# Decidiamo di andare avanti con un trend lineare

# stima della componente stagionale
CG=modx$coef[2:13] # coefficienti grezzi
CI=CG-mean(CG)       # coefficienti ideali
CI.vettore=rep(CI,31)  # replico per il numero di anni
CI.vettore=CI.vettore[1:length(x)] # taglio perché l'ultimo anno non è intero
S.hat=ts(CI.vettore,start=c(1990,1), end=c(2008,11),frequency=12)
plot(S.hat)

# stima del trend
T.hat=mean(CG)+modx$coef[1]*tempo
T.hat=ts(T.hat,start=c(1990,1), end=c(2008,11),frequency=12)
plot(x)
lines(T.hat, col="red")


## analisi della serie storica completa....

plot(y)    # sembra esserci un cambio di trend

# potenze del tempo
tempo=ts(seq(1,length(y),1))
tempo2=tempo^2

# ricostruisco dummy sulla serie intera
dm=make.dummy(length(y),freq=12, start=1)

# Proviamo a ristimare lo stesso modello su tutta la serie
# (ma non funziona)
mody=lm(y~-1+tempo+dm[,1]+dm[,2]+dm[,3]+dm[,4]+dm[,5]+dm[,6]+dm[,7]+dm[,8]+dm[,9]+dm[,10]+dm[,11]+dm[,12])
summary(mody)

# valori stimati
y.stim=ts(mody$fitted.values,start=c(1990,1), end=c(2020,8),frequency=12)

#residui del modello
residui=ts(mody$residuals,start=c(1990,1), end=c(2020,8),frequency=12)

# grafico dei valori veri e stimati
# non va bene!
plot(y)
lines(y.stim, col="red")

# grafico dei residui
plot(residui)
abline(h=0)

# proviamo a considerare due variabili di trend, una che descrive un
# trend lineare fino a novembre 2008, e una che descrive un trend 
# costante da dicembre 2008
# 
# tra 01.1990 e 11.2008 ci sono 227 osservazioni
# tra 12.2008 e 08.2020 ci sono 141 osservazioni

tempo1=c(seq(1,227,1),rep(0,141))
tempo.cost=c(rep(0,227), rep(1,141))

# modello con doppio trend, non costante e 12 dummy
mod2y=lm(y~-1+tempo1+tempo.cost+dm[,1]+dm[,2]+dm[,3]+dm[,4]+dm[,5]+dm[,6]+dm[,7]+dm[,8]+dm[,9]+dm[,10]+dm[,11]+dm[,12])
summary(mod2y)

# valori stimati
y.stim=ts(mod2y$fitted.values,start=c(1990,1), end=c(2020,8),frequency=12)

# residui
residui=ts(mod2y$residuals,start=c(1990,1), end=c(2020,8),frequency=12)

# grafico veri-stimati
plot(y)
lines(y.stim, col="red")

# analisi dei residui
# rimane qualcosa di anomalo...
plot(residui)
abline(h=0)

n=length(y)  #368

# primo elto del plot=novembre 2019, 
# ultimo elto agosto 2020
plot(residui[(n-9):n])
# notare l'errore per marzo 2020

covid=rep(0,368)
covid[363:365]=1  # marzo, aprile e maggio che corrispondono alle osservazioni 363,364 e 365 che valgono 1

mod5y=lm(y~-1+tempo1+tempo.cost+covid+dm[,1]+dm[,2]+dm[,3]+dm[,4]+dm[,5]+dm[,6]+dm[,7]+dm[,8]+dm[,9]+dm[,10]+dm[,11]+dm[,12])
summary(mod5y)
y.stim5=ts(mod5y$fitted.values,start=c(1990,1), end=c(2020,8),frequency=12)
residui5=ts(mod5y$residuals,start=c(1990,1), end=c(2020,8),frequency=12)

# grafico veri-stimati
plot(y)
lines(y.stim5, col="red")

# analisi dei residui
plot(residui5)
abline(h=0)

