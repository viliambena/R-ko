# Na�ou ulohou bude vz�jomn� anal�za akciov�ch indexov ktor� zaznamenavaj� profitovanie dan�ho trhu.
# Rozhodli sme sa pre porovnanie trhov vybra� trhy �zijsk�ch a eur�pskych kraj�n.
# Zis�ovali sme vlastnosti vybran�ch �asov�ch radov a pod�a r�znych krit�ri� ur�ovali, 
# ktor� trhy svoj�m spr�van�m ovplyv�uj� ostatn� trhy.

data <- read.csv(file="C://Users//vilia//Desktop//DataFinal.csv", header=TRUE,sep = ";")

attach(data)

library(datasets)
library(urca)
library(Hmisc)
k_max = as.integer(12*(length(DAX)/100)^(1/4))
p_ADF_DAX = punitroot(ur.df(DAX, type = "trend", lags = k_max, selectlags = "AIC")@teststat [1], N = length(DAX), trend = "ct", statistic = "t")
p_ADF_PX = punitroot(ur.df(PX, type = "trend", lags = k_max, selectlags = "AIC")@teststat [1], N = length(DAX), trend = "ct", statistic = "t")
p_ADF_SSEC = punitroot(ur.df(SSEC, type = "trend", lags = k_max, selectlags = "AIC")@teststat [1], N = length(DAX), trend = "ct", statistic = "t")
p_ADF_BUX = punitroot(ur.df(BUX, type = "trend", lags = k_max, selectlags = "AIC")@teststat [1], N = length(DAX), trend = "ct", statistic = "t")
p_ADF_WIG = punitroot(ur.df(WIG, type = "trend", lags = k_max, selectlags = "AIC")@teststat [1], N = length(DAX), trend = "ct", statistic = "t")
p_ADF_NIKKEI = punitroot(ur.df(NIKKEI, type = "trend", lags = k_max, selectlags = "AIC")@teststat [1], N = length(DAX), trend = "ct", statistic = "t")
p_ADF_HS = punitroot(ur.df(HS, type = "trend", lags = k_max, selectlags = "AIC")@teststat [1], N = length(DAX), trend = "ct", statistic = "t")
p_ADF_FTSE = punitroot(ur.df(FTSE, type = "trend", lags = k_max, selectlags = "AIC")@teststat [1], N = length(DAX), trend = "ct", statistic = "t")



results1 <- c(p_ADF_DAX,p_ADF_SSEC,p_ADF_PX,p_ADF_BUX,p_ADF_WIG,p_ADF_NIKKEI,p_ADF_HS,p_ADF_FTSE)
results1
# Vo vektore results1 vid�me v�sledky ADF-testu na stacionaritu pre ceny akciov�ch indexov.
# Z uveden�ch v�sledkov vypl�va, �e ceny sk�man�ch akciov�ch indexov nie s� stacion�rne, 
# ke�e sme pre ani jeden akciov� index nevedeli zamietnu� nulov� hypot�zu o existencii jednotkov�ho kore�a.
# Hodnoty ADF-testu s� v��ie ako 0,01. Teda na�e �asov� rady sa nemenia line�rne.


# Testovanie korel�cie n�m uk�e, �i sa dve n�hodne premenn� spolo�n� menia a aj to ak�m sp�sobom. 
# Pre vyjadrenie korela�nej anal�zy sl��i Pearsonov korela�n� koeficient,ktor� nadob�da hodnoty od -1 po 1. 
# Znamienko korela�n�ho koeficienta z�vis� od kovariancie, pod�a ktorej hodnotu koeficienta interpretujeme: 
# ??x,y > 0 ??? priama line�rna z�vislos�
# ??x,y < 0 ??? nepriama line�rna z�vislos��  
# ??x,y = 0 ??? line�rna nez�vislos�� (nekorelovanost�)
# Silu korel�cie ch�peme pod�a absol�tnej hodnoty Pearsonovho korela�n�ho koeficientu: 

# ??x,y     interpret�cia hodnoty

# 0,0 - 0,1   trivi�lna 
# 0,1 - 0,3   mal� 
# 0,3 - 0,5   stredn� 
# 0,5 - 0,7   ve�k�
# 0,7 - 0,9   ve�mi ve�k� 
# 0,9 - 1,0   takmer dokonal�
# �tatistick� v�znamnos� popisuje p-hodnota. V pr�pade, �e je hladina v�znamnosti ni��ia ako 0,01 zamietame nulov� hypot�zu (�asove rady nie s� korelovan�) 
# a prij�mame alternat�vnu hypot�zu (�asov� rady s� korelovan�).

DP <- cor.test(DAX,PX,method = "pearson")
DP
DS <- cor.test(DAX,SSEC,method = "pearson")
DS
DB <- cor.test(DAX,BUX,method = "pearson")
DB
DW <- cor.test(DAX,WIG,method = "pearson")
DW
DN <- cor.test(DAX,NIKKEI,method = "pearson")
DN
DH <- cor.test(DAX,HS,method = "pearson")
DH
DF <- cor.test(DAX,FTSE,method = "pearson")
DF
PS <- cor.test(PX,SSEC,method = "pearson")
PS
PB <- cor.test(PX,BUX,method = "pearson")
PB
PW <- cor.test(PX,WIG,method = "pearson")
PW
PN <- cor.test(PX,NIKKEI,method = "pearson")
PN
PH <- cor.test(PX,HS,method = "pearson")
PH
PF <- cor.test(PX,FTSE,method = "pearson")
PF
SB <- cor.test(SSEC,BUX,method = "pearson")
SB
SW <- cor.test(SSEC,WIG,method = "pearson")
SW
SN <- cor.test(SSEC,NIKKEI,method = "pearson")
SN
SH <- cor.test(SSEC,HS,method = "pearson")
SH
SF <- cor.test(SSEC,FTSE,method = "pearson")
SF
BW <- cor.test(BUX,WIG,method = "pearson")
BW
BN <- cor.test(BUX,NIKKEI,method = "pearson")
BN
BH <- cor.test(BUX,HS,method = "pearson")
BH
BF <- cor.test(BUX,FTSE,method = "pearson")
BF
WN <- cor.test(WIG,NIKKEI,method = "pearson")
WN
WH <- cor.test(WIG,HS,method = "pearson")
WH
WF <- cor.test(WIG,FTSE,method = "pearson")
WH
NH <- cor.test(NIKKEI,HS,method = "pearson")
NH
NF <- cor.test(NIKKEI,FTSE,method = "pearson")
NF
HF <- cor.test(HS,FTSE,method = "pearson")
HF

# Z v�sledkov sme vytvorili korela�n� maticu.

l1<- cbind(DAX,NIKKEI, FTSE,PX,BUX,WIG,HS,SSEC)
cor(l1)

# P-hodnota bola v ka�dom pr�pade nulov� s v�nimkou jedinej dvojice, a to BUX, WIG s hodnotou p-hodnota = 0.2681.
# Z �oho n�m vypl�va, �e medzi t�mito dvoma indexami sa nepreuk�zala korel�cia,
# �o je mierne prekvapuj�ce, ke�e sa jedn� o krajiny strednej Eur�py (Ma�arsko, Po�sko). Z korela�nej matice 
# vid�me ve�mi ve�k� z�vislos�� medzi rozvinut�mi trhmi (DAX, NIKKEI, FTSE, HS) a do silnej z�vislosti k 
# n�m patr� aj ��nsky SSEC. Zauj�mav� je korel�cia �esk�ho(PX) a po�sk�ho trhu(WIG), ktor� je z�porn� vo�i 
# ostatn�m trhom.

# V�sledn� anal�za n�m potvrdila predpoklad, �e rozvinut� akciov� trhy ovplyv�uj� svojim spr�van�m trhy, ktor� sa e�te len rozv�jaj�, a� na niektor� v�nimky. 
# V s��asnosti je prenos inform�cii ve�mi r�chly, �o potvrdili z�vislosti trhov vyspel�ch ekonomick�ch kraj�n, kde sa trhy vz�jomne ovplyv�uj� takmer okam�ite.
