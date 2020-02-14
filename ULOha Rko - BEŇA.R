# Našou ulohou bude vzájomná analıza akciovıch indexov ktoré zaznamenavajú profitovanie daného trhu.
# Rozhodli sme sa pre porovnanie trhov vybra trhy ázijskıch a európskych krajín.
# Zisovali sme vlastnosti vybranıch èasovıch radov a pod¾a rôznych kritérií urèovali, 
# ktoré trhy svojím správaním ovplyvòujú ostatné trhy.

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
# Vo vektore results1 vidíme vısledky ADF-testu na stacionaritu pre ceny akciovıch indexov.
# Z uvedenıch vısledkov vyplıva, e ceny skúmanıch akciovıch indexov nie sú stacionárne, 
# keïe sme pre ani jeden akciovı index nevedeli zamietnu nulovú hypotézu o existencii jednotkového koreòa.
# Hodnoty ADF-testu sú väèšie ako 0,01. Teda naše èasové rady sa nemenia lineárne.


# Testovanie korelácie nám ukáe, èi sa dve náhodne premenné spoloèné menia a aj to akım spôsobom. 
# Pre vyjadrenie korelaènej analızy slúi Pearsonov korelaènı koeficient,ktorı nadobúda hodnoty od -1 po 1. 
# Znamienko korelaèného koeficienta závisí od kovariancie, pod¾a ktorej hodnotu koeficienta interpretujeme: 
# ??x,y > 0 ??? priama lineárna závislos
# ??x,y < 0 ??? nepriama lineárna závislos’  
# ??x,y = 0 ??? lineárna nezávislos’ (nekorelovanost’)
# Silu korelácie chápeme pod¾a absolútnej hodnoty Pearsonovho korelaèného koeficientu: 

# ??x,y     interpretácia hodnoty

# 0,0 - 0,1   triviálna 
# 0,1 - 0,3   malá 
# 0,3 - 0,5   stredná 
# 0,5 - 0,7   ve¾ká
# 0,7 - 0,9   ve¾mi ve¾ká 
# 0,9 - 1,0   takmer dokonalá
# Štatistickú vıznamnos popisuje p-hodnota. V prípade, e je hladina vıznamnosti nišia ako 0,01 zamietame nulovú hypotézu (èasove rady nie sú korelované) 
# a prijímame alternatívnu hypotézu (èasové rady sú korelované).

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

# Z vısledkov sme vytvorili korelaènú maticu.

l1<- cbind(DAX,NIKKEI, FTSE,PX,BUX,WIG,HS,SSEC)
cor(l1)

# P-hodnota bola v kadom prípade nulová s vınimkou jedinej dvojice, a to BUX, WIG s hodnotou p-hodnota = 0.2681.
# Z èoho nám vyplıva, e medzi tımito dvoma indexami sa nepreukázala korelácia,
# èo je mierne prekvapujúce, keïe sa jedná o krajiny strednej Európy (Maïarsko, Po¾sko). Z korelaènej matice 
# vidíme ve¾mi ve¾kú závislos’ medzi rozvinutımi trhmi (DAX, NIKKEI, FTSE, HS) a do silnej závislosti k 
# ním patrí aj èínsky SSEC. Zaujímavá je korelácia èeského(PX) a po¾ského trhu(WIG), ktorá je záporná voèi 
# ostatnım trhom.

# Vısledná analıza nám potvrdila predpoklad, e rozvinuté akciové trhy ovplyvòujú svojim správaním trhy, ktoré sa ešte len rozvíjajú, a na niektoré vınimky. 
# V súèasnosti je prenos informácii ve¾mi rıchly, èo potvrdili závislosti trhov vyspelıch ekonomickıch krajín, kde sa trhy vzájomne ovplyvòujú takmer okamite.
