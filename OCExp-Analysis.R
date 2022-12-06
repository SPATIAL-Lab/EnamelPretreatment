library(readxl)
OrgContExp <- read_excel("data/OrgContExp.xlsx")

library(tidyr)
ocdata <- separate(OrgContExp, "Original ID", sep="-",into=c("IA21","tooth",
                                                             "Treatment","rep"))

library(dplyr)
library(data.table)

ocdata <- data.table(ocdata)
ocdata[,"IA21":=NULL][,"rep":=NULL]
ocdata$Sample <- paste(ocdata$tooth, ocdata$Treatment)
ocdata[,"tooth":=NULL][,"Treatment":=NULL]

ocdata[,"avgdc":=mean(dC, na.rm = TRUE),by="Sample"][,"avgdo":=mean(dO, na.rm = TRUE),by="Sample"][
  ,"avgco3":=mean(CO3, na.rm = TRUE),by="Sample"][,"dC":=NULL][,"dO":=NULL][,"CO3":=NULL]

ocdata <- unique(ocdata)

ocdata <- separate(ocdata, "Sample", sep=" ",into=c("Tooth","Treatment"))

TreatA <- data.table(filter(ocdata,Treatment=="A"))
TreatB <- data.table(filter(ocdata,Treatment=="B"))
TreatC <- data.table(filter(ocdata,Treatment=="C"))
TreatD <- data.table(filter(ocdata,Treatment=="D"))
TreatE <- data.table(filter(ocdata,Treatment=="E"))
TreatF <- data.table(filter(ocdata,Treatment=="F"))
TreatG <- data.table(filter(ocdata,Treatment=="G"))
TreatH <- data.table(filter(ocdata,Treatment=="H"))
TreatI <- data.table(filter(ocdata,Treatment=="I"))

####dC
TreatA$EffectSize.dc <- TreatA$avgdc - TreatI$avgdc
TreatB$EffectSize.dc <- TreatB$avgdc - TreatI$avgdc
TreatC$EffectSize.dc <- TreatC$avgdc - TreatI$avgdc
TreatD$EffectSize.dc <- TreatD$avgdc - TreatI$avgdc
TreatE$EffectSize.dc <- TreatE$avgdc - TreatI$avgdc
TreatF$EffectSize.dc <- TreatF$avgdc - TreatI$avgdc
TreatG$EffectSize.dc <- TreatG$avgdc - TreatI$avgdc
TreatH$EffectSize.dc <- TreatH$avgdc - TreatI$avgdc

par(mfrow=c(2,4))
plot(TreatI$avgdc, TreatA$EffectSize.dc)
plot(TreatI$avgdc, TreatB$EffectSize.dc)
plot(TreatI$avgdc, TreatC$EffectSize.dc)
plot(TreatI$avgdc, TreatD$EffectSize.dc)
plot(TreatI$avgdc, TreatE$EffectSize.dc)
plot(TreatI$avgdc, TreatF$EffectSize.dc)
plot(TreatI$avgdc, TreatG$EffectSize.dc)
plot(TreatI$avgdc, TreatH$EffectSize.dc)

####dO
TreatA$EffectSize.do <- TreatA$avgdo - TreatI$avgdo
TreatB$EffectSize.do <- TreatB$avgdo - TreatI$avgdo
TreatC$EffectSize.do <- TreatC$avgdo - TreatI$avgdo
TreatD$EffectSize.do <- TreatD$avgdo - TreatI$avgdo
TreatE$EffectSize.do <- TreatE$avgdo - TreatI$avgdo
TreatF$EffectSize.do <- TreatF$avgdo - TreatI$avgdo
TreatG$EffectSize.do <- TreatG$avgdo - TreatI$avgdo
TreatH$EffectSize.do <- TreatH$avgdo - TreatI$avgdo

par(mfrow=c(2,4))
plot(TreatI$avgdo, TreatA$EffectSize.do,
     col = c("red", "orange", "green", "blue", "purple"))
plot(TreatI$avgdo, TreatB$EffectSize.do,
     col = c("red", "orange", "green", "blue", "purple"))
plot(TreatI$avgdo, TreatC$EffectSize.do,
     col = c("red", "orange", "green", "blue", "purple"))
plot(TreatI$avgdo, TreatD$EffectSize.do,
     col = c("red", "orange", "green", "blue", "purple"))
plot(TreatI$avgdo, TreatE$EffectSize.do,
     col = c("red", "orange", "green", "blue", "purple"))
plot(TreatI$avgdo, TreatF$EffectSize.do,
     col = c("red", "orange", "green", "blue", "purple"))
plot(TreatI$avgdo, TreatG$EffectSize.do,
     col = c("red", "orange", "green", "blue", "purple"))
plot(TreatI$avgdo, TreatH$EffectSize.do,
     col = c("red", "orange", "green", "blue", "purple"))

####co3
TreatA$EffectSize.co3 <- TreatA$avgco3 - TreatI$avgco3
TreatB$EffectSize.co3 <- TreatB$avgco3 - TreatI$avgco3
TreatC$EffectSize.co3 <- TreatC$avgco3 - TreatI$avgco3
TreatD$EffectSize.co3 <- TreatD$avgco3 - TreatI$avgco3
TreatE$EffectSize.co3 <- TreatE$avgco3 - TreatI$avgco3
TreatF$EffectSize.co3 <- TreatF$avgco3 - TreatI$avgco3
TreatG$EffectSize.co3 <- TreatG$avgco3 - TreatI$avgco3
TreatH$EffectSize.co3 <- TreatH$avgco3 - TreatI$avgco3

par(mfrow=c(2,4))
plot(TreatI$avgco3, TreatA$EffectSize.co3)
plot(TreatI$avgco3, TreatB$EffectSize.co3)
plot(TreatI$avgco3, TreatC$EffectSize.co3)
plot(TreatI$avgco3, TreatD$EffectSize.co3)
plot(TreatI$avgco3, TreatE$EffectSize.co3)
plot(TreatI$avgco3, TreatF$EffectSize.co3)
plot(TreatI$avgco3, TreatG$EffectSize.co3)
plot(TreatI$avgco3, TreatH$EffectSize.co3)



