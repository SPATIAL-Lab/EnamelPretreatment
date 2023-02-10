# Setup -------------------------------------------------------------------

library(readxl); library(tidyverse); library(lsr)
StorageCondExp <- read_excel("data/StorageCondExp.xlsx")
StorageCondExp$sample_id <- substr(StorageCondExp$`Sample ID`, 7, 11)
scdata <- separate(StorageCondExp, "Sample ID", sep="-",into=c("SLC20","tooth",
                                                               "AB","treat"))
scdata$sample_id <- gsub("-", "", scdata$sample_id)


ambientT1 <- filter(scdata,AB=="A",Time=="T1") %>% 
  group_by(sample_id) %>% 
  summarize(dC = mean(dC), 
            dO = mean(dO), 
            CO3 = mean(CO3)) %>%  
  mutate(time = "37 Days", 
         location = "Cabinet")

ambientT2 <- filter(scdata,AB=="A",Time=="T2") %>% 
  group_by(sample_id) %>% 
  summarize(dC = mean(dC), 
            dO = mean(dO), 
            CO3 = mean(CO3)) %>%  
  mutate(time = "71 Days", 
         location = "Cabinet")

ambient <- rbind(ambientT1, ambientT2) %>% 
  group_by(sample_id) %>% 
  mutate(dC = dC[time == '37 Days'] - dC[time == '71 Days'], 
         dO = dO[time == '37 Days'] - dO[time == '71 Days'], 
         CO3 = CO3[time == '37 Days'] - CO3[time == '71 Days']) %>% 
  distinct(sample_id, .keep_all = T) %>% 
  select(-c(location, time))

desiccatorT1 <- filter(scdata,AB=="B",Time=="T1") %>% 
  group_by(sample_id) %>% 
  summarize(dC = mean(dC), 
            dO = mean(dO), 
            CO3 = mean(CO3)) %>%  
  mutate(time = "37 Days", 
         location = "Desiccator")

desiccatorT2 <- filter(scdata,AB=="B",Time=="T2") %>% 
  group_by(sample_id) %>% 
  summarize(dC = mean(dC), 
            dO = mean(dO), 
            CO3 = mean(CO3)) %>%  
  mutate(time = "71 Days", 
         location = "Desiccator")

dessiccator <- rbind(desiccatorT1, desiccatorT2) %>% 
  group_by(sample_id) %>% 
  mutate(dC = dC[time == '37 Days'] - dC[time == '71 Days'], 
         dO = dO[time == '37 Days'] - dO[time == '71 Days'], 
         CO3 = CO3[time == '37 Days'] - CO3[time == '71 Days']) %>% 
  distinct(sample_id, .keep_all = T) %>% 
  select(-c(location, time))

df <- rbind(ambientT1, ambientT2, desiccatorT1, desiccatorT2) %>% 
  mutate(condition = paste(time, location))

# Are the data normally distributed? 
hist(df$dC)
hist(df$dO)
hist(df$CO3)
#oh dear...
shapiro.test(df$dC)
shapiro.test(df$dO)
shapiro.test(df$CO3)
library(ggpubr)
ggqqplot(df$dC)
ggqqplot(df$dO)
ggqqplot(df$CO3)

kruskal.test(dC ~ condition, data = df)
kruskal.test(dO ~ condition, data = df)
kruskal.test(CO3 ~ condition, data = df)

# Post-hoc tests
median(subset(df, time == '37 Days')$CO3)
median(subset(df, time == '71 Days')$CO3)
wilcox.test(df$CO3 ~ df$time)
cohensD(subset(df, time == '37 Days')$CO3, subset(df, time == '71 Days')$CO3)

median(subset(df, location == 'Cabinet')$CO3)
median(subset(df, location == 'Desiccator')$CO3)
wilcox.test(df$CO3 ~ df$location)
# From Masters ------------------------------------------------------------

library(readxl)
StorageCondExp <- read_excel("data/StorageCondExp.xlsx")

library(tidyr)
scdata <- separate(StorageCondExp, "Sample ID", sep="-",into=c("SLC20","tooth",
"AB","treat"))


library(dplyr)
cabinetT1 <- filter(scdata,AB=="A",Time=="T1")
cabinetT2 <- filter(scdata,AB=="A",Time=="T2")
desiccatorT1 <- filter(scdata,AB=="B",Time=="T1")
desiccatorT2 <- filter(scdata,AB=="B",Time=="T2")

library(data.table)

dtcabinetT1 <- data.table(cabinetT1)
dtcabinetT2 <- data.table(cabinetT2)
dtdesiccatorT1 <- data.table(desiccatorT1)
dtdesiccatorT2 <- data.table(desiccatorT2)

dtcabinetT1$Sample <- paste(dtcabinetT1$tooth, dtcabinetT1$`AB`, dtcabinetT1$Time)
dtcabinetT2$Sample <- paste(dtcabinetT2$tooth, dtcabinetT2$`AB`, dtcabinetT2$Time)
dtdesiccatorT1$Sample <- paste(dtdesiccatorT1$tooth, dtdesiccatorT1$`AB`, dtdesiccatorT1$Time)
dtdesiccatorT2$Sample <- paste(dtdesiccatorT2$tooth, dtdesiccatorT2$`AB`, dtdesiccatorT2$Time)

dtcabinetT1[,"SLC20":=NULL][,"tooth":=NULL][,"AB":=NULL][,"Time":=NULL][
  ,"avgdc":=mean(dC),by="Sample"][,"avgdo":=mean(dO),by="Sample"][
    ,"avgco3":=mean(CO3),by="Sample"][,"stdevdc":=sd(dC),by="Sample"][
      ,"stdevdo":=sd(dO),by="Sample"][,"stdevco3":=sd(CO3),by="Sample"]

dtcabinetT2[,"SLC20":=NULL][,"tooth":=NULL][,"AB":=NULL][,"Time":=NULL][
  ,"avgdc":=mean(dC),by="Sample"][,"avgdo":=mean(dO),by="Sample"][
    ,"avgco3":=mean(CO3),by="Sample"][,"stdevdc":=sd(dC),by="Sample"][
      ,"stdevdo":=sd(dO),by="Sample"][,"stdevco3":=sd(CO3),by="Sample"]

dtdesiccatorT1[,"SLC20":=NULL][,"tooth":=NULL][,"AB":=NULL][,"Time":=NULL][
  ,"avgdc":=mean(dC),by="Sample"][,"avgdo":=mean(dO),by="Sample"][
    ,"avgco3":=mean(CO3),by="Sample"][,"stdevdc":=sd(dC),by="Sample"][
      ,"stdevdo":=sd(dO),by="Sample"][,"stdevco3":=sd(CO3),by="Sample"]

dtdesiccatorT2[,"SLC20":=NULL][,"tooth":=NULL][,"AB":=NULL][,"Time":=NULL][
  ,"avgdc":=mean(dC),by="Sample"][,"avgdo":=mean(dO),by="Sample"][
    ,"avgco3":=mean(CO3),by="Sample"][,"stdevdc":=sd(dC),by="Sample"][
      ,"stdevdo":=sd(dO),by="Sample"][,"stdevco3":=sd(CO3),by="Sample"]


dtavgdcT1 <- data.table(dtcabinetT1$Sample,dtcabinetT1$avgdc,dtdesiccatorT1$avgdc)
setnames(dtavgdcT1,c("V1","V2","V3"),c("Sample", "cabinet.avg.dc","desiccator.avg.dc"))
dtavgdcT1 <- unique(dtavgdcT1)

dtavgdcT2 <- data.table(dtcabinetT2$Sample,dtcabinetT2$avgdc,dtdesiccatorT2$avgdc)
setnames(dtavgdcT2,c("V1","V2","V3"),c("Sample", "cabinet.avg.dc","desiccator.avg.dc"))
dtavgdcT2 <- unique(dtavgdcT2)

dtavgdoT1 <- data.table(dtcabinetT1$Sample,dtcabinetT1$avgdo,dtdesiccatorT1$avgdo)
setnames(dtavgdoT1,c("V1","V2","V3"),c("Sample", "cabinet.avg.do","desiccator.avg.do"))
dtavgdoT1 <- unique(dtavgdoT1)

dtavgdoT2 <- data.table(dtcabinetT2$Sample,dtcabinetT2$avgdo,dtdesiccatorT2$avgdo)
setnames(dtavgdoT2,c("V1","V2","V3"),c("Sample", "cabinet.avg.do","desiccator.avg.do"))
dtavgdoT2 <- unique(dtavgdoT2)

dtavgco3T1 <- data.table(dtcabinetT1$Sample,dtcabinetT1$avgco3,dtdesiccatorT1$avgco3)
setnames(dtavgco3T1,c("V1","V2","V3"),c("Sample", "cabinet.avg.co3","desiccator.avg.co3"))
dtavgco3T1 <- unique(dtavgco3T1)

dtavgco3T2 <- data.table(dtcabinetT2$Sample,dtcabinetT2$avgco3,dtdesiccatorT2$avgco3)
setnames(dtavgco3T2,c("V1","V2","V3"),c("Sample", "cabinet.avg.co3","desiccator.avg.co3"))
dtavgco3T2 <- unique(dtavgco3T2)

#stdev
dtstdevdcT1 <- data.table(dtcabinetT1$Sample,dtcabinetT1$stdevdc,dtdesiccatorT1$stdevdc)
setnames(dtstdevdcT1,c("V1","V2","V3"),c("Sample", "cabinet.stdev.dc","desiccator.stdev.dc"))
dtstdevdcT1 <- unique(dtstdevdcT1)

dtstdevdcT2 <- data.table(dtcabinetT2$Sample,dtcabinetT2$stdevdc,dtdesiccatorT2$stdevdc)
setnames(dtstdevdcT2,c("V1","V2","V3"),c("Sample", "cabinet.stdev.dc","desiccator.stdev.dc"))
dtstdevdcT2 <- unique(dtstdevdcT2)

dtstdevdoT1 <- data.table(dtcabinetT1$Sample,dtcabinetT1$stdevdo,dtdesiccatorT1$stdevdo)
setnames(dtstdevdoT1,c("V1","V2","V3"),c("Sample", "cabinet.stdev.do","desiccator.stdev.do"))
dtstdevdoT1 <- unique(dtstdevdoT1)

dtstdevdoT2 <- data.table(dtcabinetT2$Sample,dtcabinetT2$stdevdo,dtdesiccatorT2$stdevdo)
setnames(dtstdevdoT2,c("V1","V2","V3"),c("Sample", "cabinet.stdev.do","desiccator.stdev.do"))
dtstdevdoT2 <- unique(dtstdevdoT2)

dtstdevco3T1 <- data.table(dtcabinetT1$Sample,dtcabinetT1$stdevco3,dtdesiccatorT1$stdevco3)
setnames(dtstdevco3T1,c("V1","V2","V3"),c("Sample", "cabinet.stdev.co3","desiccator.stdev.co3"))
dtstdevco3T1 <- unique(dtstdevco3T1)

dtstdevco3T2 <- data.table(dtcabinetT2$Sample,dtcabinetT2$stdevco3,dtdesiccatorT2$stdevco3)
setnames(dtstdevco3T2,c("V1","V2","V3"),c("Sample", "cabinet.stdev.co3","desiccator.stdev.co3"))
dtstdevco3T2 <- unique(dtstdevco3T2)


#avg diff
dtavgdcT1$dcdiff <- dtavgdcT1$desiccator.avg.dc - dtavgdcT1$cabinet.avg.dc
dtavgdcT2$dcdiff <- dtavgdcT2$desiccator.avg.dc - dtavgdcT2$cabinet.avg.dc

dtavgdoT1$dodiff <- dtavgdoT1$desiccator.avg.do - dtavgdoT1$cabinet.avg.do
dtavgdoT2$dodiff <- dtavgdoT2$desiccator.avg.do - dtavgdoT2$cabinet.avg.do

dtavgco3T1$co3diff <- dtavgco3T1$desiccator.avg.co3 - dtavgco3T1$cabinet.avg.co3
dtavgco3T2$co3diff <- dtavgco3T2$desiccator.avg.co3 - dtavgco3T2$cabinet.avg.co3

#stdev diff
dtstdevdcT1$dcdiff <- dtstdevdcT1$desiccator.stdev.dc - dtstdevdcT1$cabinet.stdev.dc
dtstdevdcT2$dcdiff <- dtstdevdcT2$desiccator.stdev.dc - dtstdevdcT2$cabinet.stdev.dc

dtstdevdoT1$dodiff <- dtstdevdoT1$desiccator.stdev.do - dtstdevdoT1$cabinet.stdev.do
dtstdevdoT2$dodiff <- dtstdevdoT2$desiccator.stdev.do - dtstdevdoT2$cabinet.stdev.do

dtstdevco3T1$co3diff <- dtstdevco3T1$desiccator.stdev.co3 - dtstdevco3T1$cabinet.stdev.co3
dtstdevco3T2$co3diff <- dtstdevco3T2$desiccator.stdev.co3 - dtstdevco3T2$cabinet.stdev.co3


library(grDevices)
##average box plots
par(mfrow=c(2,3))
boxplot(dtavgdcT1$dcdiff,dtavgdcT2$dcdiff,
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2),
        names = c("Time 1 = 37 days", "Time 2 = 71 days"),
        col = c("#ecc19c", "#1e847f"))
title(ylab = expression(delta^{13}*'C average difference'), line = 2.25, cex.lab = 1.5)
abline(h=0, lty = 6, lwd = 2, col = "gray48")
boxplot(dtavgdcT1$dcdiff,dtavgdcT2$dcdiff, 
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2),
        names = c("Time 1 = 37 days", "Time 2 = 71 days"),
        col = c("#ecc19c", "#1e847f"),
        add = TRUE)
title(ylab = expression(delta^{13}*'C average difference'), line = 2.25, cex.lab = 1.5)
mtext("a)", side = 3, line = 0.5, las = 1, adj = 0)

boxplot(dtavgdoT1$dodiff,dtavgdoT2$dodiff, 
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2),
        names = c("Time 1 = 37 days", "Time 2 = 71 days"),
        col = c("#ecc19c", "#1e847f"))
title(ylab = expression(delta^{18}*'O average difference'), line = 2.25, cex.lab = 1.5)
abline(h=0, lty = 6, lwd = 2, col = "gray48")
boxplot(dtavgdoT1$dodiff,dtavgdoT2$dodiff, 
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2),
        names = c("Time 1 = 37 days", "Time 2 = 71 days"),
        col = c("#ecc19c", "#1e847f"),
        add = TRUE)
title(ylab = expression(delta^{18}*'O average difference'), line = 2.25, cex.lab = 1.5)
mtext("b)", side = 3, line = 0.5, las = 1, adj = 0)

boxplot(dtavgco3T1$co3diff,dtavgco3T2$co3diff,
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2),
        names = c("Time 1 = 37 days", "Time 2 = 71 days"),
        col = c("#ecc19c", "#1e847f"))
title(ylab = expression('CO'[3]*'% average difference'), line = 2.25, cex.lab = 1.5)
abline(h=0, lty = 6, lwd = 2, col = "gray48")
boxplot(dtavgco3T1$co3diff,dtavgco3T2$co3diff,
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2),
        names = c("Time 1 = 37 days", "Time 2 = 71 days"),
        col = c("#ecc19c", "#1e847f"),
        add = TRUE)
title(ylab = expression('CO'[3]*'% average difference'), line = 2.25, cex.lab = 1.5)
mtext("c)", side = 3, line = 0.5, las = 1, adj = 0)

##stdev box plots
boxplot(dtstdevdcT1$dcdiff,dtstdevdcT2$dcdiff, 
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2),
        names = c("Time 1 = 37 days", "Time 2 = 71 days"),
        col = c("#ecc19c", "#1e847f"))
title(ylab = expression(delta^{13}*'C standard deviation difference'), line = 2.25, cex.lab = 1.5)
abline(h=0, lty = 6, lwd = 2, col = "gray48")
boxplot(dtstdevdcT1$dcdiff,dtstdevdcT2$dcdiff, 
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2),
        names = c("Time 1 = 37 days", "Time 2 = 71 days"),
        col = c("#ecc19c", "#1e847f"),
        add = TRUE)
title(ylab = expression(delta^{13}*'C standard deviation difference'), line = 2.25, cex.lab = 1.5)
mtext("d)", side = 3, line = 0.5, las = 1, adj = 0)

boxplot(dtstdevdoT1$dodiff,dtstdevdoT2$dodiff, 
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2),
        names = c("Time 1 = 37 days", "Time 2 = 71 days"),
        col = c("#ecc19c", "#1e847f"))
title(ylab = expression(delta^{18}*'O standard deviation difference'), line = 2.25, cex.lab = 1.5)
abline(h=0, lty = 6, lwd = 2, col = "gray48")
boxplot(dtstdevdoT1$dodiff,dtstdevdoT2$dodiff, 
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2),
        names = c("Time 1 = 37 days", "Time 2 = 71 days"),
        col = c("#ecc19c", "#1e847f"),
        add = TRUE)
title(ylab = expression(delta^{18}*'O standard deviation difference'), line = 2.25, cex.lab = 1.5)
mtext("e)", side = 3, line = 0.5, las = 1, adj = 0)

boxplot(dtstdevco3T1$co3diff,dtstdevco3T2$co3diff, 
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2),
        names = c("Time 1 = 37 days", "Time 2 = 71 days"),
        col = c("#ecc19c", "#1e847f"))
title(ylab = expression('CO'[3]*'% standard deviation difference'), line = 2.25, cex.lab = 1.5)
abline(h=0, lty = 6, lwd = 2, col = "gray48")
boxplot(dtstdevco3T1$co3diff,dtstdevco3T2$co3diff,
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2),
        names = c("Time 1 = 37 days", "Time 2 = 71 days"),
        col = c("#ecc19c", "#1e847f"),
        add = TRUE)
title(ylab = expression('CO'[3]*'% standard deviation difference'), line = 2.25, cex.lab = 1.5)
mtext("f)", side = 3, line = 0.5, las = 1, adj = 0)

##F-test
dc.ftest <- var.test(dtavgdcT1$dcdiff,dtavgdcT2$dcdiff)
do.ftest <- var.test(dtavgdoT1$dodiff,dtavgdoT2$dodiff)
co3.ftest <- var.test(dtavgco3T1$co3diff,dtavgco3T2$co3diff)


#t-tests
dcT1.avg.ttest <- t.test(dtavgdcT1$cabinet.avg.dc,dtavgdcT1$desiccator.avg.dc,paired=TRUE)
dcT2.avg.ttest <- t.test(dtavgdcT2$cabinet.avg.dc,dtavgdcT2$desiccator.avg.dc,paired=TRUE)
doT1.avg.ttest <- t.test(dtavgdoT1$cabinet.avg.do,dtavgdoT1$desiccator.avg.do,paired=TRUE)
doT2.avg.ttest <- t.test(dtavgdoT2$cabinet.avg.do,dtavgdoT2$desiccator.avg.do,paired=TRUE)
co3T1.avg.ttest <- t.test(dtavgco3T1$cabinet.avg.co3,dtavgco3T1$desiccator.avg.co3,paired=TRUE)
co3T2.avg.ttest <- t.test(dtavgco3T2$cabinet.avg.co3,dtavgco3T2$desiccator.avg.co3,paired=TRUE)


dcT1.stdev.ttest <- t.test(dtstdevdcT1$cabinet.stdev.dc,dtstdevdcT1$desiccator.stdev.dc,paired=TRUE)
dcT2.stdev.ttest <- t.test(dtstdevdcT2$cabinet.stdev.dc,dtstdevdcT2$desiccator.stdev.dc,paired=TRUE)
doT1.stdev.ttest <- t.test(dtstdevdoT1$cabinet.stdev.do,dtstdevdoT1$desiccator.stdev.do,paired=TRUE)
doT2.stdev.ttest <- t.test(dtstdevdoT2$cabinet.stdev.do,dtstdevdoT2$desiccator.stdev.do,paired=TRUE)
co3T1.stdev.ttest <- t.test(dtstdevco3T1$cabinet.stdev.co3,dtstdevco3T1$desiccator.stdev.co3,paired=TRUE)
co3T2.stdev.ttest <- t.test(dtstdevco3T2$cabinet.stdev.co3,dtstdevco3T2$desiccator.stdev.co3,paired=TRUE)


ftest.pvalues <- data.table(a = c("dc","do","co3"),
                            pvalue = c(dc.ftest$p.value,do.ftest$p.value,co3.ftest$p.value),
                            estimate = c(dc.ftest$estimate,do.ftest$estimate,co3.ftest$estimate))

ftest.pvalues[pvalue < 0.05, sig:="significant"][pvalue > 0.1, sig:="not"]
sd(dtavgdcT1$dcdiff)-sd(dtavgdcT2$dcdiff)

ttest.pvalues <- data.table(a = c("dcT1.avg","doT1.avg","co3T1.avg",
                                  "dcT2.avg","doT2.avg","co3T2.avg",
                                  "dcT1.stdev","doT1.stdev","co3T1.stdev",
                                  "dcT2.stdev","doT2.stdev","co3T2.stdev"),
                            pvalue = c(dcT1.avg.ttest$p.value,doT1.avg.ttest$p.value,
                                       co3T1.avg.ttest$p.value,
                                       dcT2.avg.ttest$p.value,doT2.avg.ttest$p.value,
                                       co3T2.avg.ttest$p.value,
                                       dcT1.stdev.ttest$p.value,doT1.stdev.ttest$p.value,
                                       co3T1.stdev.ttest$p.value,
                                       dcT2.stdev.ttest$p.value,doT2.stdev.ttest$p.value,
                                       co3T2.stdev.ttest$p.value),
                            estimate = c(dcT1.avg.ttest$estimate,doT1.avg.ttest$estimate,
                                         co3T1.avg.ttest$estimate,
                                         dcT2.avg.ttest$estimate,doT2.avg.ttest$estimate,
                                         co3T2.avg.ttest$estimate,
                                         dcT1.stdev.ttest$estimate,doT1.stdev.ttest$estimate,
                                         co3T1.stdev.ttest$estimate,
                                         dcT2.stdev.ttest$estimate,doT2.stdev.ttest$estimate,
                                         co3T2.stdev.ttest$estimate))


ttest.pvalues[pvalue < 0.05, sig:="significant"][pvalue > 0.1, sig:="not"]



