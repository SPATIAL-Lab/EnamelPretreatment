
# Setup -------------------------------------------------------------------
# note: do everything in setup and then move to publication tests
library(readxl)
psdata <- read_excel("data/ParticleSizeExp1.xlsx")

library(tidyr);library(dplyr);library(data.table); library(lsr)

##Treated Coarse, Analyzed Coarse - Batch 1
B1.CC <- data.table(filter(psdata,Tooth=="A",Treat=="CC",Analysis=="1"))

##Treated Fine, Analyzed Fine - Batch 1
B1.FF <- data.table(filter(psdata,Tooth=="A",Treat=="FF",Analysis=="1"))

##Treated Coarse, Analyzed Fine - Batch 2
B2.CF <- data.table(filter(psdata,Tooth=="A",Treat=="CF",Analysis=="2"))

##Treated Fine, Analyzed Fine - Batch 2
B2.FF <- data.table(filter(psdata,Tooth=="A",Treat=="FF",Analysis=="2"))

##Treated Coarse, Analyzed Coarse - Batch 3
B3.CC <- data.table(filter(psdata,Tooth=="B",Treat=="CC",Analysis=="1"))

##Treated Coarse, Analyzed Fine - Batch 3
B3.CF <- data.table(filter(psdata,Tooth=="B",Treat=="CF",Analysis=="1"))


# Masters Tests and Figures -------------------------------------------------

##Averages and standard deviations of triplicates
B1.CC[,"avgdc":=mean(dC),by="Tooth number"][,"avgdo":=mean(dO),by="Tooth number"][,"avgco3":=mean(CO3),by="Tooth number"][,"stdevdc":=sd(dC),by="Tooth number"][,"stdevdo":=sd(dO),by="Tooth number"][,"stdevco3":=sd(CO3),by="Tooth number"]

B1.FF[,"avgdc":=mean(dC),by="Tooth number"][,"avgdo":=mean(dO),by="Tooth number"][,"avgco3":=mean(CO3),by="Tooth number"][,"stdevdc":=sd(dC),by="Tooth number"][,"stdevdo":=sd(dO),by="Tooth number"][,"stdevco3":=sd(CO3),by="Tooth number"]

B2.CF[,"avgdc":=mean(dC),by="Tooth number"][,"avgdo":=mean(dO),by="Tooth number"][,"avgco3":=mean(CO3),by="Tooth number"][,"stdevdc":=sd(dC),by="Tooth number"][,"stdevdo":=sd(dO),by="Tooth number"][,"stdevco3":=sd(CO3),by="Tooth number"]

B2.FF[,"avgdc":=mean(dC),by="Tooth number"][,"avgdo":=mean(dO),by="Tooth number"][,"avgco3":=mean(CO3),by="Tooth number"][,"stdevdc":=sd(dC),by="Tooth number"][,"stdevdo":=sd(dO),by="Tooth number"][,"stdevco3":=sd(CO3),by="Tooth number"]

B3.CC[,"avgdc":=mean(dC),by="Tooth number"][,"avgdo":=mean(dO),by="Tooth number"][,"avgco3":=mean(CO3),by="Tooth number"][,"stdevdc":=sd(dC),by="Tooth number"][,"stdevdo":=sd(dO),by="Tooth number"][,"stdevco3":=sd(CO3),by="Tooth number"]

B3.CF[,"avgdc":=mean(dC),by="Tooth number"][,"avgdo":=mean(dO),by="Tooth number"][,"avgco3":=mean(CO3),by="Tooth number"][,"stdevdc":=sd(dC),by="Tooth number"][,"stdevdo":=sd(dO),by="Tooth number"][,"stdevco3":=sd(CO3),by="Tooth number"]


##Combine averages
avg.dc <- unique(data.table(B1.CC$`Tooth number`,B1.CC$avgdc,B1.FF$avgdc,B2.CF$avgdc,B2.FF$avgdc,
                            B3.CC$avgdc,B3.CF$avgdc))
setnames(avg.dc,c("V1","V2","V3","V4","V5","V6","V7"),
         c("Tooth","B1.CC","B1.FF","B2.CF","B2.FF","B3.CC","B3.CF"))


avg.do <- unique(data.table(B1.CC$`Tooth number`,B1.CC$avgdo,B1.FF$avgdo,B2.CF$avgdo,B2.FF$avgdo,
                            B3.CC$avgdo,B3.CF$avgdo))
setnames(avg.do,c("V1","V2","V3","V4","V5","V6","V7"),
         c("Tooth","B1.CC","B1.FF","B2.CF","B2.FF","B3.CC","B3.CF"))


avg.co3 <- unique(data.table(B1.CC$`Tooth number`,B1.CC$avgco3,B1.FF$avgco3,B2.CF$avgco3,B2.FF$avgco3,
                             B3.CC$avgco3,B3.CF$avgco3))
setnames(avg.co3,c("V1","V2","V3","V4","V5","V6","V7"),
         c("Tooth","B1.CC","B1.FF","B2.CF","B2.FF","B3.CC","B3.CF"))

##Combine standard deviations
stdev.dc <- unique(data.table(B1.CC$`Tooth number`,B1.CC$stdevdc,B1.FF$stdevdc,B2.CF$stdevdc,B2.FF$stdevdc,
                            B3.CC$stdevdc,B3.CF$stdevdc))
setnames(stdev.dc,c("V1","V2","V3","V4","V5","V6","V7"),
         c("Tooth","B1.CC","B1.FF","B2.CF","B2.FF","B3.CC","B3.CF"))


stdev.do <- unique(data.table(B1.CC$`Tooth number`,B1.CC$stdevdo,B1.FF$stdevdo,B2.CF$stdevdo,B2.FF$stdevdo,
                            B3.CC$stdevdo,B3.CF$stdevdo))
setnames(stdev.do,c("V1","V2","V3","V4","V5","V6","V7"),
         c("Tooth","B1.CC","B1.FF","B2.CF","B2.FF","B3.CC","B3.CF"))


stdev.co3 <- unique(data.table(B1.CC$`Tooth number`,B1.CC$stdevco3,B1.FF$stdevco3,B2.CF$stdevco3,B2.FF$stdevco3,
                             B3.CC$stdevco3,B3.CF$stdevco3))
setnames(stdev.co3,c("V1","V2","V3","V4","V5","V6","V7"),
         c("Tooth","B1.CC","B1.FF","B2.CF","B2.FF","B3.CC","B3.CF"))

##Difference from "controls" - Average
avg.dc$b1diff <- avg.dc$B1.CC - avg.dc$B1.FF
avg.dc$b2diff <- avg.dc$B2.CF - avg.dc$B2.FF
avg.dc$b3diff <- avg.dc$B3.CC - avg.dc$B3.CF

avg.do$b1diff <- avg.do$B1.CC - avg.do$B1.FF
avg.do$b2diff <- avg.do$B2.CF - avg.do$B2.FF
avg.do$b3diff <- avg.do$B3.CC - avg.do$B3.CF

avg.co3$b1diff <- avg.co3$B1.CC - avg.co3$B1.FF
avg.co3$b2diff <- avg.co3$B2.CF - avg.co3$B2.FF
avg.co3$b3diff <- avg.co3$B3.CC - avg.co3$B3.CF


##Difference from "controls" - Standard Deviation
stdev.dc$b1diff <- stdev.dc$B1.CC - stdev.dc$B1.FF
stdev.dc$b2diff <- stdev.dc$B2.CF - stdev.dc$B2.FF
stdev.dc$b3diff <- stdev.dc$B3.CC - stdev.dc$B3.CF

stdev.do$b1diff <- stdev.do$B1.CC - stdev.do$B1.FF
stdev.do$b2diff <- stdev.do$B2.CF - stdev.do$B2.FF
stdev.do$b3diff <- stdev.do$B3.CC - stdev.do$B3.CF

stdev.co3$b1diff <- stdev.co3$B1.CC - stdev.co3$B1.FF
stdev.co3$b2diff <- stdev.co3$B2.CF - stdev.co3$B2.FF
stdev.co3$b3diff <- stdev.co3$B3.CC - stdev.co3$B3.CF

library(grDevices)
##Boxplots - Averages
tiff("Figure4.tif", width = 1050, height = 700)
par(mfrow=c(2,3))
boxplot(avg.dc$b1diff,avg.dc$b3diff,avg.dc$b2diff,
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2,3),
        names = c("CC-FF", "CC-CF", "CF-FF"),
        col = c("#aed6dc","#ff9a8d","#4a536b"))
title(ylab = expression(delta^{13}*'C average difference'), line = 2.25, cex.lab = 1.5)
abline(h=0, lty = 6, lwd = 2, col = "gray48")
boxplot(avg.dc$b1diff,avg.dc$b3diff,avg.dc$b2diff,
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2,3),
        names = c("CC-FF", "CC-CF", "CF-FF"),
        col = c("#aed6dc","#ff9a8d","#4a536b"),
        add = TRUE)
title(ylab = expression(delta^{13}*'C average difference'), line = 2.25, cex.lab = 1.5)
mtext("a)", side = 3, line = 0.5, las = 1, adj = 0)

boxplot(avg.do$b1diff,avg.do$b3diff,avg.do$b2diff,
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2,3),
        names = c("CC-FF", "CC-CF", "CF-FF"),
        col = c("#aed6dc","#ff9a8d","#4a536b"))
title(ylab = expression(delta^{18}*'O average difference'), line = 2.25, cex.lab = 1.5)
abline(h=0, lty = 6, lwd = 2, col = "gray48")
boxplot(avg.do$b1diff,avg.do$b3diff,avg.do$b2diff,
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2,3),
        names = c("CC-FF", "CC-CF", "CF-FF"),
        col = c("#aed6dc","#ff9a8d","#4a536b"),
        add = TRUE)
title(ylab = expression(delta^{18}*'O average difference'), line = 2.25, cex.lab = 1.5)
mtext("b)", side = 3, line = 0.5, las = 1, adj = 0)

boxplot(avg.co3$b1diff,avg.co3$b3diff,avg.co3$b2diff,
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2,3),
        names = c("CC-FF", "CC-CF", "CF-FF"),
        col = c("#aed6dc","#ff9a8d","#4a536b"))
title(ylab = expression('CO'[3]*'% average difference'), line = 2.25, cex.lab = 1.5)
abline(h=0, lty = 6, lwd = 2, col = "gray48")
boxplot(avg.co3$b1diff,avg.co3$b3diff,avg.co3$b2diff,
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2,3),
        names = c("CC-FF", "CC-CF", "CF-FF"),
        col = c("#aed6dc","#ff9a8d","#4a536b"),
        add = TRUE)
title(ylab = expression('CO'[3]*'% average difference'), line = 2.25, cex.lab = 1.5)
mtext("c)", side = 3, line = 0.5, las = 1, adj = 0)

##Boxplots - Standard Deviations
boxplot(stdev.dc$b1diff,stdev.dc$b3diff,stdev.dc$b2diff,
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2,3),
        names = c("CC-FF", "CC-CF", "CF-FF"),
        col = c("#aed6dc","#ff9a8d","#4a536b"))
title(ylab = expression(delta^{13}*'C standard deviation difference'), line = 2.25, cex.lab = 1.5)
abline(h=0, lty = 6, lwd = 2, col = "gray48")
boxplot(stdev.dc$b1diff,stdev.dc$b3diff,stdev.dc$b2diff,
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2,3),
        names = c("CC-FF", "CC-CF", "CF-FF"),
        col = c("#aed6dc","#ff9a8d","#4a536b"),
        add = TRUE)
title(ylab = expression(delta^{13}*'C standard deviation difference'), line = 2.25, cex.lab = 1.5)
mtext("d)", side = 3, line = 0.5, las = 1, adj = 0)

boxplot(stdev.do$b1diff,stdev.do$b3diff,stdev.do$b2diff,
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2,3),
        names = c("CC-FF", "CC-CF", "CF-FF"),
        col = c("#aed6dc","#ff9a8d","#4a536b"))
title(ylab = expression(delta^{18}*'O standard deviation difference'), line = 2.25, cex.lab = 1.5)
abline(h=0, lty = 6, lwd = 2, col = "gray48")
boxplot(stdev.do$b1diff,stdev.do$b3diff,stdev.do$b2diff,
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2,3),
        names = c("CC-FF", "CC-CF", "CF-FF"),
        col = c("#aed6dc","#ff9a8d","#4a536b"),
        add = TRUE)
title(ylab = expression(delta^{18}*'O standard deviation difference'), line = 2.25, cex.lab = 1.5)
mtext("e)", side = 3, line = 0.5, las = 1, adj = 0)

boxplot(stdev.co3$b1diff,stdev.co3$b3diff,stdev.co3$b2diff,
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2,3),
        names = c("CC-FF", "CC-CF", "CF-FF"),
        col = c("#aed6dc","#ff9a8d","#4a536b"))
title(ylab = expression('CO'[3]*'% standard deviation difference'), line = 2.25, cex.lab = 1.5)
abline(h=0, lty = 6, lwd = 2, col = "gray48")
boxplot(stdev.co3$b1diff,stdev.co3$b3diff,stdev.co3$b2diff,
        ylab = "",
        cex.axis = 1.25,
        at = c(1,2,3),
        names = c("CC-FF", "CC-CF", "CF-FF"),
        col = c("#aed6dc","#ff9a8d","#4a536b"),
        add = TRUE)
title(ylab = expression('CO'[3]*'% standard deviation difference'), line = 2.25, cex.lab = 1.5)
mtext("f)", side = 3, line = 0.5, las = 1, adj = 0)
dev.off()

##F-tests
b1b2.dc.ftest <- var.test(avg.dc$b1diff,avg.dc$b2diff)
b1b3.dc.ftest <- var.test(avg.dc$b1diff,avg.dc$b3diff)
b2b3.dc.ftest <- var.test(avg.dc$b2diff,avg.dc$b3diff)

b1b2.do.ftest <- var.test(avg.do$b1diff,avg.do$b2diff)
b1b3.do.ftest <- var.test(avg.do$b1diff,avg.do$b3diff)
b2b3.do.ftest <- var.test(avg.do$b2diff,avg.do$b3diff)

b1b2.co3.ftest <- var.test(avg.co3$b1diff,avg.co3$b2diff)
b1b3.co3.ftest <- var.test(avg.co3$b1diff,avg.co3$b3diff)
b2b3.co3.ftest <- var.test(avg.co3$b2diff,avg.co3$b3diff)


ftest.pvalues <- data.table(a=c("b1b2.dc","b1b3.dc","b2b3.dc",
                                "b1b2.do","b1b3.do","b2b3.do",
                                "b1b2.co3","b1b3.co3","b2b3.co3"),
                            pvalue=c(b1b2.dc.ftest$p.value,b1b3.dc.ftest$p.value,
                                     b2b3.dc.ftest$p.value,
                                     b1b2.do.ftest$p.value,b1b3.do.ftest$p.value,
                                     b2b3.do.ftest$p.value,
                                     b1b2.co3.ftest$p.value,b1b3.co3.ftest$p.value,
                                     b2b3.co3.ftest$p.value))
ftest.pvalues[pvalue < 0.05, sig:="significant"][pvalue > 0.1, sig:="not"]

sd(avg.dc$b1diff)-sd(avg.dc$b2diff)
sd(avg.do$b1diff)-sd(avg.do$b3diff)
sd(avg.do$b3diff)-sd(avg.do$b1diff)

##T-tests - Averages
b1.dc.avg.ttest <- t.test(avg.dc$B1.CC,avg.dc$B1.FF,paired=TRUE)
b2.dc.avg.ttest <- t.test(avg.dc$B2.CF,avg.dc$B2.FF,paired=TRUE)
b3.dc.avg.ttest <- t.test(avg.dc$B3.CC,avg.dc$B3.CF,paired=TRUE)

b1.do.avg.ttest <- t.test(avg.do$B1.CC,avg.do$B1.FF,paired=TRUE)
b2.do.avg.ttest <- t.test(avg.do$B2.CF,avg.do$B2.FF,paired=TRUE)
b3.do.avg.ttest <- t.test(avg.do$B3.CC,avg.do$B3.CF,paired=TRUE)

b1.co3.avg.ttest <- t.test(avg.co3$B1.CC,avg.co3$B1.FF,paired=TRUE)
b2.co3.avg.ttest <- t.test(avg.co3$B2.CF,avg.co3$B2.FF,paired=TRUE)
b3.co3.avg.ttest <- t.test(avg.co3$B3.CC,avg.co3$B3.CF,paired=TRUE)


ttest.avg.pvalues <- data.table(a=c("b1.dc","b2.dc","b3.dc",
                                    "b1.do","b2.do","b3.do",
                                    "b1.co3","b2.co3","b3.co3"),
                            pvalue=c(b1.dc.avg.ttest$p.value,b2.dc.avg.ttest$p.value,
                                     b3.dc.avg.ttest$p.value,
                                     b1.do.avg.ttest$p.value,b2.do.avg.ttest$p.value,
                                     b3.do.avg.ttest$p.value,
                                     b1.co3.avg.ttest$p.value,b2.co3.avg.ttest$p.value,
                                     b3.co3.avg.ttest$p.value),
                            estimate=c(b1.dc.avg.ttest$estimate,b2.dc.avg.ttest$estimate,
                                       b3.dc.avg.ttest$estimate,
                                       b1.do.avg.ttest$estimate,b2.do.avg.ttest$estimate,
                                       b3.do.avg.ttest$estimate,
                                       b1.co3.avg.ttest$estimate,b2.co3.avg.ttest$estimate,
                                       b3.co3.avg.ttest$estimate))
ttest.avg.pvalues[pvalue < 0.05, sig:="significant"][pvalue > 0.1, sig:="not"]


##T-tests - Standard deviations
b1.dc.stdev.ttest <- t.test(stdev.dc$B1.CC,stdev.dc$B1.FF,paired=TRUE)
b2.dc.stdev.ttest <- t.test(stdev.dc$B2.CF,stdev.dc$B2.FF,paired=TRUE)
b3.dc.stdev.ttest <- t.test(stdev.dc$B3.CC,stdev.dc$B3.CF,paired=TRUE)

b1.do.stdev.ttest <- t.test(stdev.do$B1.CC,stdev.do$B1.FF,paired=TRUE)
b2.do.stdev.ttest <- t.test(stdev.do$B2.CF,stdev.do$B2.FF,paired=TRUE)
b3.do.stdev.ttest <- t.test(stdev.do$B3.CC,stdev.do$B3.CF,paired=TRUE)

b1.co3.stdev.ttest <- t.test(stdev.co3$B1.CC,stdev.co3$B1.FF,paired=TRUE)
b2.co3.stdev.ttest <- t.test(stdev.co3$B2.CF,stdev.co3$B2.FF,paired=TRUE)
b3.co3.stdev.ttest <- t.test(stdev.co3$B3.CC,stdev.co3$B3.CF,paired=TRUE)



ttest.stdev.pvalues <- data.table(a=c("b1.dc","b2.dc","b3.dc",
                                    "b1.do","b2.do","b3.do",
                                    "b1.co3","b2.co3","b3.co3"),
                                pvalue=c(b1.dc.stdev.ttest$p.value,b2.dc.stdev.ttest$p.value,
                                         b3.dc.stdev.ttest$p.value,
                                         b1.do.stdev.ttest$p.value,b2.do.stdev.ttest$p.value,
                                         b3.do.stdev.ttest$p.value,
                                         b1.co3.stdev.ttest$p.value,b2.co3.stdev.ttest$p.value,
                                         b3.co3.stdev.ttest$p.value),
                                estimate=c(b1.dc.stdev.ttest$estimate,b2.dc.stdev.ttest$estimate,
                                           b3.dc.stdev.ttest$estimate,
                                           b1.do.stdev.ttest$estimate,b2.do.stdev.ttest$estimate,
                                           b3.do.stdev.ttest$estimate,
                                           b1.co3.stdev.ttest$estimate,b2.co3.stdev.ttest$estimate,
                                           b3.co3.stdev.ttest$estimate))
ttest.stdev.pvalues[pvalue < 0.05, sig:="significant"][pvalue > 0.1, sig:="not"]



# Publication Tests -------------------------------------------------------

agg_all <- psdata %>% group_by(Sample) %>% 
  summarize(d13C = mean(dC), 
            d18O = mean(dO), 
            CO3 = mean(CO3))

# Are the data normally distributed? 
hist(agg_all$d13C)
hist(agg_all$d18O)
hist(agg_all$CO3)
#oh dear...
shapiro.test(agg_all$d13C)
shapiro.test(agg_all$d18O)
shapiro.test(agg_all$CO3)
library(ggpubr)
ggqqplot(agg_all$d13C)
ggqqplot(agg_all$d18O)
ggqqplot(agg_all$CO3)
  
agg_B1CC <- B1.CC %>% group_by(`Tooth number`) %>% 
  summarize(dC = mean(dC), 
            dO = mean(dO), 
            CO3 = mean(CO3),
            Treat = 'CC', 
            Group = 1)

agg_B1FF <- B1.FF %>% group_by(`Tooth number`) %>% 
  summarize(dC = mean(dC), 
            dO = mean(dO), 
            CO3 = mean(CO3), 
            Treat = 'FF', 
            Group = 1)

agg_B2CF <- B2.CF %>% group_by(`Tooth number`) %>% 
  summarize(dC = mean(dC), 
            dO = mean(dO), 
            CO3 = mean(CO3), 
            Treat = 'CF', 
            Group = 2)

agg_B2FF <- B2.FF %>% group_by(`Tooth number`) %>% 
  summarize(dC = mean(dC), 
            dO = mean(dO), 
            CO3 = mean(CO3), 
            Treat = 'FF', 
            Group = 2)

agg_B3CC <- B3.CC %>% group_by(`Tooth number`) %>% 
  summarize(dC = mean(dC), 
            dO = mean(dO), 
            CO3 = mean(CO3), 
            Treat = 'CC', 
            Group = 3)

agg_B3CF <- B3.CF %>% group_by(`Tooth number`) %>% 
  summarize(dC = mean(dC), 
            dO = mean(dO), 
            CO3 = mean(CO3), 
            Treat = 'CF', 
            Group = 3)

##GJB Rather than this, the final datasets should be comprised of
##pairwise differences
#CCFF <- rbind(agg_B1CC, agg_B1FF)
#CFFF <- rbind(agg_B2CF, agg_B2FF)
#CCCF <- rbind(agg_B3CC, agg_B3CF)
CCFF = cbind(agg_B1CC$`Tooth number`, agg_B1CC[,2:4] - agg_B1FF[,2:4])
CFFF = cbind(agg_B2CF$`Tooth number`, agg_B2CF[,2:4] - agg_B2FF[,2:4])
CCCF = cbind(agg_B3CC$`Tooth number`, agg_B3CC[,2:4] - agg_B3CF[,2:4])

#CC-FF

##GJB Then the effect size is the mean or median of the differences
##and we can do a 1 sample test
#median(agg_B1CC$dC)
#median(agg_B1FF$dC)
#wilcox.test(CCFF$dC ~ CCFF$Treat)
median(CCFF$dC)
wilcox.test(CCFF$dC)

##GJB These normality tests are kinda pointless on these small sample
##sizes, but
hist(CCFF$dC)
shapiro.test(CCFF$dC)

##GJB If we group and center the difference values from the 3 experiments
##what does it look like?
DeltaC = c(CCFF$dC - mean(CCFF$dC), CCCF$dC - mean(CCCF$dC),
           CFFF$dC - mean(CFFF$dC))
hist(DeltaC)
shapiro.test(DeltaC)
##OK, not any better, so maybe sticking with nonparametric is best...

median(agg_B1CC$dO)
median(agg_B1FF$dO)
wilcox.test(CCFF$dO ~ CCFF$Treat)

median(agg_B1CC$CO3)
median(agg_B1FF$CO3)
wilcox.test(CCFF$CO3 ~ CCFF$Treat)
cohensD(agg_B1CC$CO3, agg_B1FF$CO3)

#CF-FF

median(agg_B2CF$dC)
median(agg_B2FF$dC)
wilcox.test(CFFF$dC ~ CFFF$Treat)

median(agg_B2CF$dO)
median(agg_B2FF$dO)
wilcox.test(CFFF$dO ~ CFFF$Treat)

median(agg_B2CF$CO3)
median(agg_B2FF$CO3)
wilcox.test(CFFF$CO3 ~ CFFF$Treat)
cohensD(agg_B2CF$CO3, agg_B2FF$CO3)

#CC-CF

median(agg_B3CC$dC)
median(agg_B3CF$dC)
wilcox.test(CCCF$dC ~ CCCF$Treat)

median(agg_B3CC$dO)
median(agg_B3CF$dO)
wilcox.test(CCCF$dO ~ CCCF$Treat)

median(agg_B3CC$CO3)
median(agg_B3CF$CO3)
wilcox.test(CCCF$CO3 ~ CCCF$Treat)
cohensD(agg_B3CC$CO3, agg_B3CF$CO3)

# Graphs ------------------------------------------------------------------

#boxplots of our three values 
C <- ggplot() + 
  geom_boxplot(data = CCFF, aes(x = Group, y = dC, fill = Treat)) +
  geom_boxplot(data = CFFF, aes(x = Group, y = dC, fill = Treat)) +
  geom_boxplot(data = CCCF, aes(x = Group, y = dC, fill = Treat)) +
  scale_fill_manual(values = c("#aed6dc","#ff9a8d","#4a536b")) + 
  labs(
  #  fill = "Particle Size", 
    y = expression(paste(delta^13, "C", " (\u2030, VPDB)"))
  ) + 
  theme_classic() + 
  theme(legend.position = 'none')

O <- ggplot() + 
  geom_boxplot(data = CCFF, aes(x = Group, y = dO, fill = Treat)) +
  geom_boxplot(data = CFFF, aes(x = Group, y = dO, fill = Treat)) +
  geom_boxplot(data = CCCF, aes(x = Group, y = dO, fill = Treat)) +
  scale_fill_manual(values = c("#aed6dc","#ff9a8d","#4a536b")) + 
  labs(
 #   fill = "Particle Size", 
    y = expression(paste(delta^18, "O", " (\u2030, VPDB)"))
  ) + 
  theme_classic() + 
  theme(legend.position = 'none')

CO3 <- ggplot() + 
  geom_boxplot(data = CCFF, aes(x = Group, y = CO3, fill = Treat)) +
  geom_boxplot(data = CFFF, aes(x = Group, y = CO3, fill = Treat)) +
  geom_boxplot(data = CCCF, aes(x = Group, y = CO3, fill = Treat)) +
  scale_fill_manual(values = c("#aed6dc","#ff9a8d","#4a536b")) + 
  labs(
#   fill = "Particle Size", 
    y = expression(paste("% CO"[3]))
  ) + 
  theme_classic() + 
  theme(legend.position = 'none')

ggarrange(C, O, CO3)
ggsave("Figures/ParticleSize.pdf")
