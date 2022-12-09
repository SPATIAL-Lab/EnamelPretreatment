library(readxl)
psdata <- read_excel("data/ParticleSizeExp1.xlsx")

library(tidyr)
library(dplyr)
library(data.table)

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


