#Note: this is the newest code for the pretreatment experiment. 
library(openxlsx)

d1 = read.xlsx("data/211104_21-244.xlsx", 1)
d2 = read.xlsx("data/211110_21-250.xlsx", 1)
d3 = read.xlsx("data/221104_22-279.xlsx", 1)

d1$Batch = rep(211104)
d2$Batch = rep(211110)
d3$Batch = rep(221104)

d = rbind(d1, d2, d3)
#GRAPE Chris: does the line below do anything right now?
d = d[!(d$Identifier_1 %in% c("CARRARA", "MARBLE", "LSVEC", "MAR", 
                              "COND - CARRARA", "COND - LSVEC")),]

d$Treat = d$Tooth = d$TT = character(nrow(d))

for(i in 1:nrow(d)){
  d$Tooth[i] = strsplit(d$Identifier_1[i], "-")[[1]][1]
  d$Treat[i] = strsplit(d$Identifier_1[i], "-")[[1]][2]
  if(d$Batch[i] == 221104 & d$Treat[i] %in% c("F", "G", "I")){
    d$Treat[i] = paste0(d$Treat[i], "2")
  }
  d$TT[i] = paste(d$Tooth[i], d$Treat[i], sep = "-")
}

#remove some outliers
d = d[d$Identifier_1 != "C4-D-3",]
d = d[d$Identifier_1 != "C5-A-3",]

TT = unique(d$TT)

samps = data.frame(TT, "d13C.m" = rep(0), "d13C.sd" = rep(0),
                   "d18O.m" = rep(0), "d18O.sd" = rep(0), "n" = rep(0), 
                   #here's where I'm riffing, to try to compare CO3 offset
                   "CO3.m" = rep(0)
                   )
samps$Tooth = d$Tooth[match(samps$TT, d$TT)]
samps$Treat = d$Treat[match(samps$TT, d$TT)]

#sample averages
for(i in 1:length(TT)){
  samps$d13C.m[i] = mean(d$d13C.cal[d$TT == TT[i]], na.rm = TRUE)
  samps$d13C.sd[i] = sd(d$d13C.cal[d$TT == TT[i]], na.rm = TRUE)
  samps$d18O.m[i] = mean(d$d18O.cal[d$TT == TT[i]], na.rm = TRUE)
  samps$d18O.sd[i] = sd(d$d18O.cal[d$TT == TT[i]], na.rm = TRUE)
  #can I copy what Gabe did to get percent CO3?
  samps$CO3.m[i] = mean(d$pCO3[d$TT == TT[i]], na.rm = TRUE)
  samps$n[i] = sum(d$TT == TT[i], na.rm = TRUE)
}

#offsets
samps$d18O.sd.off = samps$d13C.sd.off = samps$d18O.m.off = samps$d13C.m.off = samps$CO3.m.off = rep(0)

sid = unique(samps$Tooth)
for(i in sid){
  samps$d13C.m.off[samps$Tooth == i & nchar(samps$Treat) == 1] = 
    samps$d13C.m[samps$Tooth == i & nchar(samps$Treat) == 1] - 
    samps$d13C.m[samps$Tooth == i & samps$Treat == "I"]
  samps$d18O.m.off[samps$Tooth == i & nchar(samps$Treat) == 1] = 
    samps$d18O.m[samps$Tooth == i & nchar(samps$Treat) == 1] - 
    samps$d18O.m[samps$Tooth == i & samps$Treat == "I"]
  samps$d13C.sd.off[samps$Tooth == i & nchar(samps$Treat) == 1] = 
    samps$d13C.sd[samps$Tooth == i & nchar(samps$Treat) == 1] - 
    samps$d13C.sd[samps$Tooth == i & samps$Treat == "I"]
  samps$d18O.sd.off[samps$Tooth == i & nchar(samps$Treat) == 1] = 
    samps$d18O.sd[samps$Tooth == i & nchar(samps$Treat) == 1] - 
    samps$d18O.sd[samps$Tooth == i & samps$Treat == "I"]
  samps$d13C.m.off[samps$Tooth == i & nchar(samps$Treat) == 2] = 
    samps$d13C.m[samps$Tooth == i & nchar(samps$Treat) == 2] - 
    samps$d13C.m[samps$Tooth == i & samps$Treat == "I2"]
  samps$d18O.m.off[samps$Tooth == i & nchar(samps$Treat) == 2] = 
    samps$d18O.m[samps$Tooth == i & nchar(samps$Treat) == 2] - 
    samps$d18O.m[samps$Tooth == i & samps$Treat == "I2"]
  samps$d13C.sd.off[samps$Tooth == i & nchar(samps$Treat) == 2] = 
    samps$d13C.sd[samps$Tooth == i & nchar(samps$Treat) == 2] - 
    samps$d13C.sd[samps$Tooth == i & samps$Treat == "I2"]
  samps$d18O.sd.off[samps$Tooth == i & nchar(samps$Treat) == 2] = 
    samps$d18O.sd[samps$Tooth == i & nchar(samps$Treat) == 2] - 
    samps$d18O.sd[samps$Tooth == i & samps$Treat == "I2"]
  #where I keep faking it til I make it
  samps$CO3.m.off[samps$Tooth == i & nchar(samps$Treat) == 1] = 
    samps$CO3.m[samps$Tooth == i & nchar(samps$Treat) == 1] - 
    samps$CO3.m[samps$Tooth == i & samps$Treat == "I"]  
  samps$CO3.m.off[samps$Tooth == i & nchar(samps$Treat) == 2] = 
    samps$CO3.m[samps$Tooth == i & nchar(samps$Treat) == 2] - 
    samps$CO3.m[samps$Tooth == i & samps$Treat == "I2"]  
}

samps.treats = samps[!(samps$Treat %in% c("I", "I2")),]

boxplot(d13C.m.off ~ Treat, samps.treats)
abline(0, 0)
png("~/d18O_oxy.png", width = 9, height = 5.5, units = "in", res = 600)
par("mar" = c(5.1, 5.1, 2.1, 2.1))
boxplot(d18O.m.off ~ Treat, samps.treats, xlab = "Treatment", 
        ylab = expression(delta^"18"*"O offset (vs control)"),
        names = c("A", "B", "C", "HCl", "NaOCl", "D", "E", "F", "Frep",
                  "G", "Grep", "H"))
abline(0, 0)
dev.off()


# Combining repeat measures? ----------------------------------------------
#can I just combine G and Grep, F and Frep and still sleep at night?
t.test(subset(samps.treats, Treat == "F")$d18O.m, subset(samps.treats, Treat == "F2")$d18O.m)
#F not statistically significant
t.test(subset(samps.treats, Treat == "G")$d18O.m, subset(samps.treats, Treat == "G2")$d18O.m)
#G not statistically significant

#I'm doing it I'm combining them. 
df <- samps.treats 
df$Treat <- recode(df$Treat, G2 = "G", F2 = "F", CA = "I", CB = "J")

# Tidyverse 4 lyfe
ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = df, aes(x = Treat, y = d18O.m.off, fill = Treat)) + 
  theme_classic() +
  scale_fill_viridis(option = "rocket", discrete = T) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16), ) +
  labs(x = "Treatment", 
       y = expression(delta^"18"*"O offset (vs. control)"))

ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = df, aes(x = Treat, y = d13C.m.off, fill = Treat)) + 
  theme_classic() +
  scale_fill_viridis(option = "rocket", discrete = T) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16), ) +
  labs(x = "Treatment", 
       y = expression(delta^"13"*"C offset (vs. control)"))

ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = df, aes(x = Treat, y = CO3.m.off, fill = Treat)) + 
  theme_classic() +
  scale_fill_viridis(option = "rocket", discrete = T) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16), ) +
  labs(x = "Treatment", 
       y = expression("% CO"[3]*" offset (vs. control)"))
