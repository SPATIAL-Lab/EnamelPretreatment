library(openxlsx)

d1 = read.xlsx("~/GitHub/SIRFER/out/211104_21-244.xlsx", 1)
d2 = read.xlsx("~/GitHub/SIRFER/out/211110_21-250.xlsx", 1)
d3 = read.xlsx("~/GitHub/SIRFER/out/220920_22-243.xlsx", 1)

d1$Batch = rep(211104)
d2$Batch = rep(211110)
d3$Batch = rep(220920)

d = rbind(d1, d2, d3)
d = d[!(d$Identifier_1 %in% c("CARRARA", "MARBLE", "LSVEC", "MAR", 
                              "COND - CARRARA", "COND - LSVEC")),]

d$Treat = d$Tooth = d$TT = character(nrow(d))

for(i in 1:nrow(d)){
  d$Tooth[i] = strsplit(d$Identifier_1[i], "-")[[1]][1]
  d$Treat[i] = strsplit(d$Identifier_1[i], "-")[[1]][2]
  if(d$Batch[i] == 220920 & d$Treat[i] %in% c("F", "G")){
    d$Treat[i] = paste0(d$Treat[i], "2")
  }
  d$TT[i] = paste(d$Tooth[i], d$Treat[i], sep = "-")
}

#remove some outliers
d = d[d$Identifier_1 != "C4-D-3",]
d = d[d$Identifier_1 != "C5-A-3",]
d = d[!(d$TT == "C5-G2" & d$Weight == 0.271),]
d$d13C.cal[d$TT == "C2-CA" & d$Weight == 0.273] = NA

TT = unique(d$TT)

samps = data.frame(TT, "d13C.m" = rep(0), "d13C.sd" = rep(0),
                   "d18O.m" = rep(0), "d18O.sd" = rep(0), "n" = rep(0))
samps$Tooth = d$Tooth[match(samps$TT, d$TT)]
samps$Treat = d$Treat[match(samps$TT, d$TT)]

#sample averages
for(i in 1:length(TT)){
  samps$d13C.m[i] = mean(d$d13C.cal[d$TT == TT[i]], na.rm = TRUE)
  samps$d13C.sd[i] = sd(d$d13C.cal[d$TT == TT[i]], na.rm = TRUE)
  samps$d18O.m[i] = mean(d$d18O.cal[d$TT == TT[i]], na.rm = TRUE)
  samps$d18O.sd[i] = sd(d$d18O.cal[d$TT == TT[i]], na.rm = TRUE)
  samps$n[i] = sum(d$d13C.cal[d$TT == TT[i]], na.rm = TRUE)
}

#offsets
samps$d18O.sd.off = samps$d13C.sd.off = samps$d18O.m.off = samps$d13C.m.off = rep(0)
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
    samps$d13C.m[samps$Tooth == i & samps$Treat == "F2"] + 
    (samps$d13C.m[samps$Tooth == i & samps$Treat == "F"] - 
       samps$d13C.m[samps$Tooth == i & samps$Treat == "I"])
  samps$d18O.m.off[samps$Tooth == i & nchar(samps$Treat) == 2] = 
    samps$d18O.m[samps$Tooth == i & nchar(samps$Treat) == 2] - 
    samps$d18O.m[samps$Tooth == i & samps$Treat == "F2"] +
    (samps$d18O.m[samps$Tooth == i & samps$Treat == "F"] - 
       samps$d18O.m[samps$Tooth == i & samps$Treat == "I"])
  samps$d13C.sd.off[samps$Tooth == i & nchar(samps$Treat) == 2] = 
    samps$d13C.sd[samps$Tooth == i & nchar(samps$Treat) == 2] - 
    samps$d13C.sd[samps$Tooth == i & samps$Treat == "F2"] +
    (samps$d13C.sd[samps$Tooth == i & samps$Treat == "F"] - 
       samps$d13C.sd[samps$Tooth == i & samps$Treat == "I"])
  samps$d18O.sd.off[samps$Tooth == i & nchar(samps$Treat) == 2] = 
    samps$d18O.sd[samps$Tooth == i & nchar(samps$Treat) == 2] - 
    samps$d18O.sd[samps$Tooth == i & samps$Treat == "F2"] +
    (samps$d18O.sd[samps$Tooth == i & samps$Treat == "F"] - 
       samps$d18O.sd[samps$Tooth == i & samps$Treat == "I"])
}

samps.treats = samps[!(samps$Treat %in% c("I", "F2")),]

boxplot(d13C.m.off ~ Treat, samps.treats)
abline(0, 0)
par("mar" = c(5.1, 5.1, 2.1, 2.1))
boxplot(d18O.m.off ~ Treat, samps.treats, xlab = "Treatment", 
        ylab = expression(delta^"18"*"O offset (vs control)"),
        names = c("A", "B", "C", "Acid", "NaOCl", "D", "E", "F", "G",
                  "Grep", "H"))
abline(0, 0)
