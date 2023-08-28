# The pre-treatment type nomenclature changed mid-experiment, and so some of the data
# needs to be redefined before analysis. In this script, we take the original datasheets and
# prepare them for analysis in pretreatmentCode.R

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("readxl");usePackage("tidyverse")

# Import and batch name
d1 = read_excel("data/211104_21-244.xlsx", 1)
d2 = read_excel("data/211110_21-250.xlsx", 1)
d3 = read_excel("data/221104_22-279.xlsx", 1)

d1$Batch = rep(211104)
d2$Batch = rep(211110)
d3$Batch = rep(221104)

d = rbind(d1, d2, d3)

# Identify Treatment Types
d$Treat = d$Tooth = d$TT = character(nrow(d))

for(i in 1:nrow(d)){
  d$Tooth[i] = strsplit(d$Identifier_1[i], "-")[[1]][1]
  d$Treat[i] = strsplit(d$Identifier_1[i], "-")[[1]][2]
  if(d$Batch[i] == 221104 & d$Treat[i] %in% c("F", "G", "I")){
    d$Treat[i] = paste0(d$Treat[i], "2")
  }
  d$TT[i] = paste(d$Tooth[i], d$Treat[i], sep = "-")
}

# Remove some outliers
d = d[d$Identifier_1 != "C4-D-3",]
d = d[d$Identifier_1 != "C5-A-3",]

TT = unique(d$TT)

samps = data.frame(TT, "dC" = rep(0), "dC.sd" = rep(0),
                   "dO" = rep(0), "dO.sd" = rep(0), "n" = rep(0), 
                   "CO3" = rep(0), "CO3.sd" = rep(0)
)
samps$Tooth = d$Tooth[match(samps$TT, d$TT)]
samps$Treat = d$Treat[match(samps$TT, d$TT)]

# Sample averages
for(i in 1:length(TT)){
  samps$dC[i] = mean(d$d13C.cal[d$TT == TT[i]], na.rm = TRUE)
  samps$dC.sd[i] = sd(d$d13C.cal[d$TT == TT[i]], na.rm = TRUE)
  samps$dO[i] = mean(d$d18O.cal[d$TT == TT[i]], na.rm = TRUE)
  samps$dO.sd[i] = sd(d$d18O.cal[d$TT == TT[i]], na.rm = TRUE)
  samps$CO3[i] = mean(d$pCO3[d$TT == TT[i]], na.rm = TRUE)
  samps$CO3.sd[i] = sd(d$pCO3[d$TT == TT[i]], na.rm = TRUE)
  samps$n[i] = sum(d$TT == TT[i], na.rm = TRUE)
}

# Turn CO3 yields into percent values
samps$CO3 = samps$CO3 * 100
samps$CO3.sd = samps$CO3.sd * 100

# Offsets
samps$dO.sd.off = samps$dC.sd.off = samps$dO.off = samps$dC.off = 
  samps$CO3.off = samps$CO3.sd.off =rep(0)

sid = unique(samps$Tooth)
for(i in sid){
  samps$dC.off[samps$Tooth == i & nchar(samps$Treat) == 1] = 
    samps$dC[samps$Tooth == i & nchar(samps$Treat) == 1] - 
    samps$dC[samps$Tooth == i & samps$Treat == "I"]
  samps$dO.off[samps$Tooth == i & nchar(samps$Treat) == 1] = 
    samps$dO[samps$Tooth == i & nchar(samps$Treat) == 1] - 
    samps$dO[samps$Tooth == i & samps$Treat == "I"]
  samps$dC.sd.off[samps$Tooth == i & nchar(samps$Treat) == 1] = 
    samps$dC.sd[samps$Tooth == i & nchar(samps$Treat) == 1] - 
    samps$dC.sd[samps$Tooth == i & samps$Treat == "I"]
  samps$dO.sd.off[samps$Tooth == i & nchar(samps$Treat) == 1] = 
    samps$dO.sd[samps$Tooth == i & nchar(samps$Treat) == 1] - 
    samps$dO.sd[samps$Tooth == i & samps$Treat == "I"]
  samps$dC.off[samps$Tooth == i & nchar(samps$Treat) == 2] = 
    samps$dC[samps$Tooth == i & nchar(samps$Treat) == 2] - 
    samps$dC[samps$Tooth == i & samps$Treat == "I2"]
  samps$dO.off[samps$Tooth == i & nchar(samps$Treat) == 2] = 
    samps$dO[samps$Tooth == i & nchar(samps$Treat) == 2] - 
    samps$dO[samps$Tooth == i & samps$Treat == "I2"]
  samps$dC.sd.off[samps$Tooth == i & nchar(samps$Treat) == 2] = 
    samps$dC.sd[samps$Tooth == i & nchar(samps$Treat) == 2] - 
    samps$dC.sd[samps$Tooth == i & samps$Treat == "I2"]
  samps$dO.sd.off[samps$Tooth == i & nchar(samps$Treat) == 2] = 
    samps$dO.sd[samps$Tooth == i & nchar(samps$Treat) == 2] - 
    samps$dO.sd[samps$Tooth == i & samps$Treat == "I2"]
  samps$CO3.off[samps$Tooth == i & nchar(samps$Treat) == 1] = 
    samps$CO3[samps$Tooth == i & nchar(samps$Treat) == 1] - 
    samps$CO3[samps$Tooth == i & samps$Treat == "I"]  
  samps$CO3.off[samps$Tooth == i & nchar(samps$Treat) == 2] = 
    samps$CO3[samps$Tooth == i & nchar(samps$Treat) == 2] - 
    samps$CO3[samps$Tooth == i & samps$Treat == "I2"]  
  samps$CO3.sd.off[samps$Tooth == i & nchar(samps$Treat) == 1] = 
    samps$CO3.sd[samps$Tooth == i & nchar(samps$Treat) == 1] - 
    samps$CO3.sd[samps$Tooth == i & samps$Treat == "I"]
  samps$CO3.sd.off[samps$Tooth == i & nchar(samps$Treat) == 2] = 
    samps$CO3.sd[samps$Tooth == i & nchar(samps$Treat) == 2] - 
    samps$CO3.sd[samps$Tooth == i & samps$Treat == "I2"]
}

# Renaming controls
samps$Treat <- gsub("I2", "Control", samps$Treat)
samps$Treat <- gsub("I", "Control", samps$Treat)

# Combining repeat measures?
F12 <- samps %>% 
  group_by(Tooth) %>% 
  mutate(dC = dC.off[Treat == 'F2'] - dC.off[Treat == 'F'], 
         dO = dO.off[Treat == 'F2'] - dO.off[Treat == 'F'], 
         CO3 = CO3.off[Treat == 'F2'] - CO3.off[Treat == 'F']) %>% 
  distinct(Tooth, .keep_all = T) %>% 
  select(-c(Treat))

G12 <- samps %>% 
  group_by(Tooth) %>% 
  mutate(dC = dC.off[Treat == 'G2'] - dC.off[Treat == 'G'], 
         dO = dO.off[Treat == 'G2'] - dO.off[Treat == 'G'], 
         CO3 = CO3.off[Treat == 'G2'] - CO3.off[Treat == 'G']) %>% 
  distinct(Tooth, .keep_all = T) %>% 
  select(-c(Treat))

shapiro.test(F12$dC)
shapiro.test(F12$dO)
shapiro.test(F12$CO3)

shapiro.test(G12$dC)
shapiro.test(G12$dO)
shapiro.test(G12$CO3)

t.test(F12$dC)
t.test(F12$dO)
t.test(F12$CO3)

t.test(G12$dC)
t.test(G12$dO)
t.test(G12$CO3)

# WARNING: while the control was labeled "I" in the original dataset, 
# I'm labeling "No oxidative treatment, buffered acetic acid 15 min" as "I" to continue the lettering scheme

df <- samps 
df$Treat <- recode(df$Treat, G2 = "G", F2 = "F", CA = "I", CB = "J")

write.csv(df, file = 'data/ChemicalPretreatment.csv')
