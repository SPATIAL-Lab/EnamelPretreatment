# This supplementary .R file contains all R code needed to run the statistical 
# tests and create the base R figures as part of this publication. Please note 
# that some figures were altered post-R in Adobe Illustrator. 

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("readxl");usePackage("tidyverse"); usePackage("lsr"); usePackage("ggpubr")

# Particle Size Statistics ------------------------------------------------

psdata <- read_excel("data/ParticleSize.xlsx")

# Aggregate Replicates

psdata <- psdata %>% group_by(Sample) %>% 
  mutate(dC.sd = sd(dC),
         dO.sd = sd(dO),
         CO3.sd = sd(CO3),
         dC = mean(dC),
         dO = mean(dO),
         CO3 = mean(CO3),
         Weight = NULL)
psdata <- unique(psdata)

# Pull apart comparative batches and create delta values

CCFF <- psdata %>% filter(Trial == "CCFF")
CFFF <- psdata %>% filter(Trial == "CFFF")
CCCF <- psdata %>% filter(Trial == "CCCF")

CCFFdelta <- CCFF %>%
  group_by(toothNumber) %>%
  mutate(dC = dC[Treat == 'CC'] - dC[Treat == 'FF'], 
         dO = dO[Treat == 'CC'] - dO[Treat == 'FF'], 
         CO3 = CO3[Treat == 'CC'] - CO3[Treat == 'FF']) %>% 
  distinct(toothNumber, .keep_all = T) %>% 
  select(-c(Sample, Treat, dC.sd, dO.sd, CO3.sd))

CFFFdelta <- CFFF %>%
  group_by(toothNumber) %>%
  mutate(dC = dC[Treat == 'CF'] - dC[Treat == 'FF'], 
         dO = dO[Treat == 'CF'] - dO[Treat == 'FF'], 
         CO3 = CO3[Treat == 'CF'] - CO3[Treat == 'FF']) %>% 
  distinct(toothNumber, .keep_all = T) %>% 
  select(-c(Sample, Treat, dC.sd, dO.sd, CO3.sd))

CCCFdelta <- CCCF %>%
  group_by(toothNumber) %>%
  mutate(dC = dC[Treat == 'CC'] - dC[Treat == 'CF'], 
         dO = dO[Treat == 'CC'] - dO[Treat == 'CF'], 
         CO3 = CO3[Treat == 'CC'] - CO3[Treat == 'CF']) %>% 
  distinct(toothNumber, .keep_all = T) %>% 
  select(-c(Sample, Treat, dC.sd, dO.sd, CO3.sd))

# Testing for normality

shapiro.test(CCFFdelta$dC)
shapiro.test(CCFFdelta$dO)
shapiro.test(CCFFdelta$CO3)
shapiro.test(CFFFdelta$dC)
shapiro.test(CFFFdelta$dO)
shapiro.test(CFFFdelta$CO3)
shapiro.test(CCCFdelta$dC)
shapiro.test(CCCFdelta$dO)
shapiro.test(CCCFdelta$CO3)

# No tests result in p < 0.05, we'll move forward with t-tests

t.test(CCFFdelta$dC)
t.test(CCFFdelta$dO)
t.test(CCFFdelta$CO3)

t.test(CCCFdelta$dC)
t.test(CCCFdelta$dO)
t.test(CCCFdelta$CO3)

t.test(CFFFdelta$dC)
t.test(CFFFdelta$dO)
t.test(CFFFdelta$CO3)

shapiro.test(c(CCFFdelta$dO, CCCFdelta$dO))
t.test(c(CCFFdelta$dO, CCCFdelta$dO))

# Particle Size Figures ---------------------------------------------------

PartSizeC <- ggplot() + 
  geom_hline(yintercept = 0, lty = 3) +
  geom_boxplot(data = CCFFdelta, aes(x = Group, y = dC, fill = Group)) +
  geom_boxplot(data = CFFFdelta, aes(x = Group, y = dC, fill = Group)) +
  geom_boxplot(data = CCCFdelta, aes(x = Group, y = dC, fill = Group)) +
  scale_fill_manual(values = c("#aed6dc","#ff9a8d","#4a536b")) + 
  labs(
    #  fill = "Particle Size", 
    x = "",
    y = expression(paste(Delta^13, "C", " (\u2030, VPDB)"))
  ) + 
  theme_classic() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12))

PartSizeO <- ggplot() + 
  geom_hline(yintercept = 0, lty = 3) +
  geom_boxplot(data = CCFFdelta, aes(x = Group, y = dO, fill = Group)) +
  geom_boxplot(data = CFFFdelta, aes(x = Group, y = dO, fill = Group)) +
  geom_boxplot(data = CCCFdelta, aes(x = Group, y = dO, fill = Group)) +
  scale_fill_manual(values = c("#aed6dc","#ff9a8d","#4a536b")) + 
  labs(
    #   fill = "Particle Size",
    x = "",
    y = expression(paste(Delta^18, "O", " (\u2030, VPDB)"))
  ) + 
  theme_classic() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12))

PartSizeCO3 <- ggplot() + 
  geom_hline(yintercept = 0, lty = 3) +
  geom_boxplot(data = CCFFdelta, aes(x = Group, y = CO3, fill = Group)) +
  geom_boxplot(data = CFFFdelta, aes(x = Group, y = CO3, fill = Group)) +
  geom_boxplot(data = CCCFdelta, aes(x = Group, y = CO3, fill = Group)) +
  scale_fill_manual(values = c("#aed6dc","#ff9a8d","#4a536b")) + 
  labs(
    #   fill = "Particle Size", 
    x = "",
    y = expression(paste(Delta, "% CO"[3]))
  ) + 
  theme_classic() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12))

ggarrange(PartSizeC, PartSizeO, PartSizeCO3, nrow = 1)
ggsave("Figures/ParticleSize.pdf", dpi = 300, width = 6, height = 2.8,
       units = c("in"))

# Storage Conditions Statistics--------------------------------------------

StorageCondExp <- read_excel("data/StorageCondExp.xlsx")
StorageCondExp$sample_id <- substr(StorageCondExp$`Sample ID`, 7, 11)
scdata <- separate(StorageCondExp, "Sample ID", sep="-",into=c("SLC20","tooth",
                                                               "AB","treat"))
scdata$sample_id <- gsub("-", "", scdata$sample_id)

ambientT1 <- filter(scdata,AB=="A",Time=="T1") %>% 
  group_by(tooth) %>% 
  summarize(dC.sd = sd(dC),
            dO.sd = sd(dO),
            CO3.sd = sd(CO3),
            dC = mean(dC), 
            dO = mean(dO), 
            CO3 = mean(CO3)) %>%  
  mutate(time = "37 Days", 
         location = "Cabinet")

ambientT2 <- filter(scdata,AB=="A",Time=="T2") %>% 
  group_by(tooth) %>% 
  summarize(dC.sd = sd(dC),
            dO.sd = sd(dO),
            CO3.sd = sd(CO3),
            dC = mean(dC), 
            dO = mean(dO), 
            CO3 = mean(CO3)) %>%  
  mutate(time = "71 Days", 
         location = "Cabinet")

desiccatorT1 <- filter(scdata,AB=="B",Time=="T1") %>% 
  group_by(tooth) %>% 
  summarize(dC.sd = sd(dC),
            dO.sd = sd(dO),
            CO3.sd = sd(CO3),
            dC = mean(dC), 
            dO = mean(dO), 
            CO3 = mean(CO3)) %>%  
  mutate(time = "37 Days", 
         location = "Desiccator")

desiccatorT2 <- filter(scdata,AB=="B",Time=="T2") %>% 
  group_by(tooth) %>% 
  summarize(dC.sd = sd(dC),
            dO.sd = sd(dO),
            CO3.sd = sd(CO3),
            dC = mean(dC), 
            dO = mean(dO), 
            CO3 = mean(CO3)) %>%  
  mutate(time = "71 Days", 
         location = "Desiccator")

T1 <- rbind(ambientT1, desiccatorT1) %>% 
  group_by(tooth) %>% 
  mutate(dC = dC[location == 'Cabinet'] - dC[location == 'Desiccator'], 
         dO = dO[location == 'Cabinet'] - dO[location == 'Desiccator'], 
         CO3 = CO3[location == 'Cabinet'] - CO3[location == 'Desiccator']) %>% 
  distinct(tooth, .keep_all = T) %>% 
  select(-c(location, dC.sd, dO.sd, CO3.sd))

T2 <- rbind(ambientT2, desiccatorT2) %>% 
  group_by(tooth) %>% 
  mutate(dC = dC[location == 'Cabinet'] - dC[location == 'Desiccator'], 
         dO = dO[location == 'Cabinet'] - dO[location == 'Desiccator'], 
         CO3 = CO3[location == 'Cabinet'] - CO3[location == 'Desiccator']) %>% 
  distinct(tooth, .keep_all = T) %>% 
  select(-c(location, dC.sd, dO.sd, CO3.sd))

# Testing for normality

shapiro.test(T1$dC)
shapiro.test(T1$dO)
shapiro.test(T1$CO3)

shapiro.test(T2$dC)
shapiro.test(T2$dO)
shapiro.test(T2$CO3)

# T-tests

t.test(T1$dC)
t.test(T1$dO)
t.test(T1$CO3)

t.test(T2$dC)
wilcox.test(T2$dO)
t.test(T2$CO3)


# Storage Conditions Figures ----------------------------------------------
# not used in the publication, but useful for quick examination

storage2 <- rbind(T1, T2)

ggplot(data = storage2, aes(x = time, y = dC)) + 
  geom_hline(yintercept = 0, lty = 3) +
  geom_boxplot() + 
  theme_classic()

ggplot(data = storage2, aes(x = time, y = dO)) + 
  geom_hline(yintercept = 0, lty = 3) +
  geom_boxplot() + 
  theme_classic()

ggplot(data = storage2, aes(x = time, y = CO3)) + 
  geom_hline(yintercept = 0, lty = 3) +
  geom_boxplot() + 
  theme_classic()

# Chemical Treatment Statistics -------------------------------------------
 
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

# Combining repeat measures

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
# we're labeling "No oxidative treatment, buffered acetic acid 15 min" as "I" to continue the lettering scheme

df <- samps 
df$Treat <- recode(df$Treat, G2 = "G", F2 = "F", CA = "I", CB = "J")

# Testing for normality

treats = sort(unique(df$Treat))
treats = treats[treats != "Control"]
shapTable = data.frame(treats, "dC" = rep(0), "dO" = rep(0), "CO3" = rep(0))
datacols = c("dC.off", "dO.off", "CO3.off")

for(i in seq_along(treats)){
  for(j in 1:3){
    shapTable[i, j+1] = shapiro.test(df[df$Treat == treats[i], datacols[j]])$p.value
  }
}

# Stats for all

diffTable = statTable = pTable = data.frame(treats, "dC" = rep(0), "dO" = rep(0), "CO3" = rep(0))

for(i in seq_along(treats)){
  for(j in 1:3){
    if(shapTable[i, j+1] > 0.05){
      test = t.test(df[df$Treat == treats[i], datacols[j]])
      diffTable[i, j+1] = test$estimate
      statTable[i, j+1] = test$statistic
      pTable[i, j+1] = test$p.value
    }else{
      test = wilcox.test(df[df$Treat == treats[i], datacols[j]])
      diffTable[i, j+1] = mean(df[df$Treat == treats[i], datacols[j]])
      statTable[i, j+1] = test$statistic
      pTable[i, j+1] = test$p.value
    }
  }
}

shapTable
pTable
diffTable
statTable

mean(diffTable[1:8, "dO"])
mean(diffTable[pTable$dO < 0.05, "dO"])

# Is there a difference between NaOCl and H2O2, regardless of time?
df <- df %>% mutate(Group =
                      case_when(Treat == "A" | Treat == "B" | Treat == "E" | Treat == "F" ~ "NaOCl", 
                                Treat == "C" | Treat == "D" | Treat == "G" | Treat == "H" ~ "H2O2" 
                      )
)

t.test(df$dC.off ~ df$Group)
t.test(df$dO.off ~ df$Group)
t.test(df$CO3.off ~ df$Group)

# Is there a difference between time of exposure to the oxidative treatment (15m versus 24h), regardless of solution used?
df <- df %>% mutate(Time =
                      case_when(Treat == "A" | Treat == "B" | Treat == "C" | Treat == "D" ~ "15min", 
                                Treat == "E" | Treat == "F" | Treat == "G" | Treat == "H" ~ "24h")
)

t.test(df$dC.off ~ df$Time)
t.test(df$dO.off ~ df$Time)
t.test(df$CO3.off ~ df$Time)
# 24 hours saw mean values of 0.48 offset, compared to 0.31 mean for 15 min group.

# Concentrations
df <- df %>% mutate(Conc =
                      case_when(Treat == "A" | Treat == "C" | Treat == "E" | Treat == "G" ~ "Low", 
                                Treat == "B" | Treat == "D" | Treat == "F" | Treat == "H" ~ "High" 
                      )
)

t.test(df$dC.off ~ df$Conc)
t.test(df$dO.off ~ df$Conc)
t.test(df$CO3.off ~ df$Conc)
#no significant values there either

# Chemical Treatment Figures---------------------------------------------------

graphs <- subset(df, Treat!= 'Control')
palette <- c( "#668C99","#306879", "#FD9C86", "#F47A60", "#668C99","#306879",
                       "#FD9C86", "#F47A60", "#FFF0DB", "#E4d5b7")
                       
palette2 <- c("#E4d5b7", "#306879")

O <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = graphs, aes(x = Treat, y = dO.off, fill = Treat, color = Time)) + 
  theme_classic() +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = c("#F05039", "#1F449C")) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), ) +
  labs(x = "Treatment", 
       y = expression(paste(Delta^18, "O", " (\u2030, VPDB)")))

C <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = graphs, aes(x = Treat, y = dC.off, fill = Treat, color = Time)) + 
  theme_classic() +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = c("#F05039", "#1F449C")) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), ) +
  labs(x = "Treatment", 
       y = expression(paste(Delta^13, "C", " (\u2030, VPDB)")))

CO3 <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = graphs, aes(x = Treat, y = CO3.off, fill = Treat, color = Time)) + 
  theme_classic() +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = c("#F05039", "#1F449C")) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), ) +
  labs(x = "Treatment", 
       y = expression(paste(Delta, "% CO"[3])))

ggarrange(C, O, CO3,
          ncol = 2, nrow = 2)
ggsave("Figures/Treatment.pdf", dpi = 300, width = 6, height = 5, units = c('in'))

OGroup <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(graphs, !is.na(Group)), aes(x = Group, y = dO.off, fill = Group)) + 
  theme_classic() +
  scale_fill_manual(values = palette2) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12), ) +
  labs(x = "Oxidant Treatment", 
       y = expression(paste(Delta^18, "O", " (\u2030, VPDB)")))

CGroup <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(graphs, !is.na(Group)), aes(x = Group, y = dC.off, fill = Group)) + 
  theme_classic() +
  scale_fill_manual(values = palette2) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12), ) +
  labs(x = "Oxidant Treatment", 
       y = expression(paste(Delta^13, "C", " (\u2030, VPDB)")))

CO3Group <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(graphs, !is.na(Group)), aes(x = Group, y = CO3.off, fill = Group)) + 
  theme_classic() +
  scale_fill_manual(values = palette2) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12), ) +
  labs(x = "Oxidant Treatment", 
       y = expression(paste(Delta, "% CO"[3])))

OTime <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(graphs, !is.na(Time)), aes(x = Time, y = dO.off, fill = Time)) + 
  theme_classic() +
  scale_fill_manual(values = palette2) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12), ) +
  labs(x = "Time", 
       y = "")

CTime <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(graphs, !is.na(Time)), aes(x = Time, y = dC.off, fill = Time)) + 
  theme_classic() +
  scale_fill_manual(values = palette2) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12), ) +
  labs(x = "Time", 
       y = "")

CO3Time <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(graphs, !is.na(Time)), aes(x = Time, y = CO3.off, fill = Time)) + 
  theme_classic() +
  scale_fill_manual(values = palette2) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12), ) +
  labs(x = "Time", 
       y = "")

OConc <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(graphs, !is.na(Time)), aes(x = Conc, y = dO.off, fill = Conc)) + 
  theme_classic() +
  scale_fill_manual(values = palette2) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12), ) +
  labs(x = "Concentration", 
       y = "")

CConc <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(graphs, !is.na(Time)), aes(x = Conc, y = dC.off, fill = Conc)) + 
  theme_classic() +
  scale_fill_manual(values = palette2) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12), ) +
  labs(x = "Concentration", 
       y = "")

CO3Conc <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(graphs, !is.na(Time)), aes(x = Conc, y = CO3.off, fill = Conc)) + 
  theme_classic() +
  scale_fill_manual(values = palette2) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12), ) +
  labs(x = "Concentration", 
       y = "")

ggarrange(CGroup, CTime, CConc,
          OGroup, OTime,  OConc,
          CO3Group, CO3Time,  CO3Conc, 
          ncol = 3, nrow = 3)
ggsave("Figures/Treatment2.pdf", dpi = 300, height = 6, width = 6, units = "in")

# Replicate SD across all trials

mean(c(psdata$dC.sd, ambientT1$dC.sd, ambientT1$dC.sd, desiccatorT1$dC.sd,
       desiccatorT2$dC.sd, samps$dC.sd))
mean(c(psdata$dO.sd, ambientT1$dO.sd, ambientT1$dO.sd, desiccatorT1$dO.sd,
       desiccatorT2$dO.sd, samps$dO.sd))
