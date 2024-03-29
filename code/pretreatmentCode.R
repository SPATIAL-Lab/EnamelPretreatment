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
t.test(CCCFdelta$dC)
t.test(CCCFdelta$dO)
cohensD(CCCFdelta$dO)
t.test(CCCFdelta$CO3)
cohensD(CCCFdelta$CO3)

t.test(CCFFdelta$dC)
t.test(CCFFdelta$dO)
t.test(CCFFdelta$CO3)
cohensD(CCFFdelta$CO3)

t.test(CFFFdelta$dC)
t.test(CFFFdelta$dO)
t.test(CFFFdelta$CO3)
cohensD(CFFFdelta$CO3)

shapiro.test(c(CCFFdelta$dO, CCCFdelta$dO))
t.test(c(CCFFdelta$dO, CCCFdelta$dO)) #putting variables in c() allows them to group for one-sample t-test
cohensD(c(CCFFdelta$dO, CCCFdelta$dO))

# Particle Size Figures ---------------------------------------------------

PartSizeC <- ggplot() + 
  geom_hline(yintercept = 0, lty = 3) +
  geom_boxplot(data = CCFFdelta, aes(x = Trial, y = dC, fill = Trial)) +
  geom_boxplot(data = CFFFdelta, aes(x = Trial, y = dC, fill = Trial)) +
  geom_boxplot(data = CCCFdelta, aes(x = Trial, y = dC, fill = Trial)) +
  scale_fill_manual(values = c("#aed6dc","#ff9a8d","#4a536b")) + 
  labs(
    #  fill = "Particle Size", 
    x = "",
    y = expression(paste(Delta^13, "C", " (\u2030)"))
  ) + 
  theme_classic() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12))

PartSizeO <- ggplot() + 
  geom_hline(yintercept = 0, lty = 3) +
  geom_boxplot(data = CCFFdelta, aes(x = Trial, y = dO, fill = Trial)) +
  geom_boxplot(data = CFFFdelta, aes(x = Trial, y = dO, fill = Trial)) +
  geom_boxplot(data = CCCFdelta, aes(x = Trial, y = dO, fill = Trial)) +
  scale_fill_manual(values = c("#aed6dc","#ff9a8d","#4a536b")) + 
  labs(
    #   fill = "Particle Size",
    x = "",
    y = expression(paste(Delta^18, "O", " (\u2030)"))
  ) + 
  theme_classic() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12))

PartSizeCO3 <- ggplot() + 
  geom_hline(yintercept = 0, lty = 3) +
  geom_boxplot(data = CCFFdelta, aes(x = Trial, y = CO3, fill = Trial)) +
  geom_boxplot(data = CFFFdelta, aes(x = Trial, y = CO3, fill = Trial)) +
  geom_boxplot(data = CCCFdelta, aes(x = Trial, y = CO3, fill = Trial)) +
  scale_fill_manual(values = c("#aed6dc","#ff9a8d","#4a536b")) + 
  labs(
    #   fill = "Particle Size", 
    x = "",
    y = expression(paste(Delta, "% CO"[3]))
  ) + 
  theme_classic() + 
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12))

ggarrange(PartSizeC, PartSizeO, PartSizeCO3, nrow = 1)
ggsave("Figures/ParticleSize.pdf", dpi = 300, width = 6, height = 2.7,
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
# probably not used in the publication, but useful for quick examination

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
 
df <- read.csv('data/ChemicalPretreatment.csv')

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
diffTable = statTable = pTable = dTable = 
  data.frame(treats, "dC" = rep(0), "dO" = rep(0), "CO3" = rep(0))

for(i in seq_along(treats)){
  for(j in 1:3){
    if(shapTable[i, j+1] > 0.05){
      test = t.test(df[df$Treat == treats[i], datacols[j]])
      diffTable[i, j+1] = test$estimate
      statTable[i, j+1] = test$statistic
      pTable[i, j+1] = test$p.value
      dTable[i, j+1] = cohensD(df[df$Treat == treats[i], datacols[j]])
    }else{
      test = wilcox.test(df[df$Treat == treats[i], datacols[j]])
      diffTable[i, j+1] = mean(df[df$Treat == treats[i], datacols[j]])
      statTable[i, j+1] = test$statistic
      pTable[i, j+1] = test$p.value
      dTable[i, j+1] = cohensD(df[df$Treat == treats[i], datacols[j]])
    }
  }
}

shapTable
pTable
dTable
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
# ALMOST significant in terms of changing oxygen values (unsurprising?) but not quite. 
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
       y = expression(paste(Delta^18, "O", " (\u2030)")))

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
       y = expression(paste(Delta^13, "C", " (\u2030)")))

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
        axis.title = element_text(size = 14), ) +
  labs(x = "Oxidant Treatment", 
       y = expression(paste(Delta^18, "O", " (\u2030)")))

CGroup <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(graphs, !is.na(Group)), aes(x = Group, y = dC.off, fill = Group)) + 
  theme_classic() +
  scale_fill_manual(values = palette2) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), ) +
  labs(x = "Oxidant Treatment", 
       y = expression(paste(Delta^13, "C", " (\u2030)")))

CO3Group <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(graphs, !is.na(Group)), aes(x = Group, y = CO3.off, fill = Group)) + 
  theme_classic() +
  scale_fill_manual(values = palette2) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14), ) +
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
        axis.title = element_text(size = 14), ) +
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
        axis.title = element_text(size = 14), ) +
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
        axis.title = element_text(size = 14), ) +
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
        axis.title = element_text(size = 14), ) +
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
        axis.title = element_text(size = 14), ) +
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
        axis.title = element_text(size = 14), ) +
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
