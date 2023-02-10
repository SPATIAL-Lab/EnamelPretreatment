# This supplementary .R file contains all R code needed to run the statistical 
# tests and create the base R figures as part of this publication. Please note 
# that some figures were altered post-R in Adobe Illustrator. 

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("readxl");usePackage("tidyverse"); usePackage("lsr")


# Particle Size Statistics ------------------------------------------------

psdata <- read_excel("data/ParticleSizeExp1.xlsx")

# Aggregate Replicates

psdata <- psdata %>% group_by(Sample) %>% 
  mutate(dC = mean(dC), 
            dO = mean(dO), 
            CO3 = mean(CO3)) %>% 
  ungroup()
psdata <- unique(psdata)

# Pull apart comparative batches and create delta values

CCFF <- psdata %>% filter(Tooth == "A" & Analysis == "1") %>% 
  filter(Treat == "FF" | Treat == "CC")
CFFF <- psdata %>% filter(Tooth == "A" & Analysis == "2") %>% 
  filter(Treat == "CF" | Treat == "FF")
CCCF <- psdata %>% filter(Tooth == "B") %>% 
  filter(Treat == "CC" | Treat == "CF")

CCFFdelta <- CCFF %>%
  group_by(toothNumber) %>%
  mutate(dC = dC[Treat == 'FF'] - dC[Treat == 'CC'], 
         dO = dO[Treat == 'FF'] - dO[Treat == 'CC'], 
         CO3 = CO3[Treat == 'FF'] - CO3[Treat == 'CC']) %>% 
  distinct(toothNumber, .keep_all = T) %>% 
  select(-c(Sample, Tooth, Treat, Analysis))

CFFFdelta <- CFFF %>%
  group_by(toothNumber) %>%
  mutate(dC = dC[Treat == 'FF'] - dC[Treat == 'CF'], 
         dO = dO[Treat == 'FF'] - dO[Treat == 'CF'], 
         CO3 = CO3[Treat == 'FF'] - CO3[Treat == 'CF']) %>% 
  distinct(toothNumber, .keep_all = T) %>% 
  select(-c(Sample, Tooth, Treat, Analysis))

CCCFdelta <- CCCF %>%
  group_by(toothNumber) %>%
  mutate(dC = dC[Treat == 'CC'] - dC[Treat == 'CF'], 
         dO = dO[Treat == 'CC'] - dO[Treat == 'CF'], 
         CO3 = CO3[Treat == 'CC'] - CO3[Treat == 'CF']) %>% 
  distinct(toothNumber, .keep_all = T) %>% 
  select(-c(Sample, Tooth, Treat, Analysis))

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
t.test(CFFFdelta$dC)
t.test(CFFFdelta$dO)
t.test(CFFFdelta$CO3)
t.test(CCCFdelta$dC)
t.test(CCCFdelta$dO)
t.test(CCCFdelta$CO3)


# Particle Size Figures ---------------------------------------------------


# Storage Conditions Statistics--------------------------------------------

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
  select(-c(time))

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

desiccator <- rbind(desiccatorT1, desiccatorT2) %>% 
  group_by(sample_id) %>% 
  mutate(dC = dC[time == '37 Days'] - dC[time == '71 Days'], 
         dO = dO[time == '37 Days'] - dO[time == '71 Days'], 
         CO3 = CO3[time == '37 Days'] - CO3[time == '71 Days']) %>% 
  distinct(sample_id, .keep_all = T) %>% 
  select(-c(time))

storage <- rbind(desiccator, ambient)

# Testing for normality

shapiro.test(storage$dC)
shapiro.test(storage$dO)
shapiro.test(storage$CO3)

# T-tests

t.test(storage$dC)
t.test(storage$dO)
t.test(storage$CO3)

t.test(dC ~ location, data = storage)
t.test(dO ~ location, data = storage)
t.test(CO3 ~ location, data = storage)

# Storage Conditions Figures ----------------------------------------------

ggplot(data = storage, aes(x = location, y = dC)) + 
         geom_boxplot() + 
         theme_classic()

ggplot(data = storage, aes(x = location, y = dO)) + 
  geom_boxplot() + 
  theme_classic()

ggplot(data = storage, aes(x = location, y = CO3)) + 
  geom_boxplot() + 
  theme_classic()



