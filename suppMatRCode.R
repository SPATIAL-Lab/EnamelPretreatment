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


