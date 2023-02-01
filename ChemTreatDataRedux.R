#Note: this is the newest code for the pretreatment experiment. 
library(openxlsx); library(tidyverse); library(viridis); library(ggpubr)

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
# WARNING: while the control was labeled "I" in the original dataset, 
# I'm labeling "No oxidative treatment, buffered acetic acid 15 min" as "I" to continue the lettering scheme

df <- samps.treats 
df$Treat <- recode(df$Treat, G2 = "G", F2 = "F", CA = "I", CB = "J")
#remove all but df because I like a clean environment
rm(list=setdiff(ls(), "df"))

palette <- c( "#668C99","#306879", "#FD9C86", "#F47A60", "#668C99","#306879",
                       "#FD9C86", "#F47A60", "#FFF0DB", "#E4d5b7")

# Are the data normally distributed? 
hist(df$d18O.m.off)
hist(df$d13C.m.off)
hist(df$CO3.m.off)
#oh dear...
shapiro.test(df$d18O.m.off)
shapiro.test(df$d13C.m.off)
shapiro.test(df$CO3.m.off)
library(ggpubr)
ggqqplot(df$d18O.m.off)
ggqqplot(df$d13C.m.off)
ggqqplot(df$CO3.m.off)

# Some exploratory stats --------------------------------------------------

# is there a difference between NaOCl and H2O2, regardless of time?
df <- df %>% mutate(Group =
                     case_when(Treat == "A" | Treat == "B" | Treat == "E" | Treat == "F" ~ "NaOCl", 
                               Treat == "C" | Treat == "D" | Treat == "G" | Treat == "H" ~ "H2O2" 
                               )
              )

t.test(df$d13C.m.off ~ df$Group)
t.test(df$d18O.m.off ~ df$Group)
t.test(df$CO3.m.off ~ df$Group)

#nope, no differences there

# is there a difference between time of exposure to the oxidative treatment (15m versus 24h), regardless of solution used?
df <- df %>% mutate(Time =
                      case_when(Treat == "A" | Treat == "B" | Treat == "C" | Treat == "D" ~ "15min", 
                                Treat == "E" | Treat == "F" | Treat == "G" | Treat == "H" ~ "24h")
)

t.test(df$d13C.m.off ~ df$Time)
t.test(df$d18O.m.off ~ df$Time)
t.test(df$CO3.m.off ~ df$Time)
# ALMOST significant in terms of changing oxygen values (unsurprising?) but not quite. 
# 24 hours saw mean values of 0.48 offset, compared to 0.31 mean for 15 min group.

# UGHHHHHH let's do concentrations too just because although I can't imagine
df <- df %>% mutate(Conc =
                      case_when(Treat == "A" | Treat == "C" | Treat == "E" | Treat == "G" ~ "Low", 
                                Treat == "B" | Treat == "D" | Treat == "F" | Treat == "H" ~ "High" 
                      )
)

t.test(df$d13C.m.off ~ df$Conc)
t.test(df$d18O.m.off ~ df$Conc)
t.test(df$CO3.m.off ~ df$Conc)

# Tidyverse 4 lyfe
O <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = df, aes(x = Treat, y = d18O.m.off, fill = Treat, color = Time)) + 
  theme_classic() +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = c("#F05039", "#1F449C")) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16), ) +
  labs(x = "Treatment", 
       y = expression(delta^"18"*"O offset (vs control)"))

C <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = df, aes(x = Treat, y = d13C.m.off, fill = Treat, color = Time)) + 
  theme_classic() +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = c("#F05039", "#1F449C")) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16), ) +
  labs(x = "Treatment", 
       y = expression(delta^"13"*"C offset (vs control)"))

CO3 <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = df, aes(x = Treat, y = CO3.m.off, fill = Treat, color = Time)) + 
  theme_classic() +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = c("#F05039", "#1F449C")) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16), ) +
  labs(x = "Treatment", 
       y = expression("% CO"[3]*" offset (vs control)"))

ggarrange(C, O, CO3,
          ncol = 2, nrow = 2)
ggsave("Figures/Treatment.png", dpi = 300)

OGroup <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(df, !is.na(Group)), aes(x = Group, y = d18O.m.off, fill = Group)) + 
  theme_classic() +
  scale_fill_viridis(option = "rocket", discrete = T) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16), ) +
  labs(x = "Treatment", 
       y = expression(delta^"18"*"O offset (vs control)"))

CGroup <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(df, !is.na(Group)), aes(x = Group, y = d13C.m.off, fill = Group)) + 
  theme_classic() +
  scale_fill_viridis(option = "rocket", discrete = T) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16), ) +
  labs(x = "Treatment", 
       y = expression(delta^"13"*"C offset (vs control)"))

CO3Group <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(df, !is.na(Group)), aes(x = Group, y = CO3.m.off, fill = Group)) + 
  theme_classic() +
  scale_fill_viridis(option = "rocket", discrete = T) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16), ) +
  labs(x = "Treatment", 
       y = expression("% CO"[3]*" offset (vs control)"))

OTime <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(df, !is.na(Time)), aes(x = Time, y = d18O.m.off, fill = Time)) + 
  theme_classic() +
  scale_fill_viridis(option = "rocket", discrete = T) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16), ) +
  labs(x = "Treatment", 
       y = expression(delta^"18"*"O offset (vs control)"))

CTime <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(df, !is.na(Time)), aes(x = Time, y = d13C.m.off, fill = Time)) + 
  theme_classic() +
  scale_fill_viridis(option = "rocket", discrete = T) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16), ) +
  labs(x = "Treatment", 
       y = expression(delta^"13"*"C offset (vs control)"))

CO3Time <- ggplot() + 
  geom_hline(yintercept = 0, color = 'grey20', linetype = 2) +
  geom_boxplot(data = subset(df, !is.na(Time)), aes(x = Time, y = CO3.m.off, fill = Time)) + 
  theme_classic() +
  scale_fill_viridis(option = "rocket", discrete = T) +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16), ) +
  labs(x = "Treatment", 
       y = expression("% CO"[3]*" offset (vs control)"))

ggarrange(CGroup, OGroup, CO3Group, CTime, OTime, CO3Time, 
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 3, nrow = 2)





# do we need to go back and compare these values to the control, rather than setting everything as offset relative to control? 
# with this analysis run in two batches, comparing base values might not be appropriate actually. 

# What's the mean + SD of offsets for treatments A-H
mean(subset(df, Treat != "I" & Treat != "J")$d13C.m.off)
sd(subset(df, Treat != "I" & Treat != "J")$d13C.m.off)

mean(subset(df, Treat != "I" & Treat != "J")$d18O.m.off)
sd(subset(df, Treat != "I" & Treat != "J")$d18O.m.off)

mean(df$CO3.m.off)
sd(df$CO3.m.off)
