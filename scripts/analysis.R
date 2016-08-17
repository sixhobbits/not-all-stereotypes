
     ########################################################################
     ##  Overview of experimental design:
     ##
     ##   2 x 2 within-subjects design
     ##      Stereotype Consistency (+stereotypical, –stereotypical)
     ##      Speaker-specific Knowledge Consistency (+knowledge, –knowledge)
     ##   40 Experimental items (in 4 conditions)
     ##      pS.pK
     ##      pS.mK
     ##      mS.pK
     ##      mS.mK
     ##   60 Fillers
     ##   DV: RT for critical word (CW) and subsequent word (CW+1)
     ##   Participants: Plan to run 52 participants
     ########################################################################


#-------------------------------------------------------------------------------
# Read data
#-------------------------------------------------------------------------------

# clear memory and load packages
rm(list=ls())
library(ez)
library(ggplot2)
#library(plyr)

# set paths
CodeDir = '~/Documents/WORK IN PROGRESS/Saarland/2016 Summer/ExptMethods/CourseExpt/Analysis/Code/'
DataDir = '~/Documents/WORK IN PROGRESS/Saarland/2016 Summer/ExptMethods/CourseExpt/Analysis/Data/'

# set working directory and read data
setwd(DataDir)
d0 <- read.table("data.clean.txt", sep="\t", header=T, fill=T)  # 49245 x 27

#-------------------------------------------------------------------------------
# Explore data
#-------------------------------------------------------------------------------

# compactly display the structure of the data
str(d0)
summary(d0)
ezPrecis(d0) # structure summary using ez library

head(d0)
tail(d0)
head(arrange(d0, Word.RT))
tail(arrange(d0, Word.RT))

# create subset that excludes Fillers and contains only cw, cw.m1, cw.p1 
d1 <- subset(d0, Cond != "fillers" & 
                  (Word.number=="cw.m1" |
                        Word.number=="cw" |
                        Word.number=="cw.p1" |
                        Word.number=="cw.p2")) # 6084 x 27

# Confirm that...
xtabs(~ Subj + Cond, d1)  # each Subj has 10 Items per condition
xtabs(~ Item + Cond, d1)  # each Item has 10 Items per condition
xtabs(~ Subj + Item, d1)  # each Subj saw each Item once (3 = 3 RTs per item)

# visualize balance of data using ez library
ezDesign(d1, x=Cond, y=Subj) 
ezDesign(d1, x=Item, y=Subj)  # 3 = 3 RTs per item
ezDesign(d1, x=Cond, y=Item)  # 39 = 13 subs x 3 RTs 
ezDesign(d1, x=Subj, y=DesignNum)  

head(arrange(d1, Word.RT))
tail(arrange(d1, Word.RT))

#-------------------------------------------------------------------------------
# Visualize raw data
#-------------------------------------------------------------------------------

# Histograms - RTs for CW for all Subs by Cond
with(subset(d1, Cond=="pS.pK" & Word.number=="cw"),
     hist(Word.RT, Item, xlim=c(0,700), breaks=14, # 50 msec bins
          col=rgb(red=0, green=0, blue=1, alpha=0.7), border=F, # alpha = transparency
          xlab="RT (msec)", ylab="Frequency",  
          main="RT for critical word by Item"))
with(subset(d1, Cond=="pS.mK" & Word.number=="cw"),
     hist(Word.RT, Item, xlim=c(0,700), breaks=14, 
          col=rgb(0,1,0, alpha=0.7), border=F, add=T))  # add to previous plot
with(subset(d1, Cond=="mS.pK" & Word.number=="cw"),
     hist(Word.RT, Item, xlim=c(0,700), breaks=14, 
          col=rgb(1,0,1, alpha=0.7), border=F, add=T))
with(subset(d1, Cond=="mS.mK" & Word.number=="cw"),
     hist(Word.RT, Item, xlim=c(0,700), breaks=14, 
          col=rgb(1,0,0, alpha=0.7), border=F, add=T))

# Density plots
# all Conds
ggplot(subset(d1, Word.number=="cw"),   # uses ggplot2 library
       aes(Word.RT, fill = Cond)) + geom_density(alpha = 0.2) + 
     scale_fill_manual(values=c("red","purple","green","blue"))

# Cond pairs
ggplot(subset(d1, Word.number=="cw" & (Cond=="pS.pK" | Cond=="mS.mK")), 
       aes(Word.RT, fill = Cond)) + geom_density(alpha = 0.2) + 
     scale_fill_manual(values=c("red", "blue"))
ggplot(subset(d1, Word.number=="cw" & (Cond=="pS.pK" | Cond=="pS.mK")), 
       aes(Word.RT, fill = Cond)) + geom_density(alpha = 0.2) + 
     scale_fill_manual(values=c("green","blue"))
ggplot(subset(d1, Word.number=="cw" & (Cond=="pS.pK" | Cond=="mS.pK")), 
       aes(Word.RT, fill = Cond)) + geom_density(alpha = 0.2) + 
     scale_fill_manual(values=c("purple","blue"))
ggplot(subset(d1, Word.number=="cw" & (Cond=="pS.mK" | Cond=="mS.pK")), 
       aes(Word.RT, fill = Cond)) + geom_density(alpha = 0.2) + 
     scale_fill_manual(values=c("purple","green"))


# quick plot - RTs by Cond  (qplot uses ggplot2 library
# as box plots
qplot(Cond, Word.RT, data=subset(d1, Word.number=="cw"), 
      geom=c("boxplot"), col=Cond)  
# data as points
qplot(Cond, Word.RT, data=subset(d1, Word.number=="cw"), 
      geom=c("point"), col=Cond)    
# just pS.pK and mS.mK
qplot(Cond, Word.RT, 
      data=subset(d1, Word.number=="cw" & (Cond=="pS.pK" | Cond=="mS.mK")), 
      geom=c("point"), col=Cond)  


# RTs by Subj by Cond
qplot(Cond, Word.RT, data=subset(d1, Word.number=="cw"), 
      geom=c("boxplot"), facets= ~ Subj, col=Cond)
qplot(Cond, Word.RT, data=subset(d1, Word.number=="cw"), 
      geom=c("point"), facets= ~ Subj, col=Cond)



#-------------------------------------------------------------------------------
# Aggregate data by-Subjects 
#-------------------------------------------------------------------------------

# get function for aggregating 
#source('summarySE.R')  # assuming summarySE.R lives in current directory
source(sprintf('%s%s', CodeDir, "summarySE.R"))


#--- By Subjects ---------
# aggregate RTs and calculate mean RT and SD for each subject 
bySubj <- summarySE(d1, measurevar="Word.RT", 
                    groupvars=c("Subj","Cond","Word.number"), 
                    na.rm=T)  # 24 x 8
bySubj$Subj <- as.factor(bySubj$Subj)

# group Conditions by IV (S and K)
bySubj$Stereotypicality <- NA
bySubj$Knowledge <- NA
bySubj$Stereotypicality[bySubj$Cond=="pS.pK" | bySubj$Cond=="pS.mK"] <- "pS"
bySubj$Stereotypicality[bySubj$Cond=="mS.pK" | bySubj$Cond=="mS.mK"] <- "mS"
bySubj$Knowledge[bySubj$Cond=="pS.pK" | bySubj$Cond=="mS.pK"] <- "pK"
bySubj$Knowledge[bySubj$Cond=="pS.mK" | bySubj$Cond=="mS.mK"] <- "mK"
# 24 x 10


# quick plot -- RTs -- CW
qplot(Cond, Word.RT, data=subset(bySubj, Word.number=="cw"),   # per cond
      geom=c("boxplot"), col=Cond) 
qplot(Subj, Word.RT, data=subset(bySubj, Word.number=="cw"),   # per sub    
      geom=c("boxplot"))          
# pS vs mS (collapsing across pK and mK conditions)
qplot(Stereotypicality, Word.RT, data=subset(bySubj, Word.number=="cw"),  
      geom=c("boxplot"), col=Stereotypicality) 
# pK vs mK (collapsing across pS and mS conditions)
qplot(Knowledge, Word.RT, data=subset(bySubj, Word.number=="cw"),   
      geom=c("boxplot"), col=Knowledge) 

# quick plot -- RTs -- CW.p1
qplot(Cond, Word.RT, data=subset(bySubj, Word.number=="cw.p1"),   # per cond
      geom=c("boxplot"), col=Cond) 
qplot(Subj, Word.RT, data=subset(bySubj, Word.number=="cw.p1"),   # per sub    
      geom=c("boxplot"))          
# pS vs mS (collapsing across pK and mK conditions)
qplot(Stereotypicality, Word.RT, data=subset(bySubj, Word.number=="cw.p1"),  
      geom=c("boxplot"), col=Stereotypicality) 
# pK vs mK (collapsing across pS and mS conditions)
qplot(Knowledge, Word.RT, data=subset(bySubj, Word.number=="cw.p1"),   
      geom=c("boxplot"), col=Knowledge) 

# quick plot -- RTs -- cw.p2
qplot(Cond, Word.RT, data=subset(bySubj, Word.number=="cw.p2"),   # per cond
      geom=c("boxplot"), col=Cond) 
qplot(Subj, Word.RT, data=subset(bySubj, Word.number=="cw.p2"),   # per sub    
      geom=c("boxplot"))          
# pS vs mS (collapsing across pK and mK conditions)
qplot(Stereotypicality, Word.RT, data=subset(bySubj, Word.number=="cw.p2"),  
      geom=c("boxplot"), col=Stereotypicality) 
# pK vs mK (collapsing across pS and mS conditions)
qplot(Knowledge, Word.RT, data=subset(bySubj, Word.number=="cw.p2"),   
      geom=c("boxplot"), col=Knowledge) 



#-------------------------------------------------------------------------------
# Evalute RTs 
#-------------------------------------------------------------------------------

# function to center then scale -- subtracts column means (omitting NAs) from 
# each column then divides centered columns by their standard deviations
z.abs <- function (x) abs(scale(x))  

# get mean RT and z-scores -- by Subj
mrt <- aggregate(d1$Word.RT, list(d1$Subj), mean)
mrt$z <- z.abs(mrt$x)
names(mrt) <- c("Subj","mRT","mRT.z")

# show outliers
subset(mrt, mRT.z > 2)

#    Subj      mRT    mRT.z
# 43   46 614.5449 3.264706

# get mean RT and z-scores -- by Subj x Cond
mrt1 <- aggregate(d1$Word.RT, list(d1$Subj,d1$Cond), mean)
mrt1$z <- z.abs(mrt1$x)
names(mrt1) <- c("Subj","Cond","mRT","mRT.z")

subset(mrt1, mRT.z > 2)

    Subj  Cond      mRT    mRT.z
24    24 mS.mK 189.7000 2.156302
43    46 mS.mK 658.7368 3.686772
86    39 mS.pK 536.0263 2.158093
92    46 mS.pK 563.9500 2.505955
141   46 pS.mK 542.0789 2.233494
190   46 pS.pK 692.0000 4.101151

# consider excluding subjects 24, 46


#-------------------------------------------------------------------------------
# Evalute Comprehension Question accuracy
#-------------------------------------------------------------------------------

# get function for aggregating 
#source('summarySE.R')  # assuming summarySE.R lives in current directory
source(sprintf('%s%s', CodeDir, "summarySE.R"))

# Q.ACC by Subj
acc.s <- summarySE(subset(d1, Word.number=="cw" & Cond != "fillers"), 
                 measurevar="Q.ACC",
                 groupvars=c("Subj","Cond"), 
                 na.rm=T)
acc.s <- droplevels(acc.s) # necessary because we removed fillers
head(arrange(acc.s, Q.ACC))

# create table
subj <- sort(unique(d1$Subj))
table.acc.s <- unstack(acc.s, Q.ACC ~ Cond)
table.acc.s$mean.ACC <-  rowMeans(table.acc.s)
table.acc.s <- cbind(subj, table.acc.s)

head(arrange(table.acc.s, mean.ACC))


# Q.ACC by Item
acc.i <- summarySE(subset(d1, Word.number=="cw" & Cond != "fillers"), 
                   measurevar="Q.ACC",
                   groupvars=c("Item","Cond"), 
                   na.rm=T)
acc.i <- droplevels(acc.i)
head(arrange(acc.i, Q.ACC))

# create table
items <- sort(unique(d1$Item))
table.acc.i <- unstack(acc.i, Q.ACC ~ Cond)
table.acc.i$mean.ACC <-  rowMeans(table.acc.i)
table.acc.i <- cbind(items, table.acc.i)

head(arrange(table.acc.i, mean.ACC))

# get Items/Conds with less than or equal to 50% correct
excl <- acc.i[acc.i$Q.ACC <= 0.50, c("Item","Cond","Q.ACC")]


# exclude bad Item/Cond 
tmp <- subset(d1, Word.number=="cw" & Cond != "fillers") 
tmp <- subset(tmp, !(Item == excl$Item[1] & Cond == as.character(excl$Cond[1])) &
                   !(Item == excl$Item[2] & Cond == as.character(excl$Cond[2])) &
                   !(Item == excl$Item[3] & Cond == as.character(excl$Cond[3])) &
                   !(Item == excl$Item[4] & Cond == as.character(excl$Cond[4])) &
                   !(Item == excl$Item[5] & Cond == as.character(excl$Cond[5])) &
                   !(Item == excl$Item[6] & Cond == as.character(excl$Cond[6])) &
                   !(Item == excl$Item[7] & Cond == as.character(excl$Cond[7])) &
                   !(Item == excl$Item[8] & Cond == as.character(excl$Cond[8])) &
                   !(Item == excl$Item[9] & Cond == as.character(excl$Cond[9])))

# redo Q.ACC by Subj
acc.s2 <- summarySE(tmp,
                   measurevar="Q.ACC",
                   groupvars=c("Subj","Cond"), 
                   na.rm=T)
acc.s2 <- droplevels(acc.s2)
head(arrange(acc.s2, Q.ACC))

# create table
subj <- sort(unique(d1$Subj))
table.acc.s2 <- unstack(acc.s2, Q.ACC ~ Cond)
table.acc.s2$mean.ACC <-  rowMeans(table.acc.s2)
table.acc.s2 <- cbind(subj, table.acc.s2)

head(arrange(table.acc.s2, mean.ACC))

   subj     mS.mK     mS.pK     pS.mK     pS.pK  mean.ACC
1    38 0.6000000 1.0000000 0.2857143 0.7777778 0.6658730
2    49 0.7000000 0.8000000 0.7142857 0.8888889 0.7757937
3    33 0.7000000 0.6000000 1.0000000 0.8888889 0.7972222
4    52 0.8000000 0.7777778 0.8750000 0.8000000 0.8131944
5    26 0.7000000 1.0000000 0.7142857 0.8888889 0.8257937
6     3 0.7000000 0.9000000 0.7142857 1.0000000 0.8285714
7    12 0.7000000 0.9000000 0.7142857 1.0000000 0.8285714
8    18 0.8000000 0.8000000 1.0000000 0.8888889 0.8722222
9    20 0.9000000 0.9000000 0.7142857 1.0000000 0.8785714
10   27 0.9000000 0.8888889 0.7500000 1.0000000 0.8847222
11   31 0.9000000 0.9000000 0.8571429 0.8888889 0.8865079
12   51 0.7777778 0.9000000 0.9000000 1.0000000 0.8944444
13    8 1.0000000 0.7000000 0.9000000 1.0000000 0.9000000
14   13 1.0000000 0.9000000 0.7000000 1.0000000 0.9000000
15   44 1.0000000 0.9000000 0.7142857 1.0000000 0.9035714
16    5 0.9000000 1.0000000 0.8571429 0.8888889 0.9115079
17   42 1.0000000 0.8888889 0.8750000 0.9000000 0.9159722
18   50 0.9000000 0.8888889 0.8750000 1.0000000 0.9159722
19   15 0.8000000 1.0000000 0.8750000 1.0000000 0.9187500
20   23 0.7777778 0.9000000 1.0000000 1.0000000 0.9194444

badSubs <- table.acc.s2$subj[ table.acc.s2$mean.ACC < 0.85 ]
[1]  3 12 26 33 38 49 52


#-------------------------------------------------------------------------------
# Exclusions
#-------------------------------------------------------------------------------

# remove bad subs
d1 <- subset(d1, Consent=="yes" &
                  NativeGerman=="Yes" &
                  Age >= 18 )
d1 <- subset(d1, !(Subj %in% badSubs))
d1 <- subset(d1, Subj != 24 & Subj != 46)  # consider excluding subjects 24, 46
d1 <- subset(d1, Subj != 6 & 
                  Subj != 25 & 
                  Subj != 13 & 
                  Subj != 32 & 
                  Subj != 43) 

# # replace outlier trails w NA
# d1$Word.RT[d1$Word.RT < 200] <- NA
# d1$Word.RT[d1$Word.RT > 1000] <- NA

# remove outlier trials
d1 <- subset(d1, Word.RT > 200 &
                  Word.RT < 1000)


#-------------------------------------------------------------------------------
# Visualize data after exclusions
#-------------------------------------------------------------------------------

# cw.p1

# Density plots - all Conds
ggplot(subset(d1, Word.number=="cw.p1"),   # uses ggplot2 library
       aes(Word.RT, fill = Cond)) + geom_density(alpha = 0.2) + 
     scale_fill_manual(values=c("red","purple","green","blue"))

# RTs by Cond as box plots
qplot(Cond, Word.RT, data=subset(d1, Word.number=="cw.p1"), 
      geom=c("boxplot"), col=Cond)  


# cw.p2

# Density plots - all Conds
ggplot(subset(d1, Word.number=="cw.p2"),   # uses ggplot2 library
       aes(Word.RT, fill = Cond)) + geom_density(alpha = 0.2) + 
     scale_fill_manual(values=c("red","purple","green","blue"))

# RTs by Cond as box plots
qplot(Cond, Word.RT, data=subset(d1, Word.number=="cw.p2"), 
      geom=c("boxplot"), col=Cond)  
  



#-------------------------------------------------------------------------------
# Aggregate data by-Subjects and by-Items
#-------------------------------------------------------------------------------

# get function for aggregating 
#source('summarySE.R')  # assuming summarySE.R lives in current directory
source(sprintf('%s%s', CodeDir, "summarySE.R"))


#--- By Subjects ---------
# aggregate RTs and calculate mean RT and SD for each subject 
bySubj <- summarySE(d1, measurevar="Word.RT", 
                    groupvars=c("Subj","Cond","Word.number"), 
                    na.rm=T)  # 24 x 8
bySubj$Subj <- as.factor(bySubj$Subj)

# group Conditions by IV (S and K)
bySubj$Stereotypicality <- NA
bySubj$Knowledge <- NA
bySubj$Stereotypicality[bySubj$Cond=="pS.pK" | bySubj$Cond=="pS.mK"] <- "pS"
bySubj$Stereotypicality[bySubj$Cond=="mS.pK" | bySubj$Cond=="mS.mK"] <- "mS"
bySubj$Knowledge[bySubj$Cond=="pS.pK" | bySubj$Cond=="mS.pK"] <- "pK"
bySubj$Knowledge[bySubj$Cond=="pS.mK" | bySubj$Cond=="mS.mK"] <- "mK"
# 24 x 10


# quick plot -- RTs -- CW
qplot(Cond, Word.RT, data=subset(bySubj, Word.number=="cw"),   # per cond
      geom=c("boxplot"), col=Cond) 
qplot(Subj, Word.RT, data=subset(bySubj, Word.number=="cw"),   # per sub    
      geom=c("boxplot"))          
# pS vs mS (collapsing across pK and mK conditions)
qplot(Stereotypicality, Word.RT, data=subset(bySubj, Word.number=="cw"),  
      geom=c("boxplot"), col=Stereotypicality) 
# pK vs mK (collapsing across pS and mS conditions)
qplot(Knowledge, Word.RT, data=subset(bySubj, Word.number=="cw"),   
      geom=c("boxplot"), col=Knowledge) 

# quick plot -- RTs -- CW.p1
qplot(Cond, Word.RT, data=subset(bySubj, Word.number=="cw.p1"),   # per cond
      geom=c("boxplot"), col=Cond) 
qplot(Subj, Word.RT, data=subset(bySubj, Word.number=="cw.p1"),   # per sub    
      geom=c("boxplot"))          
# pS vs mS (collapsing across pK and mK conditions)
qplot(Stereotypicality, Word.RT, data=subset(bySubj, Word.number=="cw.p1"),  
      geom=c("boxplot"), col=Stereotypicality) 
# pK vs mK (collapsing across pS and mS conditions)
qplot(Knowledge, Word.RT, data=subset(bySubj, Word.number=="cw.p1"),   
      geom=c("boxplot"), col=Knowledge) 

# quick plot -- RTs -- cw.p2
qplot(Cond, Word.RT, data=subset(bySubj, Word.number=="cw.p2"),   # per cond
      geom=c("boxplot"), col=Cond) 
qplot(Subj, Word.RT, data=subset(bySubj, Word.number=="cw.p2"),   # per sub    
      geom=c("boxplot"))          
# pS vs mS (collapsing across pK and mK conditions)
qplot(Stereotypicality, Word.RT, data=subset(bySubj, Word.number=="cw.p2"),  
      geom=c("boxplot"), col=Stereotypicality) 
# pK vs mK (collapsing across pS and mS conditions)
qplot(Knowledge, Word.RT, data=subset(bySubj, Word.number=="cw.p2"),   
      geom=c("boxplot"), col=Knowledge) 



#--- By Items ---------
byItems <- summarySE(d1, measurevar="Word.RT", 
                     groupvars=c("Item","Cond","Word.number"), 
                     na.rm=T)
byItems$Item <- as.factor(byItems$Item)  # 120 x 8

# group Conditions by IV (S and K)
byItems$Stereotypicality <- NA
byItems$Knowledge <- NA
byItems$Stereotypicality[byItems$Cond=="pS.pK" | byItems$Cond=="pS.mK"] <- "pS"
byItems$Stereotypicality[byItems$Cond=="mS.pK" | byItems$Cond=="mS.mK"] <- "mS"
byItems$Knowledge[byItems$Cond=="pS.pK" | byItems$Cond=="mS.pK"] <- "pK"
byItems$Knowledge[byItems$Cond=="pS.mK" | byItems$Cond=="mS.mK"] <- "mK"
# 120 x 10

# quick plot -- RTs -- CW 
qplot(Item, Word.RT, data=subset(byItems, Word.number=="cw"),   # per item
      geom=c("boxplot"))          
qplot(Cond, Word.RT, data=subset(byItems, Word.number=="cw"),   # per cond
      geom=c("boxplot"), col=Cond) 
# pS vs mS (collapsing across pK and mK conditions)
qplot(Stereotypicality, Word.RT, data=subset(byItems, Word.number=="cw"),  
      geom=c("boxplot"), col=Stereotypicality) 
# pK vs mK (collapsing across pS and mS conditions)
qplot(Knowledge, Word.RT, data=subset(byItems, Word.number=="cw"),   
      geom=c("boxplot"), col=Knowledge) 


# quick plot -- RTs -- CW.p1
qplot(Item, Word.RT, data=subset(byItems, Word.number=="cw.p1"),   # per item
      geom=c("boxplot"))          
qplot(Cond, Word.RT, data=subset(byItems, Word.number=="cw.p1"),   # per cond
      geom=c("boxplot"), col=Cond) 
# pS vs mS (collapsing across pK and mK conditions)
qplot(Stereotypicality, Word.RT, data=subset(byItems, Word.number=="cw.p1"),  
      geom=c("boxplot"), col=Stereotypicality) 
# pK vs mK (collapsing across pS and mS conditions)
qplot(Knowledge, Word.RT, data=subset(byItems, Word.number=="cw.p1"),   
      geom=c("boxplot"), col=Knowledge) 


#-------------------------------------------------------------------------------
# Statistical analyses
#-------------------------------------------------------------------------------

# subset bySubj data to include only RTs for CW 
cw.data <- subset(bySubj, Word.number=="cw")

# ANOVA: Calculate F statistics and associated p-values
#library(ez)
m.cw = ezANOVA(data=cw.data, 
                      dv=Word.RT, # DV
                      wid = Subj, # Subject index column
                      within = .(Stereotypicality, Knowledge), # within IV(s)
                      type = 3) # type of sum of squares (SS)
m.cw


# Quick interaction plot
# error bars represent FLSD (Fisher's Least Significant Difference)
ezPlot(data=cw.data, # data frame/set
       dv=Word.RT, # DV
       wid = Subj, # Subject index column
       within = .(Stereotypicality,Knowledge), # within-subjects IV(s)
       x=Stereotypicality,
       split=Knowledge,
       x_lab="Stereotypicality",
       y_lab="mean Reading Time (ms)",
       split_lab="Knowledge")



# subset bySubj data to include only RTs for CW.p1
cw.p1.data <- subset(bySubj, Word.number=="cw.p1")

m.cw.p1 = ezANOVA(data=cw.p1.data, 
             dv=Word.RT, # DV
             wid = Subj, # Subject index column
             within = .(Stereotypicality, Knowledge), # within IV(s)
             type = 3) # type of sum of squares (SS)
m.cw.p1

# Quick interaction plot
ezPlot(data=cw.p1.data, # data frame/set
       dv=Word.RT, # DV
       wid = Subj, # Subject index column
       within = .(Stereotypicality,Knowledge), # within-subjects IV(s)
       x=Stereotypicality,
       split=Knowledge,
       x_lab="Stereotypicality",
       y_lab="mean Reading Time (ms)",
       split_lab="Knowledge")



# subset bySubj data to include only RTs for cw.p2
cw.p2.data <- subset(bySubj, Word.number=="cw.p2")

m.cw.p2 = ezANOVA(data=cw.p2.data, 
                  dv=Word.RT, # DV
                  wid = Subj, # Subject index column
                  within = .(Stereotypicality, Knowledge), # within IV(s)
                  type = 3) # type of sum of squares (SS)
m.cw.p2

# Quick interaction plot
ezPlot(data=cw.p2.data, # data frame/set
       dv=Word.RT, # DV
       wid = Subj, # Subject index column
       within = .(Stereotypicality,Knowledge), # within-subjects IV(s)
       x=Stereotypicality,
       split=Knowledge,
       x_lab="Stereotypicality",
       y_lab="mean Reading Time (ms)",
       split_lab="Knowledge")


#----------------------------------------------------------------------------------------------------
#  For final By-Word plots - Get Grand Mean by-Subj and by-Items data
#----------------------------------------------------------------------------------------------------

bySubj.gm <- summarySE(bySubj, 
                       measurevar="Word.RT", 
                       groupvars=c("Cond", "Word.number"), 
                       na.rm=T)

bySubj.gm$Stereotypicality <- NA
bySubj.gm$Knowledge <- NA
bySubj.gm$Stereotypicality[bySubj.gm$Cond=="pS.pK" | 
                                bySubj.gm$Cond=="pS.mK"] <- "pS"
bySubj.gm$Stereotypicality[bySubj.gm$Cond=="mS.pK" | 
                                bySubj.gm$Cond=="mS.mK"] <- "mS"
bySubj.gm$Knowledge[bySubj.gm$Cond=="pS.pK" | 
                         bySubj.gm$Cond=="mS.pK"] <- "pK"
bySubj.gm$Knowledge[bySubj.gm$Cond=="pS.mK" | 
                         bySubj.gm$Cond=="mS.mK"] <- "mK"


#--- plot RT by word position (Word.number) -------
# error bars represent standard error of the mean (SEM)

pdf(paste('RT.by.word.number.pdf'), height=7, width=15, bg="white")
#png('RT.by.word.number.png', height=500, width=1000, bg="white")
pd <- position_dodge(.1) # move overlapping error bars .05 to the left and right
ggplot(subset(bySubj.gm, Cond != "filler"), 
       aes(x = Word.number, 
           y = Word.RT, 
           group = Cond)) +
     geom_errorbar(aes(ymin = Word.RT - se, 
                       ymax = Word.RT + se), 
                   colour='black', width=.25, position=pd) +
     geom_line(position = pd, 
               aes(color = Cond)) +
     geom_point(position = pd, size = 3, shape = 21, fill = 'white') + # 21 is filled circle
     scale_linetype_manual(values=c(2,2,2,2)) +
     #scale_color_manual(values=c("indianred4","darksalmon","forestgreen","darkseagreen3")) +
     #scale_size_manual(values=c(5,5,5,5)) +
     ylab("Reading Time (ms)") +
     ggtitle('Reading Times by Word') +
     theme_bw()
dev.off()


pdf(paste(title,'RT.by.word.number.pdf', sep='.'), height=7, width=15, bg="white")
#png('RT.by.word.number.png', height=500, width=1000, bg="white")
pd <- position_dodge(.1) # move overlapping error bars .05 to the left and right
ggplot(subset(bySubj.gm, Cond != "filler"), 
       aes(x = Word.number, 
           y = Word.RT, 
           group = Cond)) +
     geom_errorbar(aes(ymin = Word.RT - se, 
                       ymax = Word.RT + se), 
                   colour='black', width=.25, position=pd) +
     geom_line(position = pd, 
               aes(color = Stereotypicality, linetype = Knowledge)) +
     geom_point(position = pd, size = 3, shape = 21, fill = 'white') + # 21 is filled circle
     ylab("Reading Time (ms)") +
     ggtitle('Reading Times by Word') +
     theme_bw()
dev.off()

# reorder levels of IVs with relevel()
bySubj.gm$Word.number <- as.factor(bySubj.gm$Word.number)
bySubj.gm$Stereotypicality <- as.factor(bySubj.gm$Stereotypicality)
bySubj.gm$Knowledge <- as.factor(bySubj.gm$Knowledge)

bySubj.gm$Word.number <- relevel(bySubj.gm$Word.number, ref="cw.m1")
#bySubj.gm$Stereotypicality <- relevel(bySubj.gm$Stereotypicality, ref="pS")
bySubj.gm$Knowledge <- relevel(bySubj.gm$Knowledge, ref="pK")

# re-plot
pdf(paste(title,'RT.by.word.number.pdf', sep='.'), height=7, width=15, bg="white")
#png('RT.by.word.number.png', height=500, width=1000, bg="white")
pd <- position_dodge(.1) # move overlapping error bars .05 to the left and right
ggplot(subset(bySubj.gm, Cond != "filler"), 
       aes(x = Word.number, 
           y = Word.RT, 
           group = Cond)) +
     geom_errorbar(aes(ymin = Word.RT - se, 
                       ymax = Word.RT + se), 
                   colour='black', width=.25, position=pd) +
     geom_line(position = pd, 
               aes(color = Stereotypicality, linetype = Knowledge)) +
     geom_point(position = pd, size = 3, shape = 21, fill = 'white') + # 21 is filled circle
     ylab("Reading Time (ms)") +
     ggtitle('Reading Times by Word') +
     theme_bw()
dev.off()


#----------------------------------------------------------------------------------------------------
# For final bar plots - Get Grand Mean by-Subj and by-Items data
#----------------------------------------------------------------------------------------------------

bySubj.gm <- summarySE(subset(bySubj, Word.number=="cw"), 
                       measurevar="Word.RT", 
                       groupvars=c("Cond"), 
                       na.rm=T)
byItems.gm <- summarySE(subset(byItems, Word.number=="cw"), 
                        measurevar="Word.RT", 
                        groupvars=c("Cond"), 
                        na.rm=T)

bySubj.gm$Stereotypicality <- NA
bySubj.gm$Knowledge <- NA
bySubj.gm$Stereotypicality[bySubj.gm$Cond=="pS.pK" | 
                                bySubj.gm$Cond=="pS.mK"] <- "pS"
bySubj.gm$Stereotypicality[bySubj.gm$Cond=="mS.pK" | 
                                bySubj.gm$Cond=="mS.mK"] <- "mS"
bySubj.gm$Knowledge[bySubj.gm$Cond=="pS.pK" | 
                         bySubj.gm$Cond=="mS.pK"] <- "pK"
bySubj.gm$Knowledge[bySubj.gm$Cond=="pS.mK" | 
                         bySubj.gm$Cond=="mS.mK"] <- "mK"



# bar plots - error bars represent standard error of the mean (SEM)
ggplot(bySubj.gm, 
       aes(x=Cond, y=Word.RT, fill=Cond)) + 
     geom_bar(position=position_dodge(), stat="identity") +
     geom_errorbar(aes(ymin=Word.RT-se, ymax=Word.RT+se),
                   width=.2,  # Width of the error bars
                   position=position_dodge(.9)) +
     ylab("Reading Time (ms)") +
     ggtitle('Reading Times for Critical Word') 


# interaction plots - RT by IV - with standard error of the mean (SEM)

# to write the plot to a pdf file, uncomment the lines below
#pdf(paste(title,'RT.by.Cond.pdf', sep='.'), height=7, width=15, bg="white")
pd <- position_dodge(.1) # move overlapping error bars .05 to the left and right
ggplot(bySubj.gm, 
       aes(x=Stereotypicality, y=Word.RT, 
           colour=Knowledge, group=Knowledge)) + 
     geom_errorbar(aes(ymin=Word.RT-se, ymax=Word.RT+se), 
                   colour='black', width=.25, position=pd) +
     geom_line(position=pd) +
     geom_point(position=pd, size=3, shape=21, fill='white') + # 21 is filled circle
     ylab("Reading Time (ms)") +
     ggtitle('Reading Times for Critical Word') +
     theme_bw()
#theme(legend.justification=c(1,0), legend.position=c(0.2,0)) # position in bottom left
#dev.off()

# Equivalent plot but simply switching which IV is on which axis. It is often
# useful to plot interactions "both ways" because the pattern is sometimes more
# visible in one or the other

pd <- position_dodge(.1) # move overlapping error bars .05 to the left and rightset
ggplot(bySubj.gm, aes(x=Knowledge, y=Word.RT, colour=Stereotypicality, group=Stereotypicality)) + 
     geom_errorbar(aes(ymin=Word.RT-se, ymax=Word.RT+se), colour='black',
                   width=.25, position=pd) +
     geom_line(position=pd) +
     geom_point(position=pd, size=3, shape=21, fill='white') + # 21 is filled circle
     ylab("Reading Time (ms)") +
     ggtitle('Reading Times for Critical Word') +
     theme_bw()
