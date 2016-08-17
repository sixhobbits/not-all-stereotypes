
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
library(plyr)

# set paths
CodeDir = "C:\\Users\\g\\git\\notes\\coli\\exp\\paper\\scripts"
DataDir = "C:\\Users\\g\\git\\notes\\coli\\exp\\paper\\data"

# set working directory and read data
setwd(DataDir)
d0 <- read.table("data.clean.a.txt", sep="\t", header=T, fill=T)  # 39195 x 27

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
d1 <- subset(d0, Cond != "fillers" & Subj != 0 &
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
      geom=c("boxplot"), col=Cond)  


# RTs by Subj by Cond
qplot(Cond, Word.RT, data=subset(d1, Word.number=="cw"), 
      geom=c("boxplot"), facets= ~ Subj, col=Cond)
qplot(Cond, Word.RT, data=subset(d1, Word.number=="cw"), 
      geom=c("point"), facets= ~ Subj, col=Cond)





#-------------------------------------------------------------------------------
# Evalute Comprehension Question results
#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------
# Exclusions
#-------------------------------------------------------------------------------

# remove bad subs
d1 <- subset(d1, Consent=="yes" &
                  NativeGerman=="Yes" &
                  Age >= 18 )


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
# Rough statistical analyses
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

# find source of error (missing cell)
ezDesign(cw.data, x=Cond, y=Subj) 

# exlcude subject w missing cell
cw.data <- subset(bySubj, Word.number=="cw" & Subj != 24)

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

