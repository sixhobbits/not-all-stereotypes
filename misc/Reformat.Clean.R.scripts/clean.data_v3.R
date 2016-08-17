
clean.data <- function()
{
     
     #############################################################################
     ##  This script does the following:
     ##
     ##  1. reads data.raw.txt 
     ##  2. renames cols (Group > Item, Type > Cond, Item > Foo)
     ##  3. removes Practice trials
     ##  4. adds CW position (from CW.number.txt)
     ##  5. puts Question data (Q.ACC, Q.RT) on same line as Sentence data
     ##  6. adds CritOrder (presentation order including experimental items only)
     ##          PresOrder (presentation order including all items)
     ##  7. counts potential exclusions
     ##            subjects who did not give consent
     ##                     who did the study more than once
     ##                     who are not native German speakers
     ##                     who are under 18 or over 300
     ##            word RTs that were tooFast (<100) or tooSlow (>2000)
     ##            number of Newlines
     ##  8. returns counts
     ##  9. removes extraneous columns
     ##  10. writes table to show order of trials      -- order.of.trials.txt
     ##             clean data                         -- data.clean.txt
     #############################################################################
     
     cat("\nbegin clean.data: ", date())
     cat("\n\n  Working...\n\n\n\n")
     
     DataDir = '~/Documents/WORK IN PROGRESS/Saarland/2016 Summer/ExptMethods/CourseExpt/Analysis/Data'
     
     setwd(DataDir)
     d1 <- read.table('data.raw.txt', sep='\t', header=T, quote='')
                      #as.is=T)   # don't convert column classes)  # 9516 x 28
     
     # confirm that German characters look ok
     unique(d1$Sentence)
     
     # rename cols (Group > Item, Type > Cond, Item > Foo)
     names(d1) <- c("Subj","Age","Consent","Gender","NativeGerman",
                    "lang1","prof1","lang2","prof2","lang3","prof3",
                    "DesignNum","IbexSubj","IP","Date","Controller","Foo",
                    "Element","Cond","Item","Word.number","Word","Word.length",
                    "Word.RT","Newline","Sentence","Q.ACC","Q.RT")
     
     
     # remove practice trials
     prac <- c("p1","p2","p3","p4")
     d1 <- subset(d1, !Cond %in% prac) # 8955 x 28
     
     # get CW position from file that was output by stims.to.ibex.format.R
     cw.num <- read.table("CW.number.txt", sep="\t", header=T, quote='')
     d1$CW.number <- NA  # add col    # 8955 x 29
     
     # convert encoding of German characters from Latin1 to UTF-8
     cw.num$Statement <- as.character(cw.num$Statement)
     Encoding(cw.num$Statement) <- "latin1"  # identify encoding
     cw.num$Statement <- iconv(cw.num$Statement, "latin1", "UTF-8")
     
     # add Q.ACC, Q.RT and CW number to each line
     subs <- unique(d1$Subj)
     items <- unique(d1$Item)  # item number
     d1$Word.number <- as.character(d1$Word.number)
     for (s in subs) # for each subject
     {
          for (i in items) # for each item
          {
               # get Q.ACC and Q.RT from Question lines
               qacc <- d1$Q.ACC[d1$Subj==s & d1$Item==i & d1$Controller=='question']
               qrt <- d1$Q.RT[d1$Subj==s & d1$Item==i & d1$Controller=='question']
               # get CW number
               cw <- cw.num$CW[cw.num$Item==i]
               # copy to Sentence lines
               d1$Q.ACC[d1$Subj==s & d1$Item==i & d1$Controller=='sentence'] <- qacc 
               d1$Q.RT[d1$Subj==s & d1$Item==i & d1$Controller=='sentence'] <- qrt
               d1$CW.number[d1$Subj==s & d1$Item==i & d1$Controller=='sentence'] <- cw
               d1$Word.number[d1$Subj==s & d1$Item==i & d1$Controller=='sentence' &
                                  d1$Word.number == cw-1 ] <- "cw.m1"
               d1$Word.number[d1$Subj==s & d1$Item==i & d1$Controller=='sentence' &
                                   d1$Word.number == cw ] <- "cw"
               d1$Word.number[d1$Subj==s & d1$Item==i & d1$Controller=='sentence' &
                                   d1$Word.number == cw+1 ] <- "cw.p1"
          }
     }  # 8955 x 29
     
     # add CritOrder and PresOrder 
     d2 <- NULL  # create new df for revised data 
     
     for (s in subs)
     {
          critorder <- 1              # initialize variables
          presorder <- 1              
          tmp <- subset(d1, Subj==s)  # create subset
          tmp$CritOrder <- 0          # add cols
          tmp$PresOrder <- 0
          
          for (i in 1:nrow(tmp))      # for each row
          {
               if (!is.na(tmp$Word.number[i]))   # if not a Question row
               {
                    tmp$PresOrder[i] <- presorder    # add pres order
                    if (tmp$Cond[i] != 'filler')     # if not a filler
                    {
                         tmp$CritOrder[i] <- critorder  # add crit order
                    }
               }
               if (is.na(tmp$Word.number[i]))    # if a Question row
               {
                    presorder <- presorder + 1      # increment pres order
                    if (tmp$Cond[i] != 'filler')    # if not a filler
                    {
                         critorder <- critorder + 1     # increment crit order
                    }
               }
          }
          d2 <- rbind(d2, tmp)   # bind each row to d2
     }  # d2: 1150 x 26
     
     # remove Question rows
     d2 <- subset(d2, Controller=='sentence') # 1058 x 26
     
     # identify subjects who did not give consent, non-German, age > 200
     no.consent <- unique(d2[d2$Consent=="no", ]$Subj)
     non.german <- unique(d2[d2$NativeGerman=="No", ]$Subj)
     wrong.age <- unique(d2[d2$Age < 18 | d2$Age > 50, ]$Subj)
     
     # identify subjects who did the study more than once
     tmp <- subset(d2, Item==1 & Word.number==1)  # Item 1, word 1 for each sub
     n <- data.frame(table(tmp$IP)) # table of IPs and number of occurances
     r <- tmp[tmp$IP %in% n$Var1[n$Freq > 1], # table of Subj w repeat IPs
                    c("Subj","IP")]
     
     repeats <- NULL #list()  # list for subjects to be excluded for repeating study
     for (ip in n$Var1){
          s <- r$Subj[r$IP==ip]  # get repeated sub for each ip
          #cat("subjects:", s, " =  IP address: ", ip, "\n")
          repeats <- append(repeats, s[-1]) # add subs except first to list
     }
     
     # mark repeats in d2
     d2$Repeat <- 0
     d2$Repeat[d2$Subj %in% repeats] <- 1  # 1058 x 27
     
     # count Newlines and RTs that are probably too fast or too slow
     newline <- nrow(subset(d2, Newline=='True'))
     tooFast <- nrow(subset(d2, Word.RT < 100))
     tooSlow <- nrow(subset(d2, Word.RT > 2000))
     
     # # remove excluded subjects
     # d2 <- subset(d2, Consent=="yes" &
     #                   NativeGerman=="Yes" &
     #                   Age >= 18 & Age < 300)  
     #
     # # remove bad trials
     #
     
     # return counts
     cat('Data contains', length(unique(d2$Subj)), 'subjects \n\n')
     cat('   Possible exclusions: \n\n')
     cat('   ', length(no.consent), "subjects did not give consent:", 
         no.consent, "\n")
     cat('   ', length(non.german), "subjects are non-native German speakers:", 
         non.german, "\n")
     cat('   ', length(wrong.age), "subjects do not meeting age requirements:", 
         wrong.age, "\n")
     cat('   ', length(repeats), 
         "subjects did the study at least once before and were marked as repeats:",
         sort(repeats), "\n\n")
     cat('   ', newline, 'Newline trials =', round(newline/nrow(d2)*100,3), 
         '% of total data \n')
     cat('   ', tooFast, 'raw RTs < 100 ms =', round(tooFast/nrow(d2)*100,3), 
         '% of total data \n')
     cat('   ', tooSlow, 'word RTs > 2000 ms =', round(tooSlow/nrow(d2)*100,3), 
         '% of total data \n')
     
     # remove extraneous columns
     d2 <- subset(d2, select = -c(IbexSubj,IP,Controller,Foo,Element)) # 1058 x 22
     

     # write table to show order of trials
     trials <- subset(d2, Word.number==1)
     write.table(trials, 'order.of.trials.txt', sep="\t", row.names=F, quote=F)  # 92 x 22
     
     # write clean data
     write.table(d2, 'data.clean.txt', sep="\t", row.names=F, quote=F)  # 1058 x 22
     
     cat("\n\n printed Data.clean.txt: ", date())
     
}



