
reformat.ibex.results <- function()
{
     ###########################################################################
     ##  This script does the following:
     ##
     ##  1. reads Ibex results file (.csv format)
     ##  2. removes comment lines, reformats into long data format, adds headers,
     ##     convert "%2C" to comma
     ##  3. writes data.raw.txt file to ExptMethods/CourseExpt/Data 
     ###########################################################################
     
     # print start timestamp to console
     cat("\nbegin reformat.ibex.results ", date())
     cat("\n\n  Working...\n\n")
     
     # set paths
     ResultsDir = '~/Documents/WORK IN PROGRESS/Saarland/2016 Summer/ExptMethods/CourseExpt/Ibex/ExptNew/Results' 
     DataDir = '~/Documents/WORK IN PROGRESS/Saarland/2016 Summer/ExptMethods/CourseExpt/Analysis/Data'
     
     # set working directory and read results file
     setwd(ResultsDir)
     
     # create df for reformatted data
     d1 <- data.frame(matrix(nrow=0,ncol=28)) 
     names(d1) <- c('Subj','Age','Consent','Gender','NativeGerman',
                    'lang1','prof1','lang2','prof2','lang3','prof3',
                    'DesignNum','IbexSubj','IP','Date','Controller','Item',
                    'Element','Type','Group','Word.number','Word','Word.length',
                    'Word.RT','Newline','Sentence','Q.ACC','Q.RT')
     
     thisRow <- 1   # initialize variable for searching rows in df
     
     ## read Ibex results file
     df <- read.csv('results', header=F, 
                    col.names=c(as.character(1:12)), # 12 is max columns in data
                    as.is=T)   # don't convert column classes
     
     # replace all '%2C' with comma -- Note: this converts all cols to factors
     df <- data.frame(lapply(df, function(x) {
          gsub("%2C", ",", x)
     }))   
     
     # convert all columns back to character class
     df <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
     
     
     ###### to remove "test" data, start the for loop at row 33025 ###### 
     #for (i in 1:nrow(df)) 
     for (i in 33025:nrow(df)) 
     {
          ## for each subject, get the Date and Design Number they were run on
          if (length(grep("USER", df[i,1]))>0) # finds lines containing USER, 
               # which marks the 2nd line of each subject's data
          {
               date <- unlist(strsplit(df[i-1,1], 'day '))[2]  # get date
               date <- unlist(strsplit(date, ' UTC'))[1]
               designNum <- as.numeric(unlist(strsplit(df[i+1,1], '= '))[2])
               ## List1 = odd Design numbers; List2 = even Design numbers
               #list <- as.numeric(unlist(strsplit(df[i+1,1], '= '))[2])
               #if (list %% 2 == 0) { list <- 2 }  # even: modulo 2 = 0 
               #if (list %% 2 == 1) { list <- 1 }  # odd:  modulo 2 = 1
          }
          ## get Subject number, IbexSubject number and IP address
          if (df[i,8]=='name')
          {
               subj <- df[i,9]
               age <- df[i+1,9]
               consent <- df[i+5,9]
               gender <- df[i+6,9]
               german <- df[i+7,9]
               lang1 <- df[i+2,9]
               prof1 <- df[i+8,9]
               lang2 <- df[i+3,9]
               prof2 <- df[i+9,9]
               lang3 <- df[i+4,9]
               prof3 <- df[i+10,9]
               ibexsubj <- df[i,1]  # actually time of submission but is unique
               ip <- df[i,2]
          }
          ## get trial data
          if (df[i,3]=='DashedSentence') 
          {
               if (df[i,1]==ibexsubj) # confirm that it is same subject
               {
                    controller <- 'sentence'
                    item <- df[i,4]   # number assigned to a controller based on 
                                      # its position in the list of controllers
                    element <- df[i,5]
                    type <- df[i,6]        # type = Condition
                    group <- df[i,7]       # group = Item number
                    word.num <- df[i,8]    # position of word
                    word <- df[i,9]
                    word.length <- nchar(df[i,9])
                    word.rt <- df[i,10]
                    newline <- df[i,11]    # if sentence broken across 2 lines
                    sentence <- gsub(" ","_",df[i,12]) # replace spaces with '_'
                    
                    ## write to d1   
                    d1[thisRow, c(1:26)] <- c(subj,age,consent,gender,german,
                                              lang1,prof1,lang2,prof2,lang3,prof3,
                                              designNum,ibexsubj,ip,date,
                                              controller,item,element,type,group,
                                              word.num,word,word.length,word.rt,
                                              newline,sentence)
                    thisRow <- thisRow + 1
               }
          }
          ## get question ACC and RT
          if (df[i,3]=='Question')
          {
               if (df[i,1]==ibexsubj && df[i,4]==item) # confirm same subject
               {
                    controller <- 'question'
                    q.acc <- df[i,10]    # 1=correct; 0=incorrect
                    q.rt <- df[i,11]     # RT for answering question
                    
                    ## write to d1
                    d1[thisRow, c(1:20,27:28)] <- c(subj,age,consent,gender,german,
                                                    lang1,prof1,lang2,prof2,lang3,prof3,
                                                    designNum,ibexsubj,ip,date,
                                                    controller,item,element,
                                                    type,group,q.acc,q.rt)
                    thisRow <- thisRow + 1
               }
          }
     }  
     
     # write data to Data directory
     setwd(DataDir)
     write.table(d1, 'data.raw.txt', sep="\t", row.names=F, quote=F)  
     
     # print number of subjects and Newlines to console
     subs <- unique(d1$Subj)
     cat('Ibex results file contains',length(subs),'subjects: ', subs, '\n')
     if (length(unique(d1$Newline))>2)
     {
          cat('  *** Data.txt file contained Newlines *** \n')
     }
     
     # print end timestamp to console
     cat("\n\n printed Data.raw.txt: ", date())
     
}
