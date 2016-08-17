reformat.ibex.results.vectorized <- function()
{
    ###########################################################################
    ##  This script does the following:
    ##
    ##  1. reads Ibex results file (.csv format)
    ##  2. removes comment lines, reformats into long data format, adds headers
    ##  3. writes to tab separated file
    ###########################################################################
    
    # Globals
    RESULTS_DIR <- getwd()
    DATA_DIR <- getwd()
    DATA_FILE <- "results"
    OUT_FILE <- "results.clean.tsv"

    # List of test subjects that will be excluded from the final output
    EXCLUDE <- c("Alessandra", "medha", "Medha", "32334", "001", "111111", "00", "Vojtech_test")


    cat("\nbegin reformat.ibex.results ", date())
    cat("\n\n  Working...\n\n")
    df <- read.csv(DATA_FILE, header=F, col.names=c(as.character(1:12)), as.is=T, encoding="utf-8")

    # 46_newTry should be 46
    df[df=="46_newTry"] <- "46"

    # The string "USER" only appears at the start of each subject (in user-agent string)
    # We use this to pull out all the subjects
    subj_indices <- grep("USER", df[, 1])

    # Get info per subject
    # One data point per subject - each of the following is a list the same length as number of subjects
    # For example, age[5] is the age of the fifth subject

    # We pull out the main part of the date and get the design number by splitting on '=' as these cells are verbose
    date <- sapply(df[subj_indices - 1, 1], function(y) paste(unlist(strsplit(y, " "))[5:8], collapse = " "))
    designNum <- sapply(df[subj_indices + 1, 1], function(y) as.numeric(unlist(strsplit(y, '= '))[2][[1]]))
#
    subj <- df[subj_indices+13, 9]
    age <- df[subj_indices+14, 9]
    consent <- df[subj_indices+18, 9] 
    gender <- df[subj_indices+19, 9]
    german <- df[subj_indices+20, 9]
    lang1 <- df[subj_indices+15, 9]
    prof1 <- df[subj_indices+21, 9]
    lang2 <- df[subj_indices+16, 9]
    prof2 <- df[subj_indices+22, 9]
    lang3 <- df[subj_indices+17, 9]
    prof3 <- df[subj_indices+23, 9]
    ibexsubj <- df[subj_indices+13, 1]
    ip <- df[subj_indices+13, 2]

    # The end indices for each subject are the start indices 'moved up' by one with the length of the list appended to the end
    end_indices <- tail(subj_indices, - 1)
    end_indices[length(end_indices) + 1] <- nrow(df)
    
    # A dataframe of info per subject. We iterate over it to pull out the info for each word
    subj_df <- data.frame(start=c(subj_indices), end=c(end_indices), subj=c(subj), age=c(age), consent=c(consent), gender=c(gender), 
                          german=c(german), lang1=c(lang1), prof1=c(prof1), lang2=c(lang2), prof2=c(prof2), lang3=c(lang3), prof3=c(prof3), 
                          designNum=c(designNum), ibexsubj=c(ibexsubj), ip=c(ip), date=c(date))

    # Exclude the 'test' subjects
    subj_df <- subset(subj_df, !(subj_df[, 3] %in% EXCLUDE))

    cat("\nProcessing data for", length(subj_df[, 3]), "subjects: ", as.character(subj_df[, 3]), "\n")

    # Info per word
    ds <- apply(subj_df, 1, function(y) {
        s <- df[y[1]:y[2] ,]

        dashed_s <- grep("DashedSentence", s[, 3])
        quest <- grep("Question", s[, 3])

        item <- s[dashed_s, 4]
        element <- s[dashed_s, 5]
        type  <- s[dashed_s, 6]
        group  <- s[dashed_s, 7]
        word_num  <- s[dashed_s, 8]
        word  <- s[dashed_s, 9]
        word_length <- sapply(s[dashed_s, 9], nchar)
        word_rt  <- s[dashed_s, 10]
        newline <-   s[dashed_s, 11]
        sentence  <- sapply(s[dashed_s, 12], function(y) gsub(" ", "_", y))

        q_item <- s[quest, 4]
        q_element <- s[quest, 5]
        q_type <- s[quest, 6]
        q_group  <- s[quest, 7]
        q_acc <- s[quest, 10]
        q_rt <- s[quest, 11]

        ds_data <- cbind(line_no=dashed_s, y[3], y[4], y[5], y[6], y[7], y[8], y[9], y[10], y[11], y[12], y[13], y[14], y[15], y[16], y[17], 'sentence', item, element, type, group, 
              word_num, word, word_length, word_rt, newline, sentence, "NA", "NA")
        q_data <- cbind(line_no=quest, y[3], y[4], y[5], y[6], y[7], y[8], y[9], y[10], y[11], y[12], y[13], y[14], y[15], y[16], y[17], 'question',  q_item, q_element, q_type, q_group,"NA", "NA", "NA", "NA", "NA", "NA", q_acc, q_rt)
        both <- rbind(ds_data, q_data)
        sorted <- both[order(as.numeric(both[, 1])) , ]
        subset(sorted, !duplicated(sorted[, 1]))[, 2:ncol(sorted)]
    })

    d1 <- data.frame(matrix(nrow=0,ncol=28))
    foo_ <- sapply(ds, function(y) {
        d1 <<- rbind(d1, y)

    })

    d1 <- data.frame(lapply(d1, function(y) {
          gsub("%2C", ",", y)
    }))

    names(d1) <- c('Subj','Age','Consent','Gender','NativeGerman',
                    'lang1','prof1','lang2','prof2','lang3','prof3',
                    'DesignNum','IbexSubj','IP','Date','Controller','Item',
                    'Element','Type','Group','Word.number','Word','Word.length',
                    'Word.RT','Newline','Sentence','Q.ACC','Q.RT')
    write.table(d1, OUT_FILE, sep="\t", row.names=F, quote=F)
}
