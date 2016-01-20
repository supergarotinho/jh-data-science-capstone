## Libraries
library(tm)     ## Text processing framework
library(RWeka)  ## To use NGramTokenizer function
library(slam)   ## Function over sparse matrices: rowapply_simple_triplet_matrix

## Helpfull variables and functions
garbageInCorpus <- c("\u0094","\u0095","\u0096","\u0097")
profaneWords <- NULL
initialDataDir <- "../Data"

## Get files general statistics
getGeneralTextStats <- function(useCache = FALSE) {
    
    filePattern <- paste0(initialDataDir,"/final/en_US/en_US.*")
    statsFileName <- "Data/0-BasicStats/en_US/basicStats.rds"
    
    ## Verify if cached filed does not exists (OR explicity ignore cache)
    if ((file.exists(statsFileName) == FALSE) || (!useCache)) {
        dir.create("Data/0-BasicStats/en_US", showWarnings = FALSE, recursive = TRUE)
        
        filesStatList <- system2("wc", args = c(filePattern,"-clwL"), stdout = TRUE)
        # Trims the result
        filesStatList <- trimws(filesStatList)
        # Remove the files paths
        filesStatList <- gsub(paste0(initialDataDir,"/final/en_US/en_US\\."),"",filesStatList)
        filesStatList <- gsub("\\.txt","",filesStatList)
        # Transform in a matrix
        filesStatList <- t(sapply(strsplit(filesStatList,"[ \t]+",perl = TRUE),unlist))
        # Reorder columns
        filesStatList <- filesStatList[,c(5,1,2,4,3)]
        # Change the columns names
        colnames(filesStatList) <- c("Source name","Number of lines","Number of words","Longest line","Size (MB)")
        # Scale size to MB
        filesStatList <- as.data.frame(filesStatList)
        filesStatList[,2] <- as.numeric(as.character(filesStatList[,2]))
        filesStatList[,3] <- as.numeric(as.character(filesStatList[,3]))
        filesStatList[,4] <- as.numeric(as.character(filesStatList[,4]))
        filesStatList[,5] <- as.numeric(as.character(filesStatList[,5]))
        filesStatList[,"Size (MB)"] <- filesStatList[,"Size (MB)"] / 1024 / 1024
        
        saveRDS(filesStatList, file=statsFileName)
    } else {
        filesStatList <- readRDS(file=statsFileName)    
    }
    
    
    return(filesStatList)
}

# Load a subsample of the text data into R
getSampleCorpus <- function(corpusName,filesStatList,linesToRead = 1000,
                            sampleSize = NULL, useCache = TRUE) {
    if (!is.null(sampleSize)) {
        linesToRead <- as.integer(filesStatList[filesStatList$`Source name` == corpusName,"Number of lines"] * sampleSize)
        sampleFileName <- paste(initialDataDir,"/1-Sample/en_US/en_US.",sampleSize*100,
                                ".",corpusName,".txt", sep = "")
        corpusFileName <- paste("Data/1-SampleCorpus/en_US/en_US.",sampleSize*100,
                                ".",corpusName,".rds", sep = "")
    } else {
        sampleFileName <- paste(initialDqataDir,"/1-Sample/en_US/en_US.",linesToRead,
                                ".",corpusName,".txt", sep = "")
        corpusFileName <- paste("Data/1-SampleCorpus/en_US/en_US.",linesToRead,
                                ".",corpusName,".rds", sep = "")
    }

    if ((file.exists(corpusFileName) == FALSE) || (!useCache)) {
        ## Corpus file (in repository)
        dir.create(paste0("Data/1-SampleCorpus/en_US"), showWarnings = FALSE, recursive = TRUE)
    
        if (tolower(corpusName) == "all") {
            files <- rbind(
                c(paste(initialDataDir,"/final/en_US/en_US.",filesStatList[1,1],".txt", sep = ""),
                  as.integer(sampleSize * as.numeric(filesStatList[1,2]))),
                c(paste(initialDataDir,"/final/en_US/en_US.",filesStatList[2,1],".txt", sep = ""),
                  as.integer(sampleSize * as.numeric(filesStatList[2,2]))),
                c(paste(initialDataDir,"/final/en_US/en_US.",filesStatList[2,1],".txt", sep = ""),
                  as.integer(sampleSize * as.numeric(filesStatList[2,2])))
                )
        } else  {
            originFileName <- paste(initialDataDir,"/final/en_US/en_US.",corpusName,".txt", sep = "")    
        }
            
        ## Sample file (NOT in repository - cause is a large file)
        ## Verify if cached filed does not exists (OR explicity ignore cache)
        if ((file.exists(sampleFileName) == FALSE) || (!useCache)) {
            dir.create(paste0(initialDataDir,"/1-Sample/en_US"), showWarnings = FALSE, recursive = TRUE)
            
            ## Sample using shuf command
            
            if (tolower(corpusName) == "all") {
                ## Clean the file
                system(paste("echo \"\" ",">>",sampleFileName))
                for (i in 1:3) {
                   system(paste("shuf -n",files[i,2],files[i,1],">>",sampleFileName))    
                }
            } else {
                system(paste("shuf -n",linesToRead,originFileName,">",sampleFileName))    
            }
        }
        
        ## Read the sampled file
        con <- file(sampleFileName, "r")
        text <- readLines(con, encoding="UTF-8", skipNul = TRUE)
        close(con)
        corpusObj <- Corpus(VectorSource(text), readerControl=list(language="en_US"))
        
        saveRDS(corpusObj, file=corpusFileName)
    } else {
        corpusObj <- readRDS(file=corpusFileName)
    }
    return(corpusObj)
}

preProcessCorpus <- function(corpusName, corpusObj = NULL, useCache = TRUE, 
                             linesToRead = 1000, sampleSize = NULL) {
    if (is.null(profaneWords)) {
        con <- file("profanity-words.txt", "r")
        profaneWords <- readLines(con, encoding="UTF-8", skipNul = TRUE)
        close(con)
    }
    
    
    if (!is.null(sampleSize)) {
        cleanedFileName <- paste("Data/2-Clean/en_US/en_US.",sampleSize*100,
                                 ".",corpusName,".rds", sep = "")
    } else {
        cleanedFileName <- paste("Data/2-Clean/en_US/en_US.",linesToRead,
                                 ".",corpusName,".rds", sep = "")
    }
    

    ## Verify if cached filed does not exists (OR explicity ignore cache)
    if ((file.exists(cleanedFileName) == FALSE) || (useCache == FALSE)) {
        ## Creates the clean dir
        dir.create("Data/2-Clean/en_US", showWarnings = FALSE, recursive = TRUE)

        ## Remover URLS (Especialmente criado por mim!)
        # Too slow -- apply in other cenarios.Estou fazendo
        # The cutoff of the model will remove most of the urls
        #removeURL <- function(x) gsub(x=x,pattern = "((http(s)?:\\/\\/.)?(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,255}\\.[a-z]{2,6}([-a-zA-Z0-9@:%_\\+.~#?&//=]*\\/[-a-zA-Z0-9@:%_\\+.~#?&//=]*))|((http(s)?:\\/\\/)?www\\.[-a-zA-Z0-9@:%._\\+~#=]{2,255}\\.[a-z]{2,6}([-a-zA-Z0-9@:%_\\+.~#?&//=]*))|(http((s)?:\\/\\/)?(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,255}\\.[a-z]{2,6}([-a-zA-Z0-9@:%_\\+.~#?&//=]*))",replacement = " ")
        #corpusObj <- tm_map(corpusObj, content_transformer(removeURL));
        
        corpusObj <- tm_map(corpusObj, removePunctuation);
        corpusObj <- tm_map(corpusObj, removeNumbers);
        removeGarbage <- function(x) gsub(x=x,pattern = garbageInCorpus[i],replacement = " ")
        for (i in 1:length(garbageInCorpus)) {
            ## The content_transformer here is need cause the return of the remove garbage is a list
            corpusObj <- tm_map(corpusObj, content_transformer(removeGarbage));
        }
        ## Here we should make the url removal - and change the double+ points to ","    
        corpusObj <- tm_map(corpusObj, content_transformer(tolower));
        corpusObj <- tm_map(corpusObj, removeWords, profaneWords);
        corpusObj <- tm_map(corpusObj, stripWhitespace);
        saveRDS(corpusObj, file=cleanedFileName)
    } else {
        corpusObj <- readRDS(file=cleanedFileName)    
    }
    
    return(corpusObj)
}

processNGram <- function(corpusName, ngramValue = 3, corpusObj = NULL, cutoff = 1,
                               useCache = TRUE, linesToRead = 1000, sampleSize = NULL) {
    
    if (!is.null(sampleSize)) {
        ngramFileName <- paste("Data/3-NGramTDM/en_US/en_US.",sampleSize*100,
                               ".",corpusName,".",ngramValue,"-gram.",cutoff,
                               "-cutoff",".model.rds", sep = "")
    } else {
        ngramFileName <- paste("Data/3-NGramTDM/en_US/en_US.",linesToRead,
                               ".",corpusName,".",ngramValue,"-gram.",cutoff,
                               "-cutoff",".model.rds", sep = "")
    }
    
    ## Verify if cached filed does not exists (OR explicity ignore cache)
    if ((file.exists(ngramFileName) == FALSE) || (useCache == FALSE)) {
        ## Creates the clean dir
        dir.create("Data/3-NGramTDM/en_US", showWarnings = FALSE, recursive = TRUE)
        
        ## Do the job
        options(mc.cores=1)
        if (ngramValue == 1) {
            ngramModel <- TermDocumentMatrix(corpusObj, control = list(tokenize = words, bounds = list(global = c(1,Inf))))
            ngramModel <- rowapply_simple_triplet_matrix(ngramModel,sum)
            ngramModel <- data.frame(names(ngramModel),ngramModel,stringsAsFactors = F)
            names(ngramModel) <- c('gram','count')
        } else {
            tokenizerHelper <- function(x) {NGramTokenizer(x, Weka_control(min = ngramValue, max = ngramValue))}
            ## Using cutoffs of one or two for bigrams and trigrams can greatly
            ## decrease the size of a model, while yielding only a small 
            ## degradation in performance. (Chen and Goodman, 1998)
            print("Inicio do ngram")
            ngramModel <- TermDocumentMatrix(corpusObj, control = list(tokenize = tokenizerHelper, bounds = list(global = c(1,Inf))))
            ngramModel <- rowapply_simple_triplet_matrix(ngramModel,sum)
            print("Fim do ngram")
            
            print("Fazer a lista com strsplit")
            ngramNames <- strsplit(names(ngramModel), ' ')
            prevWordsList <- sapply(ngramNames,function(gram) paste(gram[1:ngramValue-1],collapse = " "))    
            nextWordList <- sapply(ngramNames, function(gram) {gram[ngramValue]})    
            
            
            ngramModel <- data.frame(names(ngramModel),ngramModel,
                                     prevWordsList,
                                     nextWordList,
                                     stringsAsFactors = F)
            names(ngramModel) <- c('gram','count','prevWords','nextWord')
            
            ## Making the cutoff
            ## Save the noncutted sum
            sumWithoutCutoff <- sum(ngramModel$count)
            ngramModel <- ngramModel[ngramModel$count > cutoff,]
            ngramModel <- list("model" = ngramModel, "sumOfCountsBeforeCutoff" = sumWithoutCutoff)
        }
        saveRDS(ngramModel, file=ngramFileName)
    } else {
        ngramModel <- readRDS(file=ngramFileName)  
    }
    return(ngramModel)
}

processNGramKNN <- function(corpusName, ngramValue = 3, ngramModel = NULL, 
                         useCache = TRUE, linesToRead = 1000, sampleSize = NULL) {
    
    if (!is.null(sampleSize)) {
        ngramKnnFileName <- paste("Data/4-KN_Model/en_US/en_US.",sampleSize*100,
                               ".",corpusName,".",ngramValue,"-gram-knn-model.rds", sep = "")
    } else {
        ngramKnnFileName <- paste("Data/4-KN_Model/en_US/en_US.",linesToRead,
                               ".",corpusName,".",ngramValue,"-gram-knn-model.rds", sep = "")
    }
    
	
    if (!file.exists(ngramKnnFileName)) {
        dir.create("Data/4-KN_Model/en_US", showWarnings = FALSE, recursive = TRUE)
        
        ngramNextWords <- unique(ngramModel$model$nextWord)
        ngramNextWordsContiCount <- unlist(mclapply(ngramNextWords, 
                function(w) length(ngramModel$model[ngramModel$model$nextWord == w,1]),
                mc.cores = 4))
        ngramKNNCC <- data.frame(ngramNextWords,ngramNextWordsContiCount,
                                  stringsAsFactors = F)
        names(ngramKNNCC) <- c('word','KNNContinuationCount')
        ngramModel$model$KNNContinuationCount <- unlist(mclapply(ngramModel$model$nextWord,
                    function(w) ngramKNNCC[ngramKNNCC$word == w,2],mc.cores = 4))
        
        saveRDS(ngramModel, file=ngramKnnFileName)
    } else {
        ngramModel <- readRDS(file=ngramKnnFileName)  
    }
    return(ngramModel)
}

makeCompleteNgramModels <- function(sampleSize, useCache = TRUE) {
    
    print("Getting basic stats...")
    times <- sum(system.time(filesStatList <- getGeneralTextStats())[c(1,4)])
    
    print("Getting Sample Corpus...")
    times  <- c(times,sum(system.time(
        allCorpus <- getSampleCorpus("all",filesStatList,
                                     sampleSize = sampleSize, useCache = useCache)
                )[c(1,4)]))
    
    print("Pre-process corpus...")
    times <- c(times,sum(system.time(
        allCorpus <- preProcessCorpus('all',corpusObj = allCorpus,
                                      sampleSize = sampleSize, useCache = useCache)
                )[c(1,4)]))
    
    ## Making n-grams
    ## Unigrams wont be necessary for the model
    print("Making unigrams count...")
    times <- c(times,sum(system.time(
        unigramModel <- processNGram('all', ngramValue = 1,corpusObj = allCorpus, 
                                     sampleSize = sampleSize, useCache = useCache)
                )[c(1,4)]))
    print("Making bigrams count...")
    times <- c(times,sum(system.time(
        bigramModel <- processNGram('all', ngramValue = 2,corpusObj = allCorpus, 
                                    sampleSize = sampleSize, useCache = useCache)
                )[c(1,4)]))
    
    print("Making trigrams count...")
    times <- c(times,sum(system.time(
        trigramModel <- processNGram('all', ngramValue = 3,corpusObj = allCorpus, 
                                     sampleSize = sampleSize, useCache = useCache)
                )[c(1,4)]))
    
    ## Calculating continuation count
    print("Calculation bigrams continuation count...")
    times <- c(times,sum(system.time(
        bigramModel <- processNGramKNN('all', ngramValue = 2,ngramModel = bigramModel, 
                                    sampleSize = sampleSize, useCache = useCache)
    )[c(1,4)]))

    timesDesc <- c("Generating Text Stats","Sampling Files","Pre-Process Corpus",
                   "Generate unigrams",
                   "Generate bigrams",
                   "Generate trigrams",
                   "Generate KN Continuation Count for bigrams")
    timeTable <- data.frame(times,row.names = timesDesc)
    names(timeTable) <- c('Processor time')
    
    if (!useCache) {
        saveRDS(timeTable, file=paste0("times-",sampleSize,".rds"))    
    }
    
}

# saveRDS(times, file="times-20.rds")    
# times <- readRDS(times, file="times-20.rds")

predictNextWord <- function(x,unigramModel,bigramModel,trigramModel,maxResults=3) {
    if (is.null(profaneWords)) {
        con <- file("profanity-words.txt", "r")
        profaneWords <- readLines(con, encoding="UTF-8", skipNul = TRUE)
        close(con)
    }

    ## Pre-proces the example as we did with the training set
    x <- removePunctuation(x)
    x <- removeNumbers(x)
    x <- tolower(x)
    x <- removeWords(x, profaneWords)
    x <- stripWhitespace(x)
    x <- NGramTokenizer(x,Weka_control(min = 1, max = 1))
    
    input <- paste(x[length(x)-1],x[length(x)-2])
    seektri<-grepl(paste0("^",input,"$"),trigramModel$prevWords)
    subtri<-trigramModel[seektri,]
    input2 <- unlist(strsplit(input," "))[2]
    seekbi <- grepl(paste0("^",input2,"$"),bigramModel$prevWords)
    subbi <- bigramModel[seekbi,]
    unigramModel$s <- unigramModel$count/nrow(unigramModel)*0.16
    useuni <- unigramModel[order(unigramModel$s,decreasing = T),]
    useunia <- useuni[1:maxResults,]
    
    if (sum(seektri) == 0) {
        if(sum(seekbi)==0){
            return(head(unigramModel[order(unigramModel$count,decreasing = T),1],maxResults))
        }
        subbi$s <- 0.4*subbi$count/sum(seekbi)
        names <- c(subbi$nextWord,useunia$gram)
        score <- c(subbi$s,useunia$s)
        predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
        predictWord <- predictWord[order(predictWord$score,decreasing = T),]
        # in case replicated
        final <- unique(predictWord$next_word)
        return(final[1:maxResults])
    } 
    subbi$s <- 0.4*subbi$count/sum(seekbi)
    subtri$s <- subtri$count/sum(subtri$count)
    names <- c(subtri$nextWord,subbi$nextWord,useunia$gram)
    score <- c(subtri$s,subbi$s,useunia$s)
    predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
    predictWord <- predictWord[order(predictWord$score,decreasing = T),]
    # in case replicated
    final <- unique(predictWord$next_word)
    final <- final[1:maxResults]
    return(final)
}

## Sequence of use
## 1. BasicStats:   filesStatList <- getGeneralTextStats("Data/final/en_US/en_US.*")
## 2. Sample:       blogCorpus <- getSampleCorpus('blogs',filesStatList)
## 3. Pre-Process:  blogCorpus <- preProcessCorpus('blogs',corpusObj = blogCorpus)
## 4. NGram-tokenization

#     ## Testing with quiz questions
#
# unigramModel <- processNGram('all', ngramValue = 1,corpusObj = allCorpus, sampleSize = 0.001)
# bigramModel <- processNGramKNN('all', ngramValue = 2,ngramModel = bigramModel, sampleSize = 0.001)
# trigramModel <- processNGramKNN('all', ngramValue = 3,ngramModel = trigramModel, sampleSize = 0.001)
# textList <- c("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
#               "The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
# knnModel <- NextWordModel(bigramModel = bigramModel,trigramModel = trigramModel)
# predictNextWord(knnModel,text,5)
# saveRDS(knnModel,paste0("shiny-app/data/final-model-",sampleSize,".rds"))

# unigramBlogValues <- data.frame(table(unigramBlog))



# URL Testing

# con <- file("urltext.txt", "r")
# textURL <- readLines(con, encoding="UTF-8", skipNul = TRUE)
# gsub(textURL[1],pattern = "((http(s)?:\\/\\/.)?(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*\\/[-a-zA-Z0-9@:%_\\+.~#?&//=]*))|((http(s)?:\\/\\/)?www\\.[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*))|(http((s)?:\\/\\/)?(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*))"
# ,replacement = " ")