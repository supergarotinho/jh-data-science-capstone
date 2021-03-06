######################################################################
# Create the NextWordModel class
#
# This is used to represent the complete next word model.
NextWordModel <- setClass(
    # Set the name for the class
    "NextWordModel",
    
    # Define the slots
    slots = c(
        unigramModel = "data.frame",
        bigramModel = "data.frame",
        sumOfBigramsCountsBeforeCutoff = "numeric",
        sumOfBigramsCountAfterCutoff = "numeric",
        trigramModel = "data.frame",
        sumOfTrigramsCountsBeforeCutoff = "numeric",
        sumOfTrigramsCountAfterCutoff = "numeric",
        profaneWords = "character"
    ),
    
    # Set the default values for the slots. (optional)
    prototype=list(
        unigramModel = NULL,
        bigramModel  = NULL,
        sumOfBigramsCountsBeforeCutoff = 0,
        sumOfBigramsCountAfterCutoff = 0,
        trigramModel = NULL,
        sumOfTrigramsCountsBeforeCutoff = 0,
        sumOfTrigramsCountAfterCutoff = 0,
        profaneWords = NULL,
        discount = 0.75
    ),
    
    # Make a function that can predic the next word of a string!
    validity = function(object)
    {
        if(is.null(object@bigramModel) || 
           is.null(object@trigramModel)) {
            return("Must specify the unigram, bigram and trigram models.")
        }
        return(TRUE)
    }
)

NextWordModel = function(unigramModel, bigramModel, 
                         trigramModel, profaneWords = NULL, ...) {
    
    # Pre-load the profane words for the model
    if (is.null(profaneWords)) {
        con <- file("profanity-words.txt", "r")
        profaneWords <- readLines(con, encoding="UTF-8", skipNul = TRUE)
        close(con)
    }
    
    ## Gets only the most frequent unigrams
    unigramModel <- unigramModel[order(-unigramModel[,"count"]),]
    unigramModel <- unigramModel[1:10,]
    
    ## Gets the sum of trigrams and bigrams
    sumOfBigramsCountAfterCutoff <- sum(trigramModel$model$count)
    sumOfTrigramsCountAfterCutoff <- sum(bigramModel$model$count)
    
    ## Creates the class
    modelObj = new("NextWordModel",unigramModel=unigramModel,bigramModel=bigramModel$model,
                   sumOfBigramsCountsBeforeCutoff=bigramModel$sumOfCountsBeforeCutoff,
                   sumOfBigramsCountAfterCutoff=sumOfBigramsCountAfterCutoff,
                   trigramModel=trigramModel$model,
                   sumOfTrigramsCountsBeforeCutoff = trigramModel$sumOfCountsBeforeCutoff,
                   sumOfTrigramsCountAfterCutoff=sumOfTrigramsCountAfterCutoff,
                   profaneWords=profaneWords, ...)
    
    return (modelObj)
}
    
setGeneric(name="predictNextWord",
           def=function(theObject,text,numberOfWords=5)
           {
               standardGeneric("predictNextWord")
           }
)

setMethod(f="predictNextWord",
          signature="NextWordModel",
          definition=function(theObject,text,numberOfWords=5)
          {
              ## Pre-proces the example as we did with the training set
              ## Remover URLS (Especialmente criado por mim!)
              text <- gsub(x=text,pattern = "((http(s)?:\\/\\/.)?(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,255}\\.[a-z]{2,6}([-a-zA-Z0-9@:%_\\+.~#?&//=]*\\/[-a-zA-Z0-9@:%_\\+.~#?&//=]*))|((http(s)?:\\/\\/)?www\\.[-a-zA-Z0-9@:%._\\+~#=]{2,255}\\.[a-z]{2,6}([-a-zA-Z0-9@:%_\\+.~#?&//=]*))|(http((s)?:\\/\\/)?(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,255}\\.[a-z]{2,6}([-a-zA-Z0-9@:%_\\+.~#?&//=]*))",replacement = " ")
              text <- removePunctuation(text)
              text <- removeNumbers(text)
              text <- tolower(text)
              text <- removeWords(text, theObject@profaneWords)
              text <- stripWhitespace(text)
              text <- NGramTokenizer(text,Weka_control(min = 1, max = 1))
              n <- length(text)
              discount <- 0.75
              
              if (n >=2) {
                  ## Calculating trigrams probabilties
                  possibleTrigrams <- theObject@trigramModel[theObject@trigramModel$prevWords == paste(text[(n-1):n],collapse = " "),]
                  sumOfCountOfPossibleTrigrams <- sum(possibleTrigrams$count)
                  possibleTrigrams$probability <- (possibleTrigrams$count - discount)/sumOfCountOfPossibleTrigrams
                  if (nrow(possibleTrigrams[possibleTrigrams$model$probability < 0,]) > 0) {
                      possibleTrigrams[possibleTrigrams$probability < 0,]$probability <- 0
                  }
                  possibleTrigrams <- possibleTrigrams[,c("nextWord","probability")]
                  
                  ## Calculating how much probability will be passed to the 
                  ## bigram counts
                  
                  ## We dont pass all of the probability mass of the prunned 
                  ## words (UNK words) to the lower ngram model because it increments a lot the memory requirements
                  
                  ### unkWordsMass <- theObject@sumOfTrigramsCountsBeforeCutoff - theObject@sumOfTrigramsCountAfterCutoff
                  ###  lambdaBigram <- (nrow(possibleTrigrams) * discount) + unkWordsMass / theObject@sumOfTrigramsCountsBeforeCutoff  
                  
                  ## Instead, we did a simpler aprroach
                  if (sumOfCountOfPossibleTrigrams > 0) {
                      lambdaBigram <- nrow(possibleTrigrams) * discount / sumOfCountOfPossibleTrigrams  
                  } else {
                      lambdaBigram <- 1
                  }
              } else {
                  lambdaBigram <- 1
              }
              
              if (n >=1) {
                  ## Calculating bigrams probabilities with continuation count
                  possibleBigrams <- theObject@bigramModel[theObject@bigramModel$prevWords == text[(n)],]
                  possibleBigrams$probability <- lambdaBigram*(possibleBigrams$KNNContinuationCount - discount)/nrow(theObject@bigramModel)
                  if (nrow(possibleBigrams[possibleBigrams$probability < 0,]) > 0) {
                      possibleBigrams[possibleBigrams$probability < 0,]$probability <- 0
                  }
                  possibleBigrams <- possibleBigrams[,c("nextWord","probability")]
                  
                  ## Unigrams are not used since (Jurafsky and Martin - Speech and 
                  ## Language Processing 3rd edition - not yet published - 4o chapter)
                  ## unigrams will be modeled as a uniform distribution. And its 
                  ## probabilities will be equal.
                  if ((n>=2) && (nrow(possibleTrigrams) > 0)) {
                      resultList <- rbind(possibleTrigrams,possibleBigrams)    
                  } else {
                      resultList <- possibleBigrams
                  }
                  if (nrow(resultList) > 0) {
                      finalResult <- aggregate(resultList[,"probability"], by=list(resultList$nextWord), FUN=sum)
                      names(finalResult) <- c("nextWord","probability")
                      finalResult <- finalResult[order(-finalResult[,"probability"]),]
                  } else {
                      ## Unigrams can be used here
                      return(head(theObject@unigramModel$gram,5))
                  }
              } else {
                  ## Unigrams could be used here
                  return(head(theObject@unigramModel$gram,5))
              } 

              return(head(finalResult$nextWord,numberOfWords))
          }
)