#' Compute Kneser-Ney language model's next word probability
#'
#' @param text A cleaned up and preprocessed version of the text given as input
#' @param ngramEnv An environment which contains the ngrams data frames which will be used in the model
#' @param ngramList A character which contains the names of the data frames contains the ngrams.
#'                  Obs.: each ngram data.frame must have two columns:
#'                         \itemize{
#'                            \item \code{ngram} the n-gram
#'                            \item \code{counts} number of times the n-gram appeard in the whole corpus
#'                         }
#' @param numGuesses An integer which defines the number of candidate words that will be returned.
#' @param doChecks A flag indicating if any validation checks should be carrid out.
#'
#' @details It's assumed that all ngrams are ordered in decreasing counts
#'
#' @return A data frame containing two columns
#' \itemize{
#'   \item \code{PREDICTION} a next word candidate
#'   \item \code{PROBABILITY} the word's KN probability
#' }
#' @import data.table
#' @import stringr
#' @export
probKneserNey <- function(text, ngramEnv, ngramList, numGuesses, doChecks = T){
  
  require(data.table)
  require(stringr)
  
  # extract the words of the input text that will be used to predict the next word
  words <- unlist(strsplit(text, split = " "))
  numWords <- length(words)
  
  ngramList <- ngramList[max(1,(length(ngramList)-numWords)):length(ngramList)]
  if (length(ngramList) < 2) {
    stop("At least one word must be given.")
  }
  
  excessFactor <- 2
  
  # variables to keep the subsets of the ngram data.frames which contain the last part of the input text
  dfSubNgram <- NULL        # this one keeps track of the ngrams
  
  # find candidate words
  
  # create a dataframe to keep track of the probabilities of candidate words
  dfProbs <- data.table(
    word = character(length = excessFactor * numGuesses), # keep track of excessFactor x numGuesses words
    prob = numeric(length = excessFactor * numGuesses)
  )
  
  for (i in 1:length(ngramList)){
    
    # find out the order of the ngrams
    ngramOrder <- (length(ngramList)-i+1)
    
    # find out what is the "input gram":
    # browser()
    inputGram <- stringr::str_trim(
      paste0(words[max(1, (numWords - ngramOrder + 2)):numWords],
             collapse = " ")
    )
    
    # extract first word of "input gram":
    firstWord <- regmatches(inputGram,
                            regexpr(pattern = "^\\w+",
                                    text = inputGram))
    
    # get ngram data frame
    dictKeys <- eval(
      parse(
        text = paste0('list.files(ngramEnv$', ngramList[i], ')')
      )
    )
    pos <- max(1,max(which(sort(c(dictKeys, paste0(firstWord, ".rds"))) == paste0(firstWord, ".rds"))-1))
    
    # get ngram data frame
    # chunk <- paste0("ngramEnv$", ngramList[i], "$dict$.", dictKeys[pos])
    dictPath <- eval(parse(text = paste0('ngramEnv$', ngramList[i])))
    dfSubNgram <- readRDS(file = paste0(dictPath,  dictKeys[pos]))
    
    # find ngram candidates
    if (ngramOrder == 1){
      dfSubNgram <- unique(
        rbind(
          dfSubNgram[ngram %in% dfProbs$word,],
          dfSubNgram[1:min(nrow(dfSubNgram), max(20, excessFactor * numGuesses)),]
        )
      )
    }else{
      # find the subset of ngrams which begin with the last words of the input
      dfSubNgram <- dfSubNgram[grep(paste0("(^|\\s)(", inputGram, ")\\s"), dfSubNgram$ngram),]
    }
    
    # save last word of each ngram
    # browser()
    dfSubNgram$nextWord <- ""
    nextWordAux1 <- stringr::str_trim(gsub(x = dfSubNgram$ngram, pattern = paste0(inputGram, "\\s"), replacement = ""))
    nextWordAux2 <- regexpr("\\w+$", nextWordAux1)
    dfSubNgram[which(nextWordAux2>0),]$nextWord <- regmatches(nextWordAux1, nextWordAux2)
    dfSubNgram <- dfSubNgram[which(nextWordAux2>0),]
    rm(nextWordAux1, nextWordAux2)
    
    # save lower order (n-1)gram
    dfSubNgram$lowerGram <- stringr::str_trim(gsub(x = dfSubNgram$ngram, pattern = "^\\w+\\s", replacement = ""))
    
    # order candidate ngrams in descending order of counts
    dfSubNgram <- dfSubNgram[order(-counts)]
    
    # save the subset of candidate ngrams in the ngramEnv. The subset's prefix will be "sub"
    eval(parse(text = paste("ngramEnv$sub", " <- dfSubNgram", sep = ngramList[i])))
    
    # find out which empty slots are left in dfProbs
    emptySlots <- which(dfProbs$word == "", arr.ind = T)
    numEmptySlots <- length(emptySlots)
    
    if (numEmptySlots > 0){
      
      # create a list of candidate words from the ngrams and (n-1)grams extracted above
      newCandidates <- unique(dfSubNgram$nextWord)
      
      # remove candidates that are already in dfProbs
      newCandidates <- setdiff(x = newCandidates, y = dfProbs$word)
      
      # if we reached the unigram level, complete candidate list with the most frequent unigrams
      if (ngramOrder == 1){
        newCandidates <- setdiff(dfSubNgram[seq(excessFactor*numGuesses),]$ngram, dfProbs[word != "",]$word)
      }
      
      # find out how many new candidates were chosen
      numCandidates <- length(newCandidates)
      
      if (numCandidates > 0){
        # include new candidates in dfProbs
        dfProbs[emptySlots[1:min(numCandidates,numEmptySlots)],]$word <- newCandidates[1:min(numCandidates,numEmptySlots)]
      }
    }
    
  }
  
  probKN <- function(currWord, currNgram){
    
    # the parameter currWord stands for the current word for which the kneser-ney
    # probability is being computed
    
    # the parameter currNgram stands for the current index of the highest order
    # ngram to be used in the current probKN call
    
    # to keep the calling stack at a minimum, let's get the enclosing function's
    # parameters from the enclosing environment
    
    # get enclosing environment
    encEnv <- environment(probKN)
    
    # get the words of the input text that will be used to predict the next word
    words <- encEnv$words
    numWords <- encEnv$numWords
    
    # get ngramList from enclosing environment
    ngramList <- encEnv$ngramList
    
    # find out the current ngram order
    ngramOrder <- length(ngramList) - currNgram + 1
    
    # get the subset of ngram candidates
    chunk <- paste("ngramEnv",
                   paste0("sub", ngramList[currNgram]),
                   sep = "$")
    dfCurrOrderNgramCandidates <- data.table(eval(parse(text = chunk)))
    
    # get the subset of (n-1)gram candidates
    chunk <- paste("ngramEnv",
                   paste0("sub", ngramList[currNgram+1]),
                   sep = "$")
    dfLowerOrderNgramCandidates <- data.table(eval(parse(text = chunk)))
    
    if (ngramOrder < length(ngramList)){
      # get the subset of (n+1)gram candidates
      chunk <- paste("ngramEnv",
                     paste0("sub", ngramList[currNgram-1]),
                     sep = "$")
      dfHigherOrderNgramCandidates <- data.table(eval(parse(text = chunk)))
    }
    
    # discount factor
    chunk <- paste("ngramEnv",
                   paste0("SummaryGT.", ngramList[currNgram]),
                   sep = "$")
    dtSummary <- eval(parse(text = chunk))
    for (k in 1:4){
      chunk <- paste0("n", k, " <- ", "dtSummary[c == ", k, ",]$N")
      eval(parse(text = chunk))
    }
    
    # Y <- 0.0075
    Y <- n1/(n1 + 2*n2)
    for (k in 1:3){
      chunk <- paste0("D", k, " <- ", k, " - ", (k+1), "*Y*n", (k+1), "/n", k)
      eval(parse(text = chunk))
    }
    
    if (ngramOrder > 1){
      
      # find out N1(w●), N2(w●) and N3+(w●)
      N1wdot <- sum(dfLowerOrderNgramCandidates$counts == 1)
      N2wdot <- sum(dfLowerOrderNgramCandidates$counts == 2)
      N3wdot <- sum(dfLowerOrderNgramCandidates$counts > 2)
      
      # compute probability mass set aside for unseen ngrams
      d <- D1*N1wdot + D2*N2wdot + D3*N3wdot
      
      if (ngramOrder == length(ngramList)){ # the current ngram order is the highest
        d <- d / sum(dfLowerOrderNgramCandidates$counts)
      }else{ # the current order isn't the highest
        
        # N1+(●w●): <---- COME BACK LATER
        N1dotwdot <- nrow(dfLowerOrderNgramCandidates)
        # N1dotwdot <- dfLowerOrderNgramCandidates[ngram == dfCurrOrderNgramCandidates[nextWord == currWord,]$lowerGram, ]$counts
        
        d <- d / ifelse(test = N1dotwdot > 0,
                        yes = N1dotwdot,
                        no = sum(dfLowerOrderNgramCandidates$counts))
      }
      
      if(is.nan(d)){d <- 1}
    }
    
    # absolute discounting adjusted model
    
    # browser()
    # c(●w) -> count of [prefix + current_word] ngrams
    cdotw <- dfCurrOrderNgramCandidates[nextWord == currWord,]$counts
    if (length(cdotw) == 0){
      cdotw <- 0
    }
    
    # if (length(cdotw) > 1){
    #   browser()
    # }
    
    # choose appropriate discount
    D <- 0
    if( cdotw > 2 ){
      D <- D3
    }
    if( cdotw == 2 ){
      D <- D2
    }
    if( cdotw == 1 ){
      D <- D1
    }
    
    # c(●●) -> count of [prefix + any_word] ngrams
    cdotdot <- sum(dfCurrOrderNgramCandidates$counts)
    
    # # get the (n-1)gram model
    # chunk <- paste("ngramEnv",
    #                ngramList[currNgram+1],
    #                sep = "$")
    # dfLowerNgram <- data.table(eval(parse(text = chunk)))
    # cdotdot <- dfLowerNgram[ngram == dfCurrOrderNgramCandidates[nextWord == currWord,]$lowerGram,]$counts
    if (length(cdotdot) == 0){
      cdotdot <- 0
    }
    
    if (ngramOrder == length(ngramList)){
      # since we are at the highest order ngram we will use the traditional counts
      
      alpha <- ifelse(
        test = cdotdot > 0,
        yes = max(0,cdotw - D)/cdotdot,
        no = 0)
      
    }else{
      
      # N1+(●w) -> history of [prefix + current_word]
      N1dotw <-
        sum(dfHigherOrderNgramCandidates$lowerGram == dfCurrOrderNgramCandidates[nextWord == currWord, ]$ngram)
      
      # N1(●●) -> history of [prefix + any_word] ngrams
      N1dotdot <- nrow(dfHigherOrderNgramCandidates)
      
      # use counts of histories for the lower order ngrams
      alpha <- ifelse(
        test = N1dotw > 0,
        yes = max(0,N1dotw - D)/N1dotdot,
        no = max(0,cdotw - D)/cdotdot)
    }
    if(is.nan(alpha)){alpha <- 0}
    
    
    # the general case:
    if (currNgram < length(ngramList)){
      
      prob <- alpha + d * probKN(currNgram = currNgram + 1, currWord = currWord)
      
      # the base case:
    }else {
      
      prob <- alpha
      
      return(prob)
    }
  }
  
  for (w in seq(dfProbs$word)){
    dfProbs[w,]$prob <- probKN(currWord = dfProbs[w,]$word, currNgram = 1)
  }
  
  
  return(dfProbs[order(-prob)][1:numGuesses])
  
}
