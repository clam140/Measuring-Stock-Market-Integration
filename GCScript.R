
symbolVec <- c("^FTSE", "^SSEC", "^BSESN","^GSPC", "^N225")
# in addtion, user must import a csv file or data frame titled "leadConditions" 
# with three columns, "Response" , "Explanatory", and "leadEffect"

# for example 
# Response	Explanatory	leadEffect
# FTSE	        BSESN	        FALSE
# GSPC	        BSESN	        FALSE
# N225	        BSESN	        TRUE

# etc ...

# NOTE: elements in "indices" vector must correspond exactly to Yahoo!Finance symbols("^SSEC"), 
# while Response and Explanatory columns only allow alphanumeric characters ("SSEC")

grangerSingleMix <- function(x, y, leadEffect) {
        require(quantmod)
        require(forecast)
        require(lmtest)
        require(vars)
        require(dplyr)
        require(plyr)
        require(tseries)
                symbols <- getSymbols(c(x,y))
                symbolList <- lapply(1:2, function(i) {get(symbols[i])})

        df.List <- lapply(symbolList, FUN = function(df){
                return(data.frame(date = index(df), coredata(as.numeric(df[ ,6]))))
        })
        
        combinedDF <- df.List %>% Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="date"), .)
        names(combinedDF) <- c("date", symbols)
        
        ndif <- sapply(2:ncol(combinedDF), FUN = function(i) {
                ndiffs(combinedDF[,i], alpha = 0.05, test = "adf")
        })
        
        if(dim(table(ndif)) == 1){
                totalDF <- as.data.frame(na.omit(diff(as.matrix(combinedDF[2:3]), 1)))     
        }
        x <- gsub("\\^","",x)
        y <- gsub("\\^","",y)
        if(leadEffect == FALSE){
                        # res <- resid(lm(combinedDF[ ,Y] ~ combinedDF[, X]))
                        # Pcointegrate <- adf.test(res)[[4]]
                        p <- VARselect(data.frame(totalDF[ ,y], totalDF[ ,x]))$selection[[1]]
                        grangerResults <- grangertest(totalDF[ ,y] ~ totalDF[ ,x], order = p)
                } else {
                        # res <- resid(lm(na.omit(lead(combinedDF[ ,Y])) ~ combinedDF[ ,X][1:(length(combinedDF[ ,X]) - 1)]))
                        # Pcointegrate <- adf.test(res)[[4]]
                        p <- VARselect(data.frame(na.omit(lead(totalDF[ ,y])), totalDF[ ,x][1:(length(totalDF[ ,x]) - 1)]))$selection[[1]]
                        grangerResults <- grangertest(na.omit(lead(totalDF[ ,y])) ~ totalDF[ ,x][1:(length(totalDF[ ,x]) - 1)], order = p)
                }
                return(data.frame(response = y,
                                  explanatory = x,
                                  lagLength = p, 
                                  Fvalue = grangerResults[c(3,4)][[1]][2], 
                                  Pvalue = grangerResults[c(3,4)][[2]][2]
                            )
                )
}
mapply(grangerMix, c("^SSEC", "^GSPC"), c("^GSPC", "^SSEC"), c(FALSE,TRUE))

# combinedDF <- na.omit(combinedDF)
leadConditions <- read.csv("leadConditions.csv")
leadConditions$Response <- as.character(leadConditions$Response)
leadConditions$Explanatory <- as.character(leadConditions$Explanatory)
leadEffect <- leadConditions[leadConditions$Response == Y & leadConditions$Explanatory == X, 3]


library(quantmod)
# data vis
getSymbols(symbolVec)
chart_Series(c(FTSE,SSEC,GSPC,N225,BSESN))


# nonstationary vs stationary demonstration
chart_Series(FTSE)
chart_Series(diff(FTSE))



# TOTAL
symbolVec <- c("^FTSE", "^SSEC", "^BSESN","^GSPC", "^N225")
grangerMix <- function(indices = symbolVec) {
        require(quantmod)
        require(forecast)
        require(lmtest)
        require(vars)
        require(dplyr)
        require(plyr)
        require(tseries)
        if(length(indices) >= 2) {
                symbols <- getSymbols(indices, from = "2007-01-02", to = "2017-02-02")
                symbolList <- lapply(1:length(indices), function(i) {get(symbols[i])})
        } else {
                return(paste("Please use character vector of length of 2"))
        } 
        df.List <- lapply(symbolList, FUN = function(df){
                return(data.frame(date = index(df), coredata(as.numeric(df[ ,6]))))
        })
        
        combinedDF <- df.List %>% Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="date"), .)
        names(combinedDF) <- c("date", symbols)
        
        ndif <- sapply(2:ncol(combinedDF), FUN = function(i) {
                ndiffs(combinedDF[,i], alpha = 0.05, test = "adf")
        })
        
        if(dim(table(ndif)) == 1){
                totalDF <- as.data.frame(na.omit(diff(as.matrix(combinedDF[2:ncol(combinedDF)]), 1)))     
        }
        
        # combinedDF <- na.omit(combinedDF)
        leadConditions <- read.csv("leadConditions.csv")
        leadConditions$Response <- as.character(leadConditions$Response)
        leadConditions$Explanatory <- as.character(leadConditions$Explanatory)
        
        Output <- cbind(leadConditions, data.frame(t(mapply(function(Y, X){
                leadEffect <- leadConditions[leadConditions$Response == Y & leadConditions$Explanatory == X, 3]
                if(leadEffect == FALSE){
                        # res <- resid(lm(combinedDF[ ,Y] ~ combinedDF[, X]))
                        # Pcointegrate <- adf.test(res)[[4]]
                        p <- VARselect(data.frame(totalDF[ ,Y], totalDF[ ,X]))$selection[[1]]
                        grangerResults <- grangertest(totalDF[ ,Y] ~ totalDF[ ,X], order = p)
                } else {
                        # res <- resid(lm(na.omit(lead(combinedDF[ ,Y])) ~ combinedDF[ ,X][1:(length(combinedDF[ ,X]) - 1)]))
                        # Pcointegrate <- adf.test(res)[[4]]
                        p <- VARselect(data.frame(na.omit(lead(totalDF[ ,Y])), totalDF[ ,X][1:(length(totalDF[ ,X]) - 1)]))$selection[[1]]
                        grangerResults <- grangertest(na.omit(lead(totalDF[ ,Y])) ~ totalDF[ ,X][1:(length(totalDF[ ,X]) - 1)], order = p)
                }
                return(list(lagLength = p, 
                            Fvalue = grangerResults[c(3,4)][[1]][2], 
                            Pvalue = grangerResults[c(3,4)][[2]][2]
                )
                )
        }, 
        Y = leadConditions$Response, 
        X = leadConditions$Explanatory)
        ) # transpose
        ) # data.frame
        ) # cbind
        
        GrangerResults <- do.call(cbind, Output)
        row.names(GrangerResults) <- NULL  # removes uncessary row names
        return(GrangerResults)
}

