indices <- c("^FTSE", "^SSEC", "^BSESN","^GSPC", "^N225")
require(quantmod)
require(forecast)
require(lmtest)
require(vars)
require(dplyr)
require(plyr) 
require(tseries)
symbols <- getSymbols(indices, from = "2007-01-02", to = "2017-02-02")
symbolList <- lapply(1:length(indices), function(i) {get(symbols[i])})

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
                p <- VARselect(data.frame(totalDF[ ,Y], totalDF[ ,X]))$selection[[1]]
                grangerResults <- grangertest(totalDF[ ,Y] ~ totalDF[ ,X], order = p)
        } else {
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
GrangerResults


