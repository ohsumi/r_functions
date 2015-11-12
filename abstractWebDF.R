abstractWebDF <- function(name, IdDF, IDText){

    clean <- function(df){
        df <- as.data.frame(df)
        for(i in 1:length(rownames(df))){
            rownames(df)[i] <- i
        }
        return(invisible(df))
    }
    
    evaluate <- function(df){
        
        df <- as.data.frame(df)
        if(length(colnames(df)) < 3){
            v <- as.vector(df[, 2])
            index <- grep(TRUE,v > 0)
            return(invisible(df[index,]))
        }else{
            indexes <- c()
            DFwithoutSampleIDE <- df[, -1]
            for(i in 1:length(rownames(DFwithoutSampleIDE))){
                s <- sum(as.numeric(DFwithoutSampleIDE[i,]))
                if(s >= 1){
                    indexes <- c(indexes, i)
                }
            }
            return(invisible(df[indexes,]))
        }
    }
    
    findClientID <- function(name, data){
        IDs <- c()
        indexes <- c()
        for(i in 1:length(data)){
            splited <- unlist(strsplit(data[i], ","))
            id <- splited[1]
            company <- splited[2]
            if(company == name){
                IDs <- c(IDs,id)
                indexes <- c(indexes, i)
            }
        }
        return(list(data[indexes], IDs))
    }
    
    getSubset <- function(df, ids){
        indexes <- c()
        massIDs <- colnames(df)
        for(i in 1:length(massIDs)){
            for(j in 1:length(ids)){
                if(massIDs[i]==ids[j]){
                    indexes <- c(indexes, i)
                }
            }
        }
        return(invisible(as.data.frame(df[, c(1, indexes)])))
    }
    
    l <- findClientID(name, IDText)
    info <- as.vector(l[[1]])
    ids  <- as.vector(l[[2]])

    if(length(ids)==0){
        return(invisible(list()))
    }

    result <- getSubset(IdDF, ids)
    result <- evaluate(result)
    if(length(rownames(result))==0){
        return(invisible(list()))
    }
    return(invisible(list(info, clean(result))))
}
