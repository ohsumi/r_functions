abstractMagazineDF <- function(client, IdDF, DFdata){
    
    abstractDF <- function(df){
        
        df <- as.data.frame(df)
        # remove sampleID.
        # "subset" variable is a dataframe without sampleID.
        subset <- df[,-1]
        #make variables for loop process.
        subColNum <- length(colnames(subset))
        subRowNum <- length(rownames(subset))
        index <- c()
        for(i in 1:subRowNum){
            line <- subset[i,]
            total <- sum(as.numeric(line))
            # if(total > subColNum) => TRUE
            # he/she watched the magazine
            # related to the input client.
            if(total > subColNum){
                index <- c(index, i)
            }
        }
        #return DF with sampleID.
        return(invisible(df[index,]))
    }
    
    #make input rownames of dataframe clean
    clean <- function(df){
        num <- length(rownames(df))
        for(i in 1:num){
            rownames(df)[i] <- i
        }
        return(invisible(df))
    }
    
    getIndex <- function(strings,client){
        index <- c()
        for(i in 1:length(strings)){
            if((val <- unlist(regexpr("/", strings[i]))) > 0){
                splited <- unlist(strsplit(strings[i],"/"))
                for(u in 1:length(splited)){
                    if(splited[u] == client){
                        index <- c(index, i)
                    }
                }
            }else{
                if(strings[i] == client){
                    index <- c(index, i)
                }
            }
        }
        return(invisible(index))
    }
    #this function doesn't have exception handlings for NAs.
    findCode <- function(df,codes){
        cols <- colnames(df)
        result <- c()
        for(i in 1:length(codes)){
            for(j in 1:length(cols)){
                if(codes[i] == cols[j]){
                    result <- c(result,j)
                }
            }
        }
        return(invisible(result))
    }
   
    # new version.
    mgIndex <- getIndex(as.character(DFdata$client), client)
    if(length(mgIndex) == 0) {
        #return one empty list.
        return(list())
    }
    #"mgMatrix" variable is one of return values.
    mgMatrix <- DFdata[mgIndex,]
    #MagazineCode
    codes <- mgMatrix[, 1]
    #indexes for IdDF dataframe
    index <- findCode(IdDF,unique(codes))
    #1 => a fixed number , sampleID . make one dataframe with sampleID.
    df <- IdDF[, c(1, index)]
    #abstract readers
    df <- clean(abstractDF(df))
    #make one list as a return value.
    DFList <- c(list(mgMatrix), list(clean(df)))
    #Termination processing.
    return(invisible(DFList))
    
}
