abstractTVDF <- function(client, IdDF, DFdata){
    
    abstract <- function(df){
        df <- as.data.frame(df)
        #remove sampleID.
        #subset is a dataframe without sampleID.
        subset <- df[, -1]
        subColNum <- length(colnames(subset))
        subRowNum <- length(rownames(subset))
        index <- c()
        for(i in 1:subRowNum){
            total <- sum(as.numeric(subset[i,]))
            # if (total == 0) => TRUE
            # he/she didn't watch the all programs
            # related to the input client.
            if(total > 0){
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
    
    checkStr <- function(string){
        if((bk <- unlist(regexpr("/", string)) >= 1)){
            if((sp <- unlist(regexpr(" ", string))) >= 1){
                return(TRUE)
            }
        }
        return(FALSE)
    }
    
    getIndex <- function(strings, client){
        index <- c()
        for(i in 1:length(strings)){
            if((val <- unlist(regexpr("/", strings[i]))) > 0){
                splited <- unlist(strsplit(strings[i], "/"))
                for(u in 1:length(splited)){
                    if(splited[u] == client){
                        index <- c(index, i)
                    }
                }
            }else {
                if(strings[i] == client){
                    index <- c(index, i)
                }
            }
        }
        return(invisible(index))
    }

    #this function doesn't have exception handlings.
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
    
    programIndex <- getIndex(as.character(DFdata$client),client) #cp 
    
    # failed to find the client.
    if(length(programIndex) == 0) {
        return(list())
    }
    #mat is a matrix data
    #which explains all advertisement contents.
    mat <- DFdata[programIndex,]
    #TV program code.
    code <- mat[,1]
    #indexes for mgzn dataframe
    index <- findCode(IdDF,unique(code))
    #1 => sampleID . make dataframe with sampleID.
    df <- IdDF[,c(1,index)]
    #make dataframe clean
    df <- abstract(df)
    df <- clean(df)
    #make list of dataframes.
    DFList <- c(list(mat),list(df))
    return(invisible(DFList))
}
