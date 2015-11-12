abstractNewsPaperDF <- function(name, IdDF, NPdata, DFdata){
    
    clean <- function(df){
        df <- as.data.frame(df)
        for(i in 1:length(rownames(df))){
            rownames(df)[i] <- i
        }
        return(df)
    }

    #the processing for binary variable col line.
    abstract <- function(l){
    	
        if(length(l)==0){
            #return blank list
            return(invisible(list()))
        }else{
            df <- as.data.frame(l)
            #remove sampleID line.
            df <- df[, -1]
            indexes <- c()
            for(i in 1:length(rownames(df))){
                #2 means => "YES"
                num <- length(grep(2, as.numeric(df[i,])))
                if(num > 0){
                    indexes <- c(indexes, i)
                }
            }
            l <- as.data.frame(l)
            return(invisible(list(l[indexes,])))
        }
    }
    
    evaluate <- function(l){
        if(length(l)==0){
            return(invisible(list()))
        }else{
            df <- as.data.frame(l)
            #remove sampleID line.
            df <- df[, -1]
            numCols <- length(colnames(df))
            numRows <- length(rownames(df))
            indexes <- c()
            for(i in 1:numRows){
                # num => the number of not 'NA'.
                num <- length(grep(FALSE, is.na(as.numeric(df[i,]))))
                if(num > 0){
                    indexes <- c(indexes, i)
                }
            }
            return(invisible(as.data.frame(l))[indexes,])
        }
    }
    
    searchIndex <- function(df, strings){
        numCols <- length(colnames(df))
        numStrings <- length(strings)
        index <- c()
        for(i in 1:numCols){
            for(j in 1:numStrings){
                if(colnames(df)[i] == strings[j]){
                    index <- c(index, i)
                }
            }
        }
        return(invisible(index))
    }
    
    exgrep <- function(strings, vector){
        result <- c()
        for(i in 1:length(strings)){
            vec <- grep(strings[i], vector)
            result <- c(result, vec)
        }
        return(invisible(result))
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
            }else{
                if(strings[i] == client){
                    index <- c(index, i)
                }
            }
        }
        return(invisible(index))
    }

    splitDF <- function(DF){
        df <- as.data.frame(DF)
        #variables related to return DF's col lines.
        general <- c()
        special <- c()
        for(i in 2:length(colnames(DF))){
            colLevel <- length(levels(DF[, i]))
            #YES/NO => '2'
            if(colLevel == 2 ){
                general <- c(general, i)
            }else{
                special <- c(special, i)
            }
        }
        return(invisible(
            list(DF[,c(1,general)],
                 DF[,c(1,special)])))
               
    }
    client <- DFdata$client # cp
    #new version.
    indexes <- getIndex(as.character(client), name)
    if(length(indexes) == 0){
        return(invisible(list()))
    }
    #the first element of the return value.
    mat <- DFdata[indexes,]
    # the names of newspaper.
    # "unique function" removes repetitions.
    
    newsPaperNames <- as.character(unique(mat$name))
    #the indexes of NewsPaper names.
    # return(list(newsPaperNames,NPdata$label))
    newsPaperIndexes <- exgrep(newsPaperNames,as.vector(NPdata$label))
    #remove repetitions and sort the vector.
    subset <- NPdata[sort(unique(newsPaperIndexes)),]
    # 'colStrings' is names of the newspaper
    colStrings <- as.character(subset[,1])
    index <- searchIndex(IdDF,unique(colStrings))
    # SampleID + "NP_" col
    df <- IdDF[, c(1, index)]
    df <- splitDF(df)
    hoge <- clean(abstract(df[[1]]))
    piyo <- clean(evaluate(df[[2]]))
    return(invisible(list(mat, hoge, piyo)))
}
