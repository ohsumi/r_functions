composition <- function(x, y, backpatchDFlists)
{

    backPatchProcess <- function(id, colname, DFlists){
        number <- length(DFlists)
        for(i in 1:number){
            targetDF <- as.data.frame(DFlists[[i]])
            #get the colname index.
            if(is.na(colIndex <- match(colname,colnames(targetDF))) == FALSE){
                #get the ID index.
                rowIndex <- match(id, as.vector(targetDF[,1]))
                #return the value.
                return(invisible(factor(targetDF[rowIndex, colIndex])))
            }else{
                if(i == number){
                    #back patch by "NA"
                    return(NA)
                }
            }
        }
    }

    removeDummy <- function(df,lists)
    {
        colNum <- length(colnames(df))
        rowNum <- length(rownames(df))
        df <- as.matrix(df)
        for(m in 2:colNum){    
            str <- colnames(df)[m]
            for(n in 1:rowNum){
                id <- as.numeric(df[n, 1])
                val <- backPatchProcess(id, str, lists)
                val <- as.character(val)
                df[n, m] <- val
            }
        }
        return(invisible(as.data.frame(df)))
    }

    remake <- function(df1, df2){
        #col strings
        df1 <- as.data.frame(df1)
        df2 <- as.data.frame(df2)        
        cs <- unique(c(colnames(df1), colnames(df2)))
        #merge sampleIDs
        id1 <- as.numeric(as.character(df1$SampleID))
        id2 <- as.numeric(as.character(df2$SampleID))
        SampleID <- sort(unique(c(id1, id2)))
        # make the return value.
        csNum <- length(cs)
        SampleIDNum <- length(SampleID)
        m <- matrix("dummy", nrow = SampleIDNum, ncol = csNum)
        # insert SampleID and unique col strings.
        m[, 1] <- SampleID
        colnames(m) <- cs
        # return the value.
        return(invisible(as.data.frame(m)))
    }
    
    #get dummy dataframe.    
    m <- remake(x, y)
    #the main backpatch processing.
    m <- removeDummy(m, backpatchDFlists)
    #return the merged result.
    return(invisible(m))
}
