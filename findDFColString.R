
findDFColString <- function(df, targetCol, companyName){
    
    lines <- colnames(as.data.frame(df))
    result <- match(targetCol, lines)
    if(is.na(result) == TRUE){
        stop("failed to find your target line")
    }
    index <- c()
    for(i in 1:length(rownames(df))){
        buff <- grep(companyName, as.character(df[i, result]))
        if(length(buff) != 0){
            index <- c(index, i)
        }
    }
    if(length(index) == 0){
        return(c())
    }else{
        return(as.character(df[index,result]))
    }
}
