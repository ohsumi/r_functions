IntegratedList <-
function(client,SampleIDs,DFdataDFs){
    
    cat("start to get subsets related to ..\n")
    
    cat(" -> Magazine. \n")
    l1 <- abstractMagazineDF (client,SampleIDs[[1]],DFdataDFs[[1]])
    cat(" -> TV. \n")
    l2 <- abstractTVDF       (client,SampleIDs[[2]],DFdataDFs[[2]])
    cat(" -> NewsPaper. \n")
    l3 <- abstractNewsPaperDF(client,SampleIDs[[3]],DFdataDFs[[3]],DFdataDFs[[4]])
    cat(" -> Web.  \n")
    l4 <- abstractWebDF      (client,SampleIDs[[4]],DFdataDFs[[5]])
    cat("* == * the process successfully ended! * == * \n")
    # return IntegraedList class
    return(invisible(
        new("IntegratedList",
            name = client,
            Magazine    = l1,
            TV          = l2,
            NewsPaper   = l3,
            WEB         = l4
            )))
}
