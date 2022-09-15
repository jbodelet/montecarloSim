parMontecarlo <- function( mcRun, rep_index, paramList, sourceFile = NULL, packages = NULL, ncpus = 10 ){
  # set ckzster:
  cl <- makeCluster(ncpus)
  on.exit( stopCluster(cl) )
  # Export arguments:
  packages <- c( packages, "dplyr" )
  clusterExport(cl,c("mcRun", "packages", "sourceFile"), envir = environment() )
  # Source files:
  clusterEvalQ(cl, source(sourceFile) )
  # Export required packages:
  clusterEvalQ( cl, lapply(packages, require, character.only = TRUE) )
  # whole list of parameters with rep number:
  wholeParamList <- do.call( tidyr::crossing, c( paramList, list( rep = rep_index ) ) )
  wholeParamList <- tibble::rowid_to_column( wholeParamList, "ID")
  mcRun_wrap <- function(i, wholeParamList ) {
    params <- dplyr::slice( dplyr::select( wholeParamList, -ID, -rep), i )
    do.call( mcRun, params )
  }
  # parallel replicate:
  totalNbSim <- nrow(wholeParamList)
  rawResults <- parLapply( cl, 1:totalNbSim, mcRun_wrap, wholeParamList = wholeParamList )
  return( list( rawResults = rawResults, wholeParamList = wholeParamList) )
}

getMontecarloTable <- function( mc, groupBy ){
  mcSize <- max( mc %>% select(rep) )
  mc %>% mutate_if( is.double, funs(round(., 3) ) ) %>%
    group_by( across( groupBy ) ) %>% select(-ID, -rep) %>%
    summarise( across(everything(), list(median = ~ median(.), sd = ~ sprintf("(%.3f)", mad(.) ) ) ) )  %>% ungroup
}




# # Post Montecarlo simulations:
# if(!is.null(mcPostRun) ){
#   out <- t( sapply( 1:totalNbSim, function(i) mcPostRun( mcSim[[i]], wholeParamList[i, ] ) ) )
#   return( cbind( wholeParamList, out ) )
# }else{
#   return( list( wholeParamList = wholeParamList, mc = mcSim ) )
# }



# 
# 
# montecarloTest <- function(mcRun, rep_index, paramList, mcPostRun = NULL ){
#   wholeParamList <- do.call( tidyr::crossing, c( paramList, list( rep = rep_index ) ) )
#   mcRun_wrap <- function(i, wholeParamList ) {
#     params <- dplyr::slice( dplyr::select( wholeParamList, -rep), i )
#     do.call( mcRun, params )
#   }
#   # parallel replicate:
#   totalNbSim <- nrow(wholeParamList)
#   out <- lapply( 1:totalNbSim, mcRun_wrap, wholeParamList = wholeParamList )
#   # Post Montecarlo simulations:
#   if(!is.null(mcPostRun) ){
#     out2 <- t( sapply( 1:totalNbSim, function(i) mcPostRun(out[[i]], wholeParamList[i, ] ) ) )    
#   }
#   return( cbind( wholeParamList, out2 ) )
# }
# 



# # Simulations:
# mc <- lapply( 1:(mcSize/20), function(i){
#   rep_ind <- 1:20 + 20 * (i-1)
#   parMontecarlo( mcRun, rep_ind, paramList = paramList, mcPostRun = mcPostRun, packages = packages, ncpus = 21 )
# })
# 
# mcResults <- do.call(rbind, mc)




