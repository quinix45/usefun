index_reorder <- function(original, order){
   
   reordered <- matrix(nrow = nrow(original), 
                       ncol = ncol(original))
   
   for (i in 1:nrow(original))
   {
      for (j in 1:ncol(original))
      {
         reordered[i,j] <- original[i,which(order[i,] == j)]
      }
   }
   
   return(reordered)
   
}

