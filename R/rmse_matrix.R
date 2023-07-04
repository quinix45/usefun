rmse_matrix <- function(dat,
                        scale = FALSE,
                        labels = FALSE){

   # number of variables
   vars <- ncol(dat)

   if (scale == TRUE){
      dat <- scale(dat)}

   # K*(K-1)/2 (unique elements in matrix)
   # get all unique combinations
   mat_values <- combn(c(1:vars), 2)

   # create matrix
   mat <- matrix(rep(0, vars^2),
                 ncol = vars,
                 nrow = vars)




   for(i in 1:ncol(mat_values)){
      mat[mat_values[2,i], mat_values[1,i]] <- sqrt(mean((dat[,mat_values[2,i]] - dat[,mat_values[1,i]])^2, na.rm = TRUE))
   }


   if (labels == TRUE){
      colnames(mat) <- colnames(dat)
      rownames(mat) <- colnames(dat)
      }

   return(mat)

}
