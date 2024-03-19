genPattern_2MPL <- function (th, pars, seed = NULL){

   sample_size <- nrow(th)

   # 1st part of if statement to produce results for single item

   if(class(nrow(pars)) == "NULL" )
   {
      items_n <- 1
      pars <- t(pars)
   }
   else
   {
      items_n <- nrow(pars)
   }

   # define IRT model to generate data from

   irf_2MPL <- function(theta, a, d)
   {
      return(t(1/(1 + exp( - (t(theta%*%t(a)) + d)))))
   }

   pp <- as.vector(irf_2MPL(th,
                            pars[,1:2],
                            pars[,3]))


   # Put probabilities through `rbinom()` to generate responses

   set.seed(seed)

   response_vec <-  rbinom(n = length(pp),
                           size = 1,
                           prob = pp)

   # reshape vector to get a matrix of n_sample BY n_items

   response_mat <- matrix(response_vec,
                          ncol = items_n,
                          byrow = FALSE)

   return(data.frame(response_mat))

}
