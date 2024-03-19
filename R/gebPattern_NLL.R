genPattern_NLL <- function (th, pars, seed = NULL)

{
   sample_size <- length(th)

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

   irf_NLL <- function(theta, a, b)
   {
      exp( -exp( -a*(theta - b)))
   }

   # Vectorize probability calculations

   pp <- irf_NLL(th,
                 rep(pars[,1], each = sample_size),
                 rep(pars[,2], each = sample_size))

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
