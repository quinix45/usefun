genPattern_Stukel <- function (th, pars, seed = NULL)

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
   # given how the function is vectorized, alpha1 and 2 cannot be exactly 0, otherwise NAs will be produced

   irf_stukel <- function (theta, a = 1, b = 0, alpha1 = 0.00001, alpha2 = 0.00001)
   {

      # Using the fact that TRUE and FALSE are 1 and 0 respectively to vectorize pice-wise function

      h <- ((theta - b) > 0)*
         (((alpha1 > 0) * alpha1^(-1) * (exp(alpha1 * a * (theta - b)) - 1)) +
             ((alpha1 == 0) * a * (theta - b)) +
             (alpha1 < 0) * -alpha1^(-1) * log(abs(1 - alpha1 * a * (theta - b)))) +

         ((theta - b) <= 0)*
         (((alpha2 > 0) * -alpha2^(-1) * (exp(alpha2 * a * ((b - theta))) - 1)) +
             ((alpha2 == 0) * a * (theta - b)) +
             (alpha2 < 0) * alpha2^(-1) * log(abs(1 - alpha2 * a * ((b - theta)))))

      # Make sure that h does not become too large for R to handle

      h <- ifelse(h > 601.7777, 601.7777, h)
      h <- ifelse(h < -601.7777, -601.7777, h)

      return(exp(h)/(1 + exp(h)))
   }


   # Vectorize probability calculations

   pp <- irf_stukel(th,
                    rep(pars[,1], each = sample_size),
                    rep(pars[,2], each = sample_size),
                    rep(pars[,3], each = sample_size),
                    rep(pars[,4], each = sample_size))

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


