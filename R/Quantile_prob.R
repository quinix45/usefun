Quantile_prob <- function(parameter_draws,
                          n_items,
                          n_person,
                          quantiles = 0.1,
                          method = "empirical",
                          n_draws = 6000,
                          irf_function = "4PL")
{

   # Define the number of MCMC draws
   n_draws <- nrow(parameter_draws)

   quants <- c(n_person*seq(0, 1, by = quantiles))
   quants[1] <- 1

   if(method == "empirical"){

      # create theta quantiles and repeat each row per each item to vectorize probability calculation

      draws_theta <- apply(t(parameter_draws[, c(grepl("r_id", colnames(parameter_draws)))]),
                           2,
                           sort,
                           decreasing=FALSE)

      quant <- t(draws_theta[quants,])

      # dimension of quantile matrix is a [n_items*n_draws, n_quantiles]
      quant_rep <- data.frame(quant) %>%
         dplyr::slice(rep(1:n(), each = n_items))
   } else if (method == "theoretical"){

      theoretical_quantiles <- data.frame(t(qnorm(c(.01, seq(.1, .9, by = .1), .99))))

      quant_rep <- theoretical_quantiles %>%
         dplyr::slice(rep(1:n(), each = n_items*n_draws))

   }

   # sort item parameters correctly
   unsorted_draws_it <- parameter_draws[, c(grepl("r_item", colnames(parameter_draws)))]
   draws_it <- unsorted_draws_it[,gtools::mixedsort(colnames(unsorted_draws_it))]

   # Get pooled intercepts of item parameters
   unsorted_post_intercepts <- parameter_draws[, c(grepl("b_", substr(colnames(parameter_draws), 1, 2)))]
   post_intercepts <- unsorted_post_intercepts[, gtools::mixedsort(colnames(unsorted_post_intercepts))]


   if(irf_function == "4PL")
   {

      irf_4PL <- function(theta = 0, a = 1, b = 0, c = 0, d = 1)
      {
         (c + (d - c) * (exp(a * (theta - b))/(1 + exp(a * (theta - b)))))
      }

      if(ncol(draws_it) == n_items)
      {
         ####### 1 PL #######
         a <- rep(exp(post_intercepts[,1]), each = n_items)
         b <- c(unlist(t(draws_it + post_intercepts[,2])))
         c <- rep(0, n_draws*n_items)
         d <- rep(1, n_draws*n_items)

         return(irf_4PL(quant_rep, a, b, c, d))
      }
      else if(ncol(draws_it) == 2*n_items)
      {
         ####### 2 PL #######
         a <- exp(c(unlist(t(draws_it[,1:n_items] + post_intercepts[,1]))))
         b <- c(unlist(t(draws_it[,(1 + n_items):(n_items*2)] + post_intercepts[,2])))
         c <- rep(0, n_draws*n_items)
         d <- rep(1, n_draws*n_items)

         return(irf_4PL(quant_rep, a, b, c, d))
      }
      else if(ncol(draws_it) == 3*n_items)
      {
         ####### 3 PL #######
         a <- exp(c(unlist(t(draws_it[,1:n_items] + post_intercepts[,1]))))
         b <- c(unlist(t(draws_it[,(1 + n_items):(n_items*2)] + post_intercepts[,2])))
         c <- brms::inv_logit_scaled(c(unlist(t(draws_it[,(1 + n_items*2):(n_items*3)] + post_intercepts[,3]))))
         d <- rep(1, n_draws*n_items)

         return(irf_4PL(quant_rep, a, b, c, d))
      }
      else
      {
         ####### 4 PL #######
         a <- exp(c(unlist(t(draws_it[,1:n_items] + post_intercepts[,1]))))
         b <- c(unlist(t(draws_it[,(1 + n_items):(n_items*2)] + post_intercepts[,2])))
         c <- brms::inv_logit_scaled(c(unlist(t(draws_it[,(1 + n_items*2):(n_items*3)] + post_intercepts[,3]))))
         d <- brms::inv_logit_scaled(c(unlist(t(draws_it[,(1 + n_items*3):(n_items*4)] + post_intercepts[,4]))))

         return(irf_4PL(quant_rep, a, b, c, d))
      }

   }
   else if(irf_function == "2CLL")
   {

      irf_2CLL <- function(theta = 0, a = 1, b = 0)
      {
         1 - exp( -exp( a * (theta - b)))
      }

      if(ncol(draws_it) == n_items)
      {
         ####### 1 CLL #######
         a <- rep(exp(post_intercepts[,1]), each = n_items)
         b <- c(unlist(t(draws_it + post_intercepts[,2])))


         return(irf_2CLL(quant_rep, a, b))
      }
      else if(ncol(draws_it) == 2*n_items)
      {
         ####### 2 CLL #######
         a <- exp(c(unlist(t(draws_it[,1:n_items] + post_intercepts[,1]))))
         b <- c(unlist(t(draws_it[,(1 + n_items):(n_items*2)] + post_intercepts[,2])))

         return(irf_2CLL(quant_rep, a, b))
      }
   }
   else if(irf_function == "2NLL"){

      irf_2NLL <- function (theta = 0, a = 1, b = 0)
      {
         exp( -exp( -a * (theta - b)))
      }

      if(ncol(draws_it) == n_items)
      {
         ####### 1 NLL #######
         a <- rep(exp(post_intercepts[,1]), each = n_items)
         b <- c(unlist(t(draws_it + post_intercepts[,2])))

         return(irf_2NLL(quant_rep, a, b))
      }
      else if(ncol(draws_it) == 2*n_items)
      {
         ####### 2 NLL #######
         a <- exp(c(unlist(t(draws_it[,1:n_items] + post_intercepts[,1]))))
         b <- c(unlist(t(draws_it[,(1 + n_items):(n_items*2)] + post_intercepts[,2])))

         return(irf_2NLL(quant_rep, a, b))
      }

   }
   else if(irf_function == "LPE"){

      irf_LPE <- function(theta, a, b, acc)
      {
         (exp(a * (theta - b))/(1 + exp(a * (theta - b))))^acc
      }

      a <- exp(c(unlist(t(draws_it[,1:n_items] + post_intercepts[,1]))))
      b <- c(unlist(t(draws_it[,(1 + n_items):(n_items*2)] + post_intercepts[,2])))
      acc <- exp(c(unlist(t(draws_it[,(1 + n_items*2):(n_items*3)] + post_intercepts[,3]))))

      return(irf_LPE(quant_rep, a, b, acc))
   }

}
