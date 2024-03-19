Quantile_sample <- function(quant_list,
                            n_items,
                            n_draws = 6000,
                            weights)
{

   item_list <- list()

   for(i in 1:n_items)
   {
      sampler <- function(df,ws)
      {
         # select 1 item at a time
         reduced_df <- df[seq(i, nrow(df)-(20-i), by = n_items),]
         # sample from item draw of specific model base on corresponding weight
         reduced_df[floor(runif((n_draws*ws),1,n_draws)),]
      }

      # NOTE: models and weights need to be entered in the same order
      # and have the same length (i.e, as many weights as models)

      item_list[[i]] <- purrr::list_rbind(mapply(sampler,
                                                 quant_list,
                                                 ws = weights,
                                                 SIMPLIFY = FALSE))
   }
   return(item_list)
}

