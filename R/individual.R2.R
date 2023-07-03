#, individual r2

individual.R2 <- function(x)
{
   anovadf <- anova(x)
   for(i in 1:nrow(anovadf))
   {
      Predictors <- c(rownames(anovadf))
      SStotdf <- sum(anovadf$`Sum Sq`)
      R2 <- c(anovadf$`Sum Sq`/SStotdf)
   }
   return(data.frame(Predictors = Predictors, R2 = R2 ))
}
