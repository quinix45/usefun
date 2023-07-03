irf_stukel.2PL <- function(theta, a = 1, b = 0, alpha1 = 0, alpha2 = 0)
{
   if ((theta - b)>0)
   {
      if (alpha1>0)
      {
         h <- alpha1^(-1)*(exp(alpha1*a*(theta - b))-1)
      }
      else if (alpha1 == 0)
      {
         h <- a*(theta - b)
      }
      else
      {
         h <- -alpha1^-1*log(1-alpha1*a*(theta - b))
      }
   }
   else
   {
      if (alpha2 > 0)
      {
         h <- -alpha2^-1*(exp(alpha2*a*((b - theta)))-1)
      }
      else if (alpha2 == 0)
      {
         h <- a*(theta - b)
      }
      else
      {
         h <- alpha2^-1*log(1-alpha2*a*((b - theta)))
      }
   }


   # make sure that exponent does not result in a number that is too large for R to handle when probability is very extreme.

   if(h > 601.7777)
      {
      h = 601.7777
      }
   if(h < - 601.7777)
   {
      h = - 601.7777
   }

   (exp(h))/(1 + exp(h))
}
