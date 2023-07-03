irf_4PL <- function(theta, a = 1, b = 0, c = 0, d = 1)
   {
      (c + (d-c)*(exp(a * (theta - b))/ (1 + exp(a * (theta - b)))))
   }

