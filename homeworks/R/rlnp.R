# Define the dlnp function
dlnp <- function(x,y ,log = FALSE) {
    val <-
      dnorm( x, log = TRUE ) +
      dpois( y, exp( x ), log = TRUE)
    if (log) val else exp( val )
  }


rlnp <- function(n, y, scale) {

  # Generate samples from a Cauchy distribution
  x <- rcauchy(n)
  # Generate samples from a uniform distribution
  yunif <- runif(n)
  
  # Calculate the ratio of dlnp to the envelope
  dratio <- dlnp(x, y) / (dcauchy(x, 1) * exp(y * x))
  
  # Accept or reject values of x based on yunif and dratio
  keep <- yunif <= dratio

  # Calculate M (relative height of the envelope)
  M <- max(dlnp(x[keep],y) / dcauchy(x[keep], 1) * exp(y * x[keep]))
  # Calculate x0 (mode)
  x0 <- median(x[keep])
  
  # Create and return the list of components
  list(x = x, yunif = yunif, dratio = dratio, keep = keep, x0 = x0, M = M, y = y, scale = scale)
}


rlnpPlot <- function(results) {
  x <- seq(-10, 10, length.out = 1000)
  
  # Plot a curve for dlnp
  plot(x, dlnp(x, results$y), type = "l", lwd = 2, 
       xlab = "x", ylab = "Density",
       main = "Rejection Sampling form dlnp")
  
  # Plot a curve for dlnp envelope function
  lines(x, results$M * dcauchy(x, 1) * exp(results$y * (x - results$x0)), 
        col = "orange", lwd = 2)
  
  # a scatterplot with x and yunif * yscale(x) and colored by rejection
  points(results$x[results$keep], results$dratio[results$keep], col = "blue")
  points(results$x[!results$keep], results$dratio[!results$keep], col = "red")

  legend("topright", legend = c("Accepted", "Rejected"), 
         col = c("blue", "red"), pch = 1)
  
  # Add label for y, scale, and fraction of samples rejected
  rejection_fraction <- round(1 - mean(results$keep), 2)
  label <- paste("y =", results$y, ", scale =", results$scale, 
                 ", Rejection Fraction =", rejection_fraction)
  mtext(label, side = 1, line = 4)
}

# Sample 
results <- rlnp(n = 1000, y = 0, scale = 1)
rlnpPlot(results)

