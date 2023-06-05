dlnp <-
  function(x,y ,log = FALSE) {
    val <-
      dnorm( x, log = TRUE ) +
      dpois( y, exp( x ), log = TRUE)
    if (log) val else exp( val )
  }
curve(dlnp(x,2),-5,5)
grid()

# Define the mode of dlnp using optimization
mode <- optimize(function(x) -dlnp(x, 2), interval = c(-5, 5))$minimum # 0.443
# Calculate the relative height
rela_h <- dlnp(mode, 2) / dcauchy(mode, location = mode, scale = 1) # 0.290

# Add a plot of the envelope
curve(dcauchy(x, location = mode, scale = 1) * rela_h, add = TRUE, col = "red")
