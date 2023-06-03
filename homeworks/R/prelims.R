dlnp <-
  function(x,y ,log = FALSE) {
    val <-
      dnorm( x, log = TRUE ) +
      dpois( y, exp( x ), log = TRUE)
    if (log) val else exp( val )
  }
curve(dlnp(x,2),-5,5)
grid()

# Define the maximum height point x of dlnp using optimization
x_max <- optimize(function(x) -dlnp(x, 2), interval = c(-5, 5))$minimum # 0.443
# Calculate the relative height
rela_h <- dlnp(x_max, 2) / dcauchy(x_max, location = x_max, scale = 1) # 0.290

# Add a plot of the envelope
curve(dcauchy(x, location = x_max, scale = 1) * rela_h, add = TRUE, col = "red")
