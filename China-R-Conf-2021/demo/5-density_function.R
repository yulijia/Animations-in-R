# r animation package tutorial
# How To Make Animated Graphs using R
# Example of animated transitions in statistical data graphics


# This r animation plots the density functions of 150 draws of 100 values 
# from a normally distributed random variable. To make things a little more 
# interesting (i.e., make the distribution move), a constant that varies based on 
# the iteration count is added to the 100 values. The chart also includes 
# a slightly stylized frame tracker (or draw counter) along the top of the chart and 
# a horizontal bar that notes the current position and previous two positions of 
# the sample mean. Finally, the foreground color of the chart changes based on 
# the mean of the distribution.



library(animation)

#Set delay between frames when replaying
ani.options(interval=.05)

# Set up a vector of colors for use below
col.range <- heat.colors(15)

# Begin animation loop
# Note the brackets within the parentheses
saveGIF({
  
  # For the most part, it’s safest to start with graphical settings in
  # the animation loop, as the loop adds a layer of complexity to
  # manipulating the graphs. For example, the layout specification needs to
  # be within animation loop to work properly.
  layout(matrix(c(1, rep(2, 5)), 6, 1))
  
  # Adjust the margins a little
  par(mar=c(4,4,2,1) + 0.1)
  
  # Begin the loop that creates the 150 individual graphs
  for (i in 1:150) {
    
    # Pull 100 observations from a normal distribution
    # and add a constant based on the iteration to move the distribution
    chunk <- rnorm(100)+sqrt(abs((i)-51))
    
    # Reset the color of the top chart every time (so that it doesn’t change as the
    # bottom chart changes)
    par(fg=1)
    
    # Set up the top chart that keeps track of the current frame/iteration
    # Dress it up a little just for fun
    plot(-5, xlim = c(1,150), ylim = c(0, .3), axes = F, xlab = "", ylab =  "", main = "Iteration")
    abline(v=i, lwd=5, col = rgb(0, 0, 255, 255, maxColorValue=255))
    abline(v=i-1, lwd=5, col = rgb(0, 0, 255, 50, maxColorValue=255))
    abline(v=i-2, lwd=5, col = rgb(0, 0, 255, 25, maxColorValue=255))
    
    # Bring back the X axis
    axis(1)
    
    # Set the color of the bottom chart based on the distance of the distribution’s mean from 0
    par(fg = col.range[mean(chunk)+3])
    
    # Set up the bottom chart
    
    plot(density(chunk), main = "", xlab = "X Value", xlim = c(-5, 15), ylim = c(0, .6))
    
    # Add a line that indicates the mean of the distribution. Add additional lines to track
    # previous means
    abline(v=mean(chunk), col = rgb(255, 0, 0, 255, maxColorValue=255))
    if (exists("lastmean")) {abline(v=lastmean, col = rgb(255, 0, 0, 50, maxColorValue=255)); prevlastmean <- lastmean;}
    if (exists("prevlastmean")) {abline(v=prevlastmean, col = rgb(255, 0, 0, 25, maxColorValue=255))}
    #Fix last mean calculation
    lastmean <- mean(chunk)
  }
})