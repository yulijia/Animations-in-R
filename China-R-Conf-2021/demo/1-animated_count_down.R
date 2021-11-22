png(file="example%02d.png", width=100, height=100)
par(bg="grey",mar=c(1,1,1,1))
# par(mar=c(1,1,1,1))
for (i in c(5:1, "Go!")){
  plot.new()
  text(0.5,0.5,i, cex = 3)
}
dev.off()

# Use image magick
system("convert -delay 80 *.png animated_count_down.gif")

# Remove png files
file.remove(list.files(pattern=".png"))








png(file="example%02d.png", width=100, height=100)
par(bg="grey",mar=c(1,1,1,1))
# par(mar=c(1,1,1,1))
for (i in c(1:10)){
  plot.new()
  text(0.5,0.5,"R", cex =i,col="Blue")
}
dev.off()

# Use image magick
system("convert -delay 10 *.png animated_R.gif")

# Remove png files
file.remove(list.files(pattern=".png"))