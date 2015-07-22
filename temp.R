pp <- function (n,r=4) {
  x <-  seq(0, 100000000, len=n)
  y <- seq(0, 100000000, len=n)
  df <- expand.grid(x=x, y=y)
  df$z <- NumUnits(1000000000,x+y,100000000)
  df
}
p <- ggplot(pp(20), aes(x=x,y=y))

p <- p + geom_tile(aes(fill=z))
p
