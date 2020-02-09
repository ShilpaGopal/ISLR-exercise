par(xpd=NA)
plot(NA, NA , type = "n", xlim = c(-2,2), ylim=c(-3,3), xlab = "X1", ylab = "X2")
#X2<1
lines(x=c(-2,2),y=c(1,1))
text(x=(-2+1)/2, y=-1, labels = c(-1.8))
text(x = 1.5, y = -1, labels = c(0.63))
#X1<1
lines(x=c(1,1), y=c(-3,1))
text(x = 0, y = 2.5, labels = c(2.49))
#X2<2
lines(x=c(-2,2), y=c(2,2))
#X1<0
lines(x=c(0,0), y=c(1,2))
text(x = -1, y = 1.5, labels = c(-1.06))
text(x = 1, y = 1.5, labels = c(0.21))
