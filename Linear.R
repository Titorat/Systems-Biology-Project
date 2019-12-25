library(deSolve)
library(phaseR)
options(repr.plot.width=10, repr.plot.height=5)
plot(1, type="n", xlab="", ylab="", xlim=c(0, 100), ylim=c(0, 100))
D <- 1
k1 <- 0.012*D
k2 <- 99*D
k3 <- 0.11*D
k4 <- 0.22*D

C <- 100
cells <- c(5,0,10,0,20,0,30,0,40,0,50,0,60,0,70,0)
X <- matrix(cells,nrow=8, ncol=2, byrow=TRUE)

model <- function (t,y,parms) {
    x1 <-y[1]
    x3 <-y[2]
    
    dx1 <- -(k1 + k3 + 0.5*D)*x1 - (k3 + 0.5*D)*x3 + C*(k3 + 0.5*D)
    dx3 <- -k2*x1 - (k2+k4+D)*x3+C*k2
    list(c(dx1,dx3))
    
    
}
model.flowflowField <- flowField(model, xlim = c(0, 100), ylim = c(0, 100), points = 39, xlab = "t")
grid()
model.nullclines    <- nullclines(model, xlim = c(0, 100), ylim = c(0, 100), points = 50, xlab = "t")
model.trajectory    <- trajectory(model, y0=X, tlim=c(0,100))

