library(deSolve)
library(phaseR)
options(repr.plot.width=10, repr.plot.height=5)
plot(1, type="n", xlab="", ylab="", xlim=c(0, 100), ylim=c(0, 100))
D <- 1
k11 <- 2.1
k12 <- 2 * 10^-5
k21 <- 10
k22 <- 10^-2
k31 <- 1
k32 <- 10^-2
k41 <- 4
k42 <- 2*(10^-4)

C <- 100
cells <- c(5,0,10,0,20,0,30,0,40,0,50,0,60,0,70,0)
X <- matrix(cells,nrow=8, ncol=2, byrow=TRUE)

model <- function (t,y,parms) {
    x1 <-y[1]
    x3 <-y[2]
    
    dx1 <- -(k11 - k12*x1^2 + k31 + k32*x1^2 + 0.5*D)*x1 - (k31+ k32*x1^2 + 0.5*D)*x3 + 100*(k31 + k32*x1^2+ 0.5*D)
    dx3 <- -(k21 + k22*x3^2)*x1 - (k21 + k22*x3^2 + k41 - k42*x3^2 + D)*x3   +  100*(k21 + k22*x3^2)
    list(c(dx1,dx3))
    
    
}
model.flowflowField <- flowField(model, xlim = c(0, 100), ylim = c(0, 100), points = 39, xlab = "t")
grid()
model.nullclines    <- nullclines(model, xlim = c(0, 100), ylim = c(0, 100), points = 50, xlab = "t")
model.trajectory    <- trajectory(model, y0=X, tlim=c(0,100))


#yini  <- c(x1=10,x3=60)
#times <- seq(from = 0, to = 100,by = 0.01)
#out <- ode(y = yini,times = times,func = model,parms = NULL,method = rkMethod("rk4"))
#plot(out[,"x1"],out[,"x3"],type="l",col = "red",main="I ")



