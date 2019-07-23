library("fractal")

#-------------------------------------------------------------------
## create a faux object of class chaoticInvariant
faux.data <- list(matrix(rnorm(1024), ncol=2), matrix(1:512))
chaoticInvariant(faux.data,
                 dimension = 1:2,
                 n.embed = 10,
                 n.reference = 50,
                 n.neighbor = 35,
                 tlag = 10,
                 olag = 15,
                 resolution = 2,
                 series.name = "my series",
                 series = 1:10,
                 ylab = "log2(C2)",
                 xlab = "log2(scale)",
                 metric = Inf,
                 invariant = "correlation dimension")

#-------------------------------------------------------------------
## calculate the correlation dimension estimates
## for chaotic beam data using a delay
## embedding for dimensions 1 through 10, a
## orbital lag of 10, and a spatial resolution
## of 4.
beam.d2 <- corrDim(beamchaos, olag=10, dim=10, res=4)
## print a summary of the results
print(beam.d2)
## plot the correlation summation curves
plot(beam.d2, fit=FALSE, legend=FALSE)
## plot an extended data analysis plot
eda.plot(beam.d2)

#-------------------------------------------------------------------

## Not run:
## perform a determinism test for the beamchaos
## series. in order to do so, it is vitally
## important to provide the proper orbital lag,
## which can be estimated as the lag value
## associated with the first common maxima over
## all contours in a spaceTime plot.
plot(spaceTime(beamchaos))
## we esimate an appropriate olag of 30, and
## subsequently perform the deterrminism test
beam.det <- determinism(beamchaos, olag=30)
print(beam.det)
plot(beam.det)
eda.plot(beam.det)
## perform a similar analysis for a Gaussian white
## noise realization
rnorm.det <- determinism(rnorm(1024),olag=1)
print(rnorm.det)
plot(rnorm.det)
eda.plot(rnorm.det)
## End(Not run)

#-------------------------------------------------------------------

## calculate the scaling exponent for a random
## walk realization
DFA.walk <- DFA(rnorm(1024), detrend="poly1", sum.order=1)
## print the results
print(DFA.walk)
## plot a summary of the results
eda.plot(DFA.walk)
#-------------------------------------------------------------------

## create a time-varying FD parameter, linearly
## varying from white to pink noise, then jump
## to a red noise plateau
delta <- c(seq(0, 0.5, by=0.01), rep(1,100))
## set the innovations variance to unity
innovation <- rep(1, length(delta))
## simulate a time-varying FD process
z <- FDSimulate(delta=delta, innovation=innovation)
print(z)
plot(z)

#-------------------------------------------------------------------

## create test series
set.seed(100)
x <- rnorm(1024)
walk <- cumsum(x)
diffwalk <- diff(walk)
## calculate the Hurst coefficient of a random
## walk series using various techniques
methods <- c("aggabs","aggvar","diffvar","higuchi")
z <- list(
  "aggabs" = hurstBlock(walk, method = "aggabs"),
  "aggvar" = hurstBlock(walk, method = "aggvar"),
  "diffvar" = hurstBlock(walk, method = "diffvar"),
  "higuchi" = hurstBlock(diffwalk, method = "higuchi"))
## plot results
old.plt <- splitplot(2,2,1)
for (i in 1:4){
  if (i > 1)
    splitplot(2,2,i)
  plot(z[[i]], key=FALSE)
  mtext(paste(attr(z[[i]],"stat.name"), round(as.numeric(z[[i]]),3), sep=", H="),
        line=0.5, adj=1)
}
par(old.plt)
#-------------------------------------------------------------------

old.plt <- par("plt")
models <- c("ppl","fdp","fgn","dfbm")
for (i in seq(along=models)){
  splitplot(2,2,i)
  plot(lmSDF(lmModel(models[i])),
       reference.grid=FALSE, log.axes="xy")
}
par(plt=old.plt)
#-------------------------------------------------------------------
