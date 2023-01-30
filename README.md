# The R package ETH.OLP

This is the repository for the R package "ETH.OLP" which was developed for the course "Optimierung Landwirtschaflicher Produktionssysteme".
The package containing various functions for linear programming and positive mathematical programming.

*Please make sure to use this package with R Version 4.1 or higher.*

## Defining a problem and setting up an `LP` object

To set up a linear programming problem, only three function arguments are needed: a objective function coefficient vector `c`, the technical coefficients `A`as well as the resource endowment `b`.

```{r}
c <- c(Wheat = 1000, Potato = 2000, Rapeseed = 1200, Soybean = 1100, Corn = 900, Vegetables = 2800)
A <- matrix(c(1,40,120,1,0,0,
              1,120,80,0,1,0,
              1,50,150,0,0,0,
              1,50,-20,0,0,1,
              1,30,120,0,0,0,
              1,200,180,0,0,1),
              ncol = 6)
b <- c(Area = 20, Labour = 1800, Nitrogen = 2000, Fusarium = 12, Phytophthora = 13, Fly = 8)
LP <- LP(c, A, b)
```

## Basic functions

We can print out information about the `LP`-object as follows:
```{r}
print(LP)
```

To get a qick insight, we can also plot two variables with their restrictions against each other.
```{r}
plot(LP) # plot model, if more than 2 variables, plot the first two
plot(LP, which = c(2,4)) # look at some other variables
```


## Sensitivity and shadow values

```{r}
sensitivity(LP)
plot(sensitivity(LP))
duals(LP)
plot(duals(LP))
```

## Sensitivity Analysis

```{r}
library(sensitivity)
simdata <- simulate(LP, r = 0.2, n = 500)
plot(simdata) # Press `ENTER` until it ends. Look at the simulated data before doing a sensitivity analysis. Adjust `r` if it looks non-linear.
SA <- src(X = simdata$Input, y = simdata$Output$Objective, nboot = 200)
print(SA)
plot(SA) # this is the generic function contained within the "sensitivity" package
ggplot2::ggplot(SA) # this as well
barplot(SA) # this function is exported by `ETH.OLP` -- information in title is adjusted automatically
```

## Positive Mathematical Programming

```{r}
xstar <- c(Wheat = 8,
           Potato = 2,
           Rapeseed = 2,
           Soybean = 3,
           Corn = 4,
           Vegetables = 1) # read in values observed in "reality"
PMP <- PMP(c, A, b, xstar)
Q <- PMP$Q
print(PMP) # we are notified that one constraint was violated, and which it is. Also, PMP solution derives from observed one, because it needs to be within feasibility set!
new_c <- c(Wheat = 1000, Potato = 2000, Rapeseed = 1200, Soybean = 1100, Corn = 900, Vegetables = 3500)
predict(PMP, new_c) # what happens to production if we increase vegetable prices?
```

## Looping with `sapply`

Define a function which runs the LP for a specific given `Labour`:
```{r}
f <- function(Labour){
   b <- c(Area = 20, Labour = Labour, Nitrogen = 2000, Fusarium = 12, Phytophthora = 13, Fly = 8) # `Labour` is variable
   return(update(LP, b = b)$solution)
}
LabourVector <- seq(500,3500,50) # define some values which we want to evaluate
results <- sapply(LabourVector, f)
barplot(results)
barplot(results, col = viridis::cividis(7), space = 0, legend = TRUE, args.legend = list(x = "bottomright")) # could also be nicer, but works like this...
```

## Looping over two dimensions

```{r}
n <- 200
AreaVector <- seq(5, 55, length=n)
LabourVector <- seq(1000,3000,length=n)
```
Define a function to return the `LP` results based on available `Area` and `Labour`.
```{r}
f <- function(Area, Labour){
   b <- c(Area = Area, Labour = Labour, Nitrogen = 2000, Fusarium = 12, Phytophthora = 13, Fly = 8) # `Labour` is variable
   return(update(LP, b = b)$solution/Area)
}
```
Compute the results for all possible combinations of `AreaVector` and `LabourVector`, then save results.
```{r}
expand.grid("Area" = AreaVector, "Labour" = LabourVector) |> # create a dataframe with all combinations of AreaVector and LabourVector
   apply(MARGIN = 1, FUN = function(x) f(x[1],x[2])) |>
   t() -> results
results <- cbind(results, Total = apply(results, 1, sum))
```
Visualise the computed results results (one at a time, otherwise it gets unhandy).
```{r}
for (variable in colnames(results)) {
   filled.contour(AreaVector, LabourVector, round(matrix(results[,variable], ncol = n),1),
                  zlim = c(0,1),
                  nlevels = 9, col = viridis::cividis(10),
                  xlab = "Land", ylab = "Labour",
                  main = paste("Response of", variable, "to parameters"))
}
```



