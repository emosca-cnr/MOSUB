## ---- fig.show='hold', eval=FALSE----------------------------------------
#  install.packages("/your/dir/`mosub`_1.0.tar.gz")

## ---- eval = FALSE-------------------------------------------------------
#  library(mosub)

## ---- eval=FALSE---------------------------------------------------------
#  library(help=mosub)

## ---- eval=FALSE---------------------------------------------------------
#  help(mosub_find)
#  ?mosub_find

## ------------------------------------------------------------------------
library(mosub)
data(mosub_data)
class(interactions)
head(interactions)
dim(interactions)

## ------------------------------------------------------------------------
eval.obj.ee

## ------------------------------------------------------------------------
head(ge1)
head(ge2)

## ---- eval = FALSE-------------------------------------------------------
#  res.10 <- mosub_find(gen=c(1, 5, 10), pop=100, M=2,
#      interactions=interactions, eval_obj=eval.obj.ee,
#      eval_obj.p=list(exprs1=abs(ge1), exprs2=abs(ge2)))

## ------------------------------------------------------------------------
names(res.10)

## ---- fig=TRUE, echo=TRUE------------------------------------------------
plot(res.500$objectives, pch=16, xlab='f1', ylab='f2')
points(res.500$objectives[res.500$rank==1, ], pch=16, col='red')

## ---- fig=TRUE, echo=TRUE------------------------------------------------
sim <- mn.sim(res.500$individuals)
plot(hclust(as.dist(1-sim), method='ward'))

## ---- fig=TRUE, echo=TRUE------------------------------------------------
N10 <- length(which(res.10$rank==1))
N100 <- length(which(res.100$rank==1))
N500 <- length(which(res.500$rank==1))
fronts <- rbind(res.10$objectives[res.10$rank==1, ],
    res.100$objectives[res.500$rank==1, ],
    res.500$objectives[res.500$rank==1, ])
fronts.norm <- pareto.normalize(fronts)
fronts.norm.hi.10 <- pareto.hypervolume(fronts.norm[1:N10, ])
fronts.norm.hi.100 <- pareto.hypervolume(
    fronts.norm[(N10+1):(N10+N100), ])
fronts.norm.hi.500 <- pareto.hypervolume(
    fronts.norm[-(1:(N10+N100)), ])
print(fronts.norm.hi.10)
print(fronts.norm.hi.100)
print(fronts.norm.hi.500)
plot(fronts.norm, pch=16)

## ---- eval = FALSE-------------------------------------------------------
#  res.500 <- population.postProcess(res.500,
#      interactions=interactions)

## ------------------------------------------------------------------------
names(res.500)
res.500$igraphs[[1]]

## ---- fig=TRUE, echo=TRUE, eval = TRUE-----------------------------------
library(igraph)
plot.igraph(res.500$igraph[[1]], layout=layout.lgl)

