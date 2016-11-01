library (tidyverse)
library(apaTables)
library(cocor)
library(predictionInterval)

# load data
bfi <- read_csv(file="bfi2.csv")
# View(bfi)


# create correlation table
apa.cor.table(bfi)


# 1 Look at correlation between A1 and C1, and E1 and O1
cocor(~A1+C1|E1+O1, data=as.data.frame(bfi))


# 2 Look at correlation between A1 and C1 and A1 and E1
cocor(~A1+C1|A1+E1, data=as.data.frame(bfi))


# 3 create data sets for men and women
bfi_men <- bfi %>% filter(gender==1) %>% select(-gender)
bfi_women <- bfi %>% filter(gender==2) %>% select(-gender)


# create correlation tables
apa.cor.table(bfi_men)
apa.cor.table(bfi_women)


#compare men and women
bfi_men <- as.data.frame(bfi_men)
bfi_women <- as.data.frame(bfi_women)

cocor(~A1+E1|A1+E1, data=list(bfi_men, bfi_women))


## correlation matrix time
## 4 determine correlation between rating-raises and rating-critical correlation
r.jk <- .59
r.jh <- .16
r.kh <- .38
n <- 30

cocor.dep.groups.overlap(r.jk, r.jh, r.kh, n)


## 5 determine correlation between rating-complaint and raises-critical correlation
r.jk <- .59
r.hm <- .19
r.jh <- .83
r.jm <- .16
r.kh <- .67
r.km <- .38
n <- 30

cocor.dep.groups.nonoverlap(r.jk, r.hm, r.jh, r.jm, r.kh, r.km, n)



## 6 Comparing correlations from 2 papers
r1.jk <- .59
r2.hm <- .03
n1=30
n2=3000

cocor.indep.groups(r1.jk, r2.hm, n1, n2)

