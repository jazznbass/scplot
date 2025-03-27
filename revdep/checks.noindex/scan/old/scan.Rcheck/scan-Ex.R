pkgname <- "scan"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('scan')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_l2")
### * add_l2

flush(stderr()); flush(stdout())

### Name: add_l2
### Title: Add level-2 data
### Aliases: add_l2
### Keywords: transform

### ** Examples

Leidig2018 %>% add_l2(Leidig2018_l2)



cleanEx()
nameEx("as.data.frame.scdf")
### * as.data.frame.scdf

flush(stderr()); flush(stdout())

### Name: as.data.frame.scdf
### Title: Creating a long format data frame from several single-case data
###   frames (scdf).
### Aliases: as.data.frame.scdf
### Keywords: manip

### ** Examples


## Convert the list of three single-case data frames from Grosche (2011)
### into one long data frame
Grosche2011
Grosche2011_long <- as.data.frame(Grosche2011)
Grosche2011_long

## Combine an scdf with data for l2
Leidig2018_long <- as.data.frame(Leidig2018, l2 = Leidig2018_l2)
names(Leidig2018_long)
summary(Leidig2018_long)




cleanEx()
nameEx("autocorr")
### * autocorr

flush(stderr()); flush(stdout())

### Name: autocorr
### Title: Autocorrelation for single-case data
### Aliases: autocorr
### Keywords: regression

### ** Examples

## Compute autocorrelations for a list of four single-cases up to lag 2.
autocorr(Huber2014, lag_max = 2)



cleanEx()
nameEx("batch_apply")
### * batch_apply

flush(stderr()); flush(stdout())

### Name: batch_apply
### Title: Apply a function to each element in an scdf.
### Aliases: batch_apply

### ** Examples

batch_apply(exampleAB, coef(plm(.)))




cleanEx()
nameEx("cdc")
### * cdc

flush(stderr()); flush(stdout())

### Name: cdc
### Title: Conservative Dual-Criterion Method
### Aliases: cdc
### Keywords: overlap

### ** Examples


## Apply the CDC method (standard OLS line)
design <- design(n = 1, slope = 0.2)
dat <- random_scdf(design, seed = 42)
cdc(dat)

## Apply the CDC with Koenig's bi-split and an expected decrease in phase B.
cdc(exampleAB_decreasing, decreasing = TRUE, trend_method = "bisplit")

## Apply the CDC with Tukey's tri-split, comparing the first and fourth phase
cdc(exampleABAB, trend_method = "trisplit", phases = c(1,4))

## Apply the Dual-Criterion (DC) method (i.e., mean and trend without
##shifting).
cdc(
 exampleAB_decreasing,
 decreasing = TRUE,
 trend_method = "bisplit",
 conservative = 0
)





cleanEx()
nameEx("coef.sc_plm")
### * coef.sc_plm

flush(stderr()); flush(stdout())

### Name: coef.sc_plm
### Title: Extract coefficients from plm/hplm objects
### Aliases: coef.sc_plm

### ** Examples

coefficients(plm(exampleAB$Johanna))




cleanEx()
nameEx("convert")
### * convert

flush(stderr()); flush(stdout())

### Name: convert
### Title: Convert
### Aliases: convert
### Keywords: io

### ** Examples

filename <- tempfile()
convert(exampleABC, file = filename)
source(filename)
all.equal(study, exampleABC)
unlink(filename)



cleanEx()
nameEx("corrected_tau")
### * corrected_tau

flush(stderr()); flush(stdout())

### Name: corrected_tau
### Title: Baseline corrected tau
### Aliases: corrected_tau

### ** Examples

dat <- scdf(c(A = 33,25,17,25,14,13,15, B = 15,16,16,5,7,9,6,5,3,3,8,11,7))
corrected_tau(dat)



cleanEx()
nameEx("describe")
### * describe

flush(stderr()); flush(stdout())

### Name: describe
### Title: Descriptive statistics for single-case data
### Aliases: describe

### ** Examples


## Descriptive statistics for a study of three single-cases
describe(Grosche2011)

## Descriptives of a three phase design
describe(exampleABC)

## Write descriptive statistics to .csv-file
study <- describe(Waddell2011)
write.csv(study$descriptives, file = tempfile())



cleanEx()
nameEx("design")
### * design

flush(stderr()); flush(stdout())

### Name: design
### Title: Generate a single-case design matrix
### Aliases: design
### Keywords: datagen

### ** Examples

 ## Create random single-case data and inspect it
 design <- design(
   n = 3, rtt = 0.75, slope = 0.1, extreme_prop = 0.1,
   missing_prop = 0.1
 )
 dat <- random_scdf(design, round = 1, random.names = TRUE, seed = 123)
 describe(dat)

 ## And now have a look at poisson-distributed data
 design <- design(
   n = 3, B_start = c(6, 10, 14), mt = c(12, 20, 22), start_value = 10,
   distribution = "poisson", level = -5, missing_prop = 0.1
 )
 dat <- random_scdf(design, seed = 1234)
 pand(dat, decreasing = TRUE)



cleanEx()
nameEx("estimate_design")
### * estimate_design

flush(stderr()); flush(stdout())

### Name: estimate_design
### Title: Estimate single-case design
### Aliases: estimate_design

### ** Examples

# create a random scdf with predefined parameters
set.seed(1234)
design <- design(
  n = 10, trend = -0.02,
  level = list(0, 1), rtt = 0.8,
  s = 1
)
scdf<- random_scdf(design)

# Estimate the parameters based on the scdf and create a new random scdf
# based on these estimations
design_est <- estimate_design(scdf, rtt = 0.8)
scdf_est <- random_scdf(design_est)

# Analyze both datasets with an hplm model. See how similar the estimations
# are:
hplm(scdf, slope = FALSE)
hplm(scdf_est, slope = FALSE)

# Also similar results for pand and randomization tests:
pand(scdf)
pand(scdf_est)
rand_test(scdf)
rand_test(scdf_est)



cleanEx()
nameEx("fill_missing")
### * fill_missing

flush(stderr()); flush(stdout())

### Name: fill_missing
### Title: Replacing missing measurement times in single-case data
### Aliases: fill_missing
### Keywords: manip

### ** Examples


## In his study, Grosche (2011) could not realize measurements each
## single week for all participants. During the course of 100 weeks,
## about 20 measurements per person at different times were administered.

## Fill missing values in a single-case dataset with discontinuous
## measurement times
Grosche2011filled <- fill_missing(Grosche2011)
study <- c(Grosche2011[2], Grosche2011filled[2])
names(study) <- c("Original", "Filled")
plot(study)

## Fill missing values in a single-case dataset that are NA
Maggie <- random_scdf(design(level = list(0,1)), seed = 123)
Maggie_n <- Maggie
replace.positions <- c(10,16,18)
Maggie_n[[1]][replace.positions,"values"] <- NA
Maggie_f <- fill_missing(Maggie_n)
study <- c(Maggie, Maggie_n, Maggie_f)
names(study) <- c("original", "missing", "interpolated")
plot(study, marks = list(positions = replace.positions), style = "grid2")




cleanEx()
nameEx("hplm")
### * hplm

flush(stderr()); flush(stdout())

### Name: hplm
### Title: Hierarchical piecewise linear model / piecewise regression
### Aliases: hplm print.sc_hplm export.sc_hplm coef.sc_hplm

### ** Examples


## Compute hplm model on a MBD over fifty cases (restricted log-likelihood)
hplm(exampleAB_50, method = "REML", random.slopes = FALSE)

## Analyzing with additional L2 variables
Leidig2018 %>%
  add_l2(Leidig2018_l2) %>%
  hplm(update.fixed = .~. + gender + migration + ITRF_TOTAL*phaseB,
       slope = FALSE, random.slopes = FALSE, lr.test = FALSE
  )




cleanEx()
nameEx("mplm")
### * mplm

flush(stderr()); flush(stdout())

### Name: mplm
### Title: Multivariate Piecewise linear model / piecewise regression
### Aliases: mplm print.sc_mplm

### ** Examples

res <- mplm(Leidig2018$`1a1`,
  dvar = c("academic_engagement", "disruptive_behavior")
)
print(res)
## also report standardized coefficients:
print(res, std = TRUE)



cleanEx()
nameEx("nap")
### * nap

flush(stderr()); flush(stdout())

### Name: nap
### Title: Nonoverlap of all Pairs
### Aliases: nap

### ** Examples


## Calculate NAP for a study with  lower expected phase B scores
## (e.g. aggressive behavior)
gretchen <- scdf(c(A = 12, 14, 9, 10, B = 10, 6, 4, 5, 3, 4))
nap(gretchen, decreasing = TRUE)

## Request NAP for all cases from the Grosche2011 scdf
nap(Grosche2011)




cleanEx()
nameEx("outlier")
### * outlier

flush(stderr()); flush(stdout())

### Name: outlier
### Title: Handling outliers in single-case data
### Aliases: outlier
### Keywords: manip

### ** Examples


## Identify outliers using 1.5 standard deviations as criterion
susanne <- random_scdf(level = 1.0)
res_outlier <- outlier(susanne, method = "SD", criteria = 1.5)
plot(susanne, marks = res_outlier)

## Identify outliers in the original data from Grosche (2011)
## using Cook's Distance greater than 4/n as criterion
res_outlier <- outlier(Grosche2011, method = "Cook", criteria = "4/n")
plot(Grosche2011, marks = res_outlier)




cleanEx()
nameEx("overlap")
### * overlap

flush(stderr()); flush(stdout())

### Name: overlap
### Title: Overlap indices for single-case data
### Aliases: overlap

### ** Examples


## Display overlap indices for one single-case
overlap(Huitema2000, decreasing = TRUE)

## Display overlap indices for six single-cases
overlap(GruenkeWilbert2014)

## Combining phases for analyszing designs with more than two phases
overlap(exampleA1B1A2B2, phases = list(c("A1","A2"), c("B1","B2")))




cleanEx()
nameEx("pand")
### * pand

flush(stderr()); flush(stdout())

### Name: pand
### Title: Percentage of all non-overlapping data
### Aliases: pand print.sc_pand export.sc_pand

### ** Examples

## REplication of the Parker et al. 2007 example
pand(Parker2007)

## Calculate the PAND with an expected decrease of phase B scores
cubs <- scdf(c(20,22,24,17,21,13,10,9,20,9,18), B_start = 5)
pand(cubs, decreasing = TRUE)




cleanEx()
nameEx("pem")
### * pem

flush(stderr()); flush(stdout())

### Name: pem
### Title: Percent exceeding the median
### Aliases: pem

### ** Examples


## Calculate the PEM including the Binomial and Chi-square tests for a single-case
dat <- random_scdf(5, level = 0.5)
pem(dat, chi.test = TRUE)




cleanEx()
nameEx("pet")
### * pet

flush(stderr()); flush(stdout())

### Name: pet
### Title: Percent exceeding the trend
### Aliases: pet

### ** Examples


## Calculate the PET and use a 99%-CI for the additional calculation
# create random example data
design <- design(n = 5, slope = 0.2)
dat <- random_scdf(design, seed = 23)
pet(dat, ci = .99)




cleanEx()
nameEx("plm")
### * plm

flush(stderr()); flush(stdout())

### Name: plm
### Title: Piecewise linear model / piecewise regression
### Aliases: plm

### ** Examples


## Compute a piecewise regression model for a random single-case
set.seed(123)
AB <- design(
  phase_design = list(A = 10, B = 20),
  level = list(A = 0, B = 1), slope = list(A = 0, B = 0.05),
  trend = 0.05
)
dat <- random_scdf(design = AB)
plm(dat, AR = 3)

## Another example with a more complex design
A1B1A2B2 <- design(
  phase_design = list(A1 = 15, B1 = 20, A2 = 15, B2 = 20),
  level = list(A1 = 0, B1 = 1, A2 = -1, B2 = 1),
  slope = list(A1 = 0, B1 = 0.0, A2 = 0, B2 = 0.0),
  trend = 0.0)
dat <- random_scdf(design = A1B1A2B2, seed = 123)
plm(dat, contrast = "preceding")

## no slope effects were found. Therefore, you might want to the drop slope
## estimation:
plm(dat, slope = FALSE, contrast = "preceding")

## and now drop the trend estimation as well
plm(dat, slope = FALSE, trend = FALSE, contrast = "preceding")

## A poisson regression
example_A24 %>%
  plm(family = "poisson")

## A binomial regression (frequencies as dependent variable)
plm(exampleAB_score$Christiano, family = "binomial", var_trials = "trials")

## A binomial regression (percentage as dependent variable)
exampleAB_score$Christiano %>%
  transform(percentage = values/trials) %>%
  set_dvar("percentage") %>%
  plm(family = "binomial", var_trials = "trials", dvar_percentage = TRUE)



cleanEx()
nameEx("plot.scdf")
### * plot.scdf

flush(stderr()); flush(stdout())

### Name: plot.scdf
### Title: Plot single-case data
### Aliases: plot.scdf plotSC

### ** Examples


## Request the default plot of the data from Borckhardt (2014)
plot(Borckardt2014)

## Plot the three cases from Grosche (2011) and visualize the phase A trend
plot(Grosche2011, style = "grid", lines = "trendA")

## Request the local regression line for Georg from that data set and customize the plot
plot(Grosche2011$Georg, style = "sienna", ylim = c(0,NA),
       xlab = "Training session", ylab = "Words per minute",
       phase.names = c("Baseline", "Intervention"), xinc = 5,
       lines = list(type = "loreg", f = 0.2, lty = "solid", col = "black", lwd = 3))

## Plot a random MBD over three cases and mark interesting MTs
dat <- random_scdf(design = design(3))
plot(dat, marks = list(positions = list(c(2,4,5),c(1,2,3),c(7,8,9)), col = "blue",
       cex = 1.4), style = c("grid", "annotate", "tiny"))




cleanEx()
nameEx("pnd")
### * pnd

flush(stderr()); flush(stdout())

### Name: pnd
### Title: Percentage of non-overlapping data
### Aliases: pnd

### ** Examples


## Calculate the PND for multiple single-case data
pnd(GruenkeWilbert2014)




cleanEx()
nameEx("power_test")
### * power_test

flush(stderr()); flush(stdout())

### Name: power_test
### Title: Empirical power analysis for single-case data
### Aliases: power_test

### ** Examples


## Assume you want to conduct a single-case study with 15 measurements
## (phases: A = 6 and B = 9) using a highly reliable test and
## an expected level effect of d = 1.4.
## A (strong) trend effect is trend = 0.05. What is the power?
## (Note: n_sims is set to 10. Set n_sims to 1000 for a serious calculation.)
design <- design(
  n = 1, phase_design = list(A = 6, B = 9),
  rtt = 0.8, level = 1.4, trend = 0.05
)
power_test(design, n_sim = 10)

## Would you achieve higher power by setting up a MBD with three cases?
design <- design(
  n = 3, phase_design = list(A = 6, B = 9),
  rtt = 0.8, level = 1.4, trend = 0.05
)
power_test(design, n_sim=10, method=list("hplm_level", "rand", "tauU_meta"))



cleanEx()
nameEx("rand_test")
### * rand_test

flush(stderr()); flush(stdout())

### Name: rand_test
### Title: Randomization Tests for single-case data
### Aliases: rand_test

### ** Examples


## Compute a randomization test on the first case of the byHeart2011 data and include a graph
rand_test(byHeart2011[1], statistic = "Median B-A", graph = TRUE, seed = 123)

## Compute a randomization test on the Grosche2011 data using complete permutation
rand_test(Grosche2011, statistic = "Median B-A", complete = TRUE, limit = 4, seed = 123)




cleanEx()
nameEx("random_scdf")
### * random_scdf

flush(stderr()); flush(stdout())

### Name: random_scdf
### Title: Single-case data generator
### Aliases: random_scdf
### Keywords: datagen

### ** Examples


## Create random single-case data and inspect it
design <- design(
  n = 3, rtt = 0.75, slope = 0.1, extreme_prop = 0.1,
  missing_prop = 0.1
)
dat <- random_scdf(design, round = 1, random_names = TRUE, seed = 123)
describe(dat)

## And now have a look at poisson-distributed data
design <- design(
  n = 3, B_start = c(6, 10, 14), mt = c(12, 20, 22), start_value = 10,
  distribution = "poisson", level = -5, missing_prop = 0.1
)
dat <- random_scdf(design, seed = 1234)
pand(dat, decreasing = TRUE)



cleanEx()
nameEx("ranks")
### * ranks

flush(stderr()); flush(stdout())

### Name: ranks
### Title: Rank-transformation of single-case data files
### Aliases: ranks
### Keywords: internal

### ** Examples

ranks(Huber2014, var = "compliance")



cleanEx()
nameEx("rci")
### * rci

flush(stderr()); flush(stdout())

### Name: rci
### Title: Reliable change index
### Aliases: rci

### ** Examples


## Report the RCIs of the first case from the byHeart data and include a graph
rci(byHeart2011[1], graph = TRUE, rel = 0.8)




cleanEx()
nameEx("read_scdf")
### * read_scdf

flush(stderr()); flush(stdout())

### Name: read_scdf
### Title: Load single-case data from files
### Aliases: read_scdf
### Keywords: io

### ** Examples


## Read SC-data from a file named "study1.csv" in your working directory
# study1 <- read_scdf("study1.csv")

## Read SC-data from a .csv-file with semicolon as field and comma as decimal separator
# study2 <- read_scdf("study2.csv", sep = ";", dec = ",")

## write_scdf and read_scdf
filename <- file.path(tempdir(), "test.csv")
write_scdf(exampleA1B1A2B2_zvt, filename)
dat <- read_scdf(filename, cvar = "case", pvar = "part", dvar = "zvt", mvar = "day")
res1 <- describe(exampleA1B1A2B2_zvt)$descriptives
res2 <- describe(dat)$descriptives
all.equal(res1,res2)




cleanEx()
nameEx("sample_names")
### * sample_names

flush(stderr()); flush(stdout())

### Name: sample_names
### Title: Samples random names
### Aliases: sample_names

### ** Examples

sample_names(3)



cleanEx()
nameEx("scdf")
### * scdf

flush(stderr()); flush(stdout())

### Name: scdf
### Title: Single case data frame
### Aliases: scdf scdf-class as.scdf

### ** Examples


## Scores on a letter naming task were collected on eleven days in a row.
## The intervention started after the fifth measurement,
## so the first B phase measurement was 6 (B_start = 6).
klaas <- scdf(
  c(5, 7, 8, 5, 7, 12, 16, 18, 15, 14, 19),
  B_start = 6, name = "Klaas"
)
describe(klaas)

# Alternative coding 1:
klaas <- scdf(
  c(A = 5, 7, 8, 5, 7, B = 12, 16, 18, 15, 14, 19),
  name = "Klaas"
)

# Alternative coding 2:
klaas <- scdf(
  c(5, 7, 8, 5, 7, 12, 16, 18, 15, 14, 19),
  phase_design = c(A = 5, B = 6), name = "Klaas"
)

## Unfortunately in a similar study there were no data collected on
## days 3 and 9. Use NA to pass them to the function:
emmi <- scdf(c(5, 7, NA, 5, 7, 12, 16, 18, NA, 14, 19),
  phase_design = c(A = 5, B = 6), name = "Emmi"
)
describe(emmi)

## In a MBD over three cases, data were collected eleven days in a row.
## Intervention starting points differ between subjects as they were
## randomly assigned. The three SCDFs are then combined in a list for
## further conjoined analyses.
charlotte <- scdf(c(A = 5, 7, 10, 5, 12, B = 7, 10, 18, 15, 14, 19))
theresa <- scdf(c(A = 3, 4, 3, 5, B = 7, 4, 7, 9, 8, 10, 12))
antonia <- scdf(c(A = 9, 8, 8, 7, 5, 7, B = 6, 14, 15, 12, 16))
mbd <- c(charlotte, theresa, antonia)
names(mbd) <- c("Charlotte", "Theresa", "Antonia")
overlap(mbd)

## In a classroom-based intervention it was not possible to measure outcomes
## every day, but only on schooldays. The sequence of measurements is passed
## to the package by using a vector of measurement times.
frida <- scdf(
  c(A = 3, 2, 4, 2, 2, 3, 5, 6, B = 8, 10, 8, 12, 14, 13, 12),
  mt = c(1, 2, 3, 4, 5, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18)
)
summary(frida)
describe(frida)

## example with two independent variables and four phases
jim <- scdf(
  zvt = c(47, 58, 76, 63, 71, 59, 64, 69, 72, 77, 76, 73),
  d2 = c(131, 134, 141, 141, 140, 140, 138, 140, 141, 140, 138, 140),
  phase_design = c(A1 = 3, B1 = 3, A2 = 3, B2 = 3), dvar = "zvt"
)
overlap(jim, phases = list(c("A1", "A2"), c("B1", "B2")))



cleanEx()
nameEx("select_cases")
### * select_cases

flush(stderr()); flush(stdout())

### Name: select_cases
### Title: Select a subset of cases
### Aliases: select_cases

### ** Examples

select_cases(exampleAB, Johanna, Karolina)
select_cases(exampleAB, c(Johanna, Karolina))
select_cases(exampleAB, 1,2)
select_cases(exampleAB, 1:2)
select_cases(exampleAB, -Johanna)
select_cases(exampleAB, -c(Johanna, Karolina))
v <- c("Moritz", "Jannis")
select_cases(exampleA1B1A2B2, v)



cleanEx()
nameEx("select_phases")
### * select_phases

flush(stderr()); flush(stdout())

### Name: select_phases
### Title: Select and combine phases for overlap analyses
### Aliases: select_phases

### ** Examples

exampleA1B1A2B2_zvt %>%
  select_phases(A = c(1, 3), B = c(2, 4)) %>%
  overlap()



cleanEx()
nameEx("set_vars")
### * set_vars

flush(stderr()); flush(stdout())

### Name: set_vars
### Title: Set analysis variables in an scdf
### Aliases: set_vars set_dvar set_mvar set_pvar

### ** Examples

exampleAB_add %>% 
  set_dvar("depression") %>%
  describe()



cleanEx()
nameEx("shift")
### * shift

flush(stderr()); flush(stdout())

### Name: shift
### Title: Shift values in a single-case data file
### Aliases: shift
### Keywords: internal

### ** Examples

### Shift the measurement time for a better estimation of the intercept
ex <- shift(example_A24, value = -1996)
plm(ex)

# Please use transform instead:
example_A24 %>%
  transform(year = year - 1996) %>%
  plm()



cleanEx()
nameEx("smd")
### * smd

flush(stderr()); flush(stdout())

### Name: smd
### Title: Standardized mean differences
### Aliases: smd

### ** Examples

smd(exampleAB)



cleanEx()
nameEx("smooth_cases")
### * smooth_cases

flush(stderr()); flush(stdout())

### Name: smooth_cases
### Title: Smoothing single-case data
### Aliases: smooth_cases
### Keywords: internal

### ** Examples


## Use the three different smoothing functions and compare the results
study <- c(
  "Original" = Huber2014$Berta,
  "Moving median" = smooth_cases(Huber2014$Berta, method = "median"),
  "Moving mean" = smooth_cases(Huber2014$Berta, method = "mean"),
  "Local regression" = smooth_cases(Huber2014$Berta, method = "regression")
)
plot(study)

Huber2014$Berta %>% 
transform(
  "compliance (moving median)" = moving_median(compliance),
  "compliance (moving mean)" = moving_mean(compliance),
  "compliance (local regression)" = local_regression(compliance, mt)
)




cleanEx()
nameEx("standardize")
### * standardize

flush(stderr()); flush(stdout())

### Name: standardize
### Title: Standardize values of an scdf file
### Aliases: standardize
### Keywords: internal

### ** Examples


## Standardize a multiple case scdf and compute an hplm
exampleAB_50 %>%
  standardize("values", center = TRUE, scale = TRUE) %>%
  hplm()

## The more versatile transform function supersedes standardize:
exampleAB_50 %>%
  transform(values = (values - mean(all(values))) / sd(all(values))) %>%
  hplm()



cleanEx()
nameEx("style_plot")
### * style_plot

flush(stderr()); flush(stdout())

### Name: style_plot
### Title: Create styles for single-case data plots
### Aliases: style_plot

### ** Examples

newstyle <- style_plot(style = "default")
newstyle$text.ABlag <- c("START", "END")
newstyle$col.dots <- ""
newstyle$annotations <- list(cex = 0.6, col = "grey10", offset = 0.4)
newstyle$names <- list(cex = 0.8, col = "blue", side = 1, adj = 1, line = -1, at = 31)
newstyle$fill.bg <- c("grey99", "grey95", "grey90")
plot(exampleABC, style = newstyle, main = "Example Plot")




cleanEx()
nameEx("subset.scdf")
### * subset.scdf

flush(stderr()); flush(stdout())

### Name: subset.scdf
### Title: Subset cases, rows, and variables
### Aliases: subset.scdf

### ** Examples

exampleAB %>%
  subset((values < 60 & phase == "A") | (values >= 60 & phase == "B"))
subset(exampleAB_add, select = c(-cigarrets, -depression))
subset(exampleAB, cases = c(Karolina, Johanna))
subset(exampleA1B1A2B2, phase %in% c("A1", "B2"), cases = Pawel:Moritz)



cleanEx()
nameEx("tau_u")
### * tau_u

flush(stderr()); flush(stdout())

### Name: tau_u
### Title: Tau-U for single-case data
### Aliases: tau_u print.sc_tauu export.sc_tauu

### ** Examples


tau_u(Grosche2011$Eva)

## Replicate  tau-U calculation from Parker et al. (2011)
bob <- scdf(c(A = 2, 3, 5, 3, B = 4, 5, 5, 7, 6), name = "Bob")
res <- tau_u(bob, method = "parker", tau_method = "a")
print(res, complete = TRUE)

## Request tau-U for all single-cases from the Grosche2011 data set
tau_u(Grosche2011)



cleanEx()
nameEx("transform.scdf")
### * transform.scdf

flush(stderr()); flush(stdout())

### Name: moving_median
### Title: Transform every single case of a single case data frame
### Aliases: moving_median moving_mean local_regression first_of
###   across_cases all_cases transform.scdf

### ** Examples

## Creates a single-case with frequency distributions. The proportion and
## percentage of the frequencies are calculated with transform:
design <- design(
 n = 3,
 level = 5,
 distribution = "binomial",
 n_trials = 20,
 start_value = 0.5
)
study <- random_scdf(design)
transform(study, proportion = values/trials, percentage = proportion * 100)

## Z standardize the dependent variable and add two new variables:
exampleAB %>%
  transform(
    values = scale(values),
    mean_values = mean(values),
    sd_values = sd(values)
  )

## Use `all` to calculate global variables.
exampleAB %>%
  transform(
    values_center_case = values - mean(values[phase == "A"]),
    values_center_global = values - mean(all(values[phase == "A"])),
    value_dif = values_center_case - values_center_global
  )

## Use `across_cases` to calculate or replace a variable with values from
## all cases. E.g., standardize the dependent variable:
exampleABC %>%
  transform(
    across_cases(values = scale(values))
  )

## Rank transform the values based on all cases vs. within each case:
exampleABC %>%
  transform(
    across_cases(values_across = rank(values, na.last="keep")),
    value_within = rank(values, na.last="keep")
  )

## Three helper functions to smooth the data
Huber2014$Berta %>%
transform(
  "compliance (moving median)" = moving_median(compliance),
  "compliance (moving mean)" = moving_mean(compliance),
  "compliance (local regression)" = local_regression(compliance, mt)
)

## Function first_of() helps to set NAs for specific phases.
## E.g., you want to replace the first two values of phase A and the first
## value of phase B and its preceding value.

byHeart2011 %>%
  transform(
    values = replace(values, first_of(phase == "A", 0:1), NA),
    values = replace(values, first_of(phase == "B", -1:0), NA)
  )



cleanEx()
nameEx("trend")
### * trend

flush(stderr()); flush(stdout())

### Name: trend
### Title: Trend analysis for single-cases data
### Aliases: trend

### ** Examples


## Compute the linear and squared regression for a random single-case
design <- design(slope = 0.5)
matthea <- random_scdf(design)
trend(matthea)

## Besides the linear and squared regression models compute two custom models:
## a) a cubic model, and b) the values predicted by the natural logarithm of the
## measurement time.
design <- design(slope = 0.3)
ben <- random_scdf(design)
trend(ben, offset = 0, model = c("Cubic" = values ~ I(mt^3), "Log Time" = values ~ log(mt)))




cleanEx()
nameEx("truncate_phase")
### * truncate_phase

flush(stderr()); flush(stdout())

### Name: truncate_phase
### Title: Truncate single-case data
### Aliases: truncate_phase
### Keywords: internal

### ** Examples


## Truncate the first two data points of both phases and compare the two 
## data sets
study <- c(
  "Original" = byHeart2011[1],
  "Selected" = truncate_phase(
    byHeart2011[1], truncate = list(A = c(2, 0), B = c(2, 0))
  )
)
plot(study)



cleanEx()
nameEx("write_scdf")
### * write_scdf

flush(stderr()); flush(stdout())

### Name: write_scdf
### Title: Data output
### Aliases: write_scdf
### Keywords: io

### ** Examples

## write single-case data to a .csv-file
filename <- tempfile(fileext = ".csv")
jessica <- random_scdf(design(level = .5))
write_scdf(jessica, tempfile())

## write multiple cases to a .csv-file with semicolon as field and comma as
## decimal separator
write_scdf(Grosche2011, filename, sep = ";", dec = ",")

## read_scdf and write_scdf
write_scdf(exampleA1B1A2B2_zvt, filename)
dat <- read_scdf(filename, cvar = "case", pvar = "part",
                 dvar = "zvt", mvar = "day")
res1 <- describe(exampleA1B1A2B2_zvt)$descriptives
res2 <- describe(dat)$descriptives
all.equal(res1,res2)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
