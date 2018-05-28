pkgname <- "toy"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('toy')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("d")
### * d

flush(stderr()); flush(stdout())

### Name: d
### Title: ~~ toy ~~
### Aliases: d
### Keywords: datasets

### ** Examples

data(d)
## maybe str(d) ; plot(d) ...



cleanEx()
nameEx("f1")
### * f1

flush(stderr()); flush(stdout())

### Name: f1
### Title: ~~function to do ... ~~
### Aliases: f1
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y) 
x + y



cleanEx()
nameEx("f2")
### * f2

flush(stderr()); flush(stdout())

### Name: f2
### Title: ~~function to do ... ~~
### Aliases: f2
### Keywords: ~kwd1 ~kwd2

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y) 
x * y



cleanEx()
nameEx("toy-package")
### * toy-package

flush(stderr()); flush(stdout())

### Name: toy-package
### Title: What the package does (short line)
### Aliases: toy-package toy
### Keywords: package

### ** Examples

simple examples of the most important functions



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
