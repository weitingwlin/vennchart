0. Download, install, library the package
----------------------------------------

    # install.packages("devtools")
     library(devtools)
    # install the package from github
     install_github("vennchart", "weitingwlin")
     library(vennchart)

1. Prepare mock data
--------------------

    rm(list =ls())
    co_A <- 8 # including pure and overlapping parts
    co_B <- 12
    co_C <- 3
    co_AB <- 1.9 # overlaping parts
    co_BC <- 0.5
    co_AC <-1



2. Examples
-----------

### 2.1 two-circle Venn chart (with default)

    Q <- vennChart(A_all = co_A, B_all = co_B, AandB = co_AB)

![](image/unnamed-chunk-3-1.png)

### 2.2 three-circle Venn chart (more parameters)

    Q <- vennChart(A_all = co_A, B_all = co_B, C_all = co_C,
                    AandB = co_AB, AandC = co_AC, BandC = co_BC,
                   # rename element A,B,C
                   # rewrite format
                   names = c("a", "b", "c"), connect = "\n(", unit = "%)", 
                   # adjust text positions
                   textadj = c(1.3, 0.3),
                   # make the text relatively bigger
                   cex = 1.3,
                   # make the frame relatively smaller
                   frameadj = 1.2)

![](image/unnamed-chunk-4-1.png)
