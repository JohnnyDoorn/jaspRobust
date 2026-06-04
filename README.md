<div align="right">

[![Unit Tests](https://github.com/JohnnyDoorn/jaspRobust/actions/workflows/unittests.yml/badge.svg)](https://github.com/JohnnyDoorn/jaspRobust/actions/workflows/unittests.yml)
<br>
<b>Maintainer:</b> <a href="https://github.com/JohnnyDoorn/">Johnny van Doorn</a>

</div>

# The Robust Statistics Module

## Overview

<img src='inst/icons/analysis-classical-anova.svg' width='149' height='173' align='right'/>

**JASP Robust Statistics module** offers robust versions of classical statistical analyses for situations where standard methods are unreliable — heavy-tailed distributions, outliers, heteroscedasticity. The module covers robust **ANOVA**, **repeated-measures ANOVA**, **ANCOVA**, **linear regression**, and **correlation**, drawing on trimmed-mean and Winsorised approaches from Rand Wilcox's work alongside M-estimators from classical robust regression.


## R Packages

<img src='https://www.r-project.org/logo/Rlogo.svg' width='100' height='78' align='right'/>

The functionality is served by two R packages

- **WRS2** — Wilcox's robust statistics for ANOVA, ANCOVA, and correlation ([WRS2 on CRAN](https://cran.r-project.org/package=WRS2))
- **MASS** — Modern Applied Statistics with S; robust linear regression via `rlm()` ([MASS on CRAN](https://cran.r-project.org/package=MASS))


## References

- Wilcox, R. R. (2022). *Introduction to Robust Estimation and Hypothesis Testing* (5th ed.). Academic Press.
- Mair, P., & Wilcox, R. R. (2020). Robust statistical methods in R using the WRS2 package. *Behavior Research Methods*, 52, 464–488. <https://doi.org/10.3758/s13428-019-01246-w>
- Venables, W. N., & Ripley, B. D. (2002). *Modern Applied Statistics with S* (4th ed.). Springer.
