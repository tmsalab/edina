## Test environments

- local R installation, R 3.6.3
- ubuntu 16.04 (on GitHub Actions), R 3.6.3
- win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

> Days since last update: 0

- This release addresses an issue uncovered with `pow()` on Solaris causing 
  the package to fail to build.
  https://www.r-project.org/nosvn/R.check/r-patched-solaris-x86/edina-00install.html
