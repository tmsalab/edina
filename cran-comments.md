## Test environments

- local R installation, R 3.6.2
- ubuntu 16.04 (on travis-ci), R 3.6.2
- win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

- This is a new release and a resubmit to address CRAN remarks.

**Remarks**

- The following misspelled words are due to the shortening of the list of authors
  to "et al.".

> Possibly mis-spelled words in DESCRIPTION:
>   al (21:53)
>   et (21:50)

- The URLs inside the README will become active once the package is listed on CRAN.

> Found the following (possibly) invalid URLs:
>  URL: http://www.r-pkg.org/pkg/edina (moved to https://www.r-pkg.org:443/pkg/edina)
>    From: README.md
>    Status: 404
>    Message: Not Found
>  URL: https://cran.r-project.org/web/checks/check_results_edina.html
>    From: README.md
>    Status: 404
>    Message: Not Found

- We removed the DOI in the `README.md` and capitalized the B in Bayesian.
