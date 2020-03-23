## Test environments

- local R installation, R 3.6.3
- ubuntu 16.04 (on GitHub Actions), R 3.6.3
- win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

- This is a new release and a resubmit to address CRAN remarks.

**Remarks**

- The following misspelled words are due to the shortening of the list of authors
  to "et al." and the method acronym.

> Possibly mis-spelled words in DESCRIPTION:
> EDINA (20:42)
> al (21:53)
> et (21:50)

- From the last submission:

- We've removed the `donttest` portion for `edina()` by dropping the number of
  observations calculated to 1. We cannot remove the donttest option for 
  `auto_edina()` as it requires at least 10 seconds with 2 different matrices
  being estimated. 

