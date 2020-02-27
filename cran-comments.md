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

- We've reduced the capitalization in the DESCRIPTION heading to only
  capitalize names, sentence beginnings and abbreviations.
- We've deactivated the DOI in the README and removed references to the 
  r-pkg website. 
- wherever possible, we added small sample executables. That said, the
  algorithm is O(2^k). 
- As a result, the examples given need to be protected with `\donttest{}` as 
  they require at least 5 seconds to run. 
