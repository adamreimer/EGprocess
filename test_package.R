# This script is an intermediate while we build and test the package.
# I'll leave lines in here that were run previously so that we have a record for
# the future of what code was run and how different parts were built.

library(usethis)
#usethis::proj_get() # only run once
proj_sitrep()


# Add README
#use_readme_md()

# Ensure roxygen uses markdown in your docs (keeps DESCRIPTION in sync)
use_roxygen_md()


# License (pick one; MIT shown)
use_mit_license("Your Name")

# Document dependencies
# use_package("dplyr", type = "Imports")
# use_package("ggplot2", type = "Imports")
# use_package("tibble", type = "Imports")
# use_package("stringr", type = "Imports")
# use_package("tidyr", type = "Imports")
# use_package("magrittr", type = "Imports")
# use_package("scales", type = "Imports")


# Write the needed .Rd files for each script
devtools::document()





devtools::check()




use_vignette("overview")  # long-form tutorial / workflow
use_pkgdown()             # add pkgdown config
# later, build site locally:
pkgdown::build_site()




devtools::load_all()     # load package without installing
devtools::test()         # run tests
devtools::document()     # rebuild docs & NAMESPACE
devtools::check()        # full check

