#devtools::build()

#Run the code below to ensure the package can be installed and passes all tests.
devtools::install()
library(ANOVApower)
devtools::test()
devtools::check()
covr::package_coverage()




