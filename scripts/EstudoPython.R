# install.packages("reticulate")
# reticulate::py_discover_config()
# reticulate::py_module_available("rpytools")
# install.packages("rpytools")
# ap <- available.packages()
# View(ap)
# library(remotes)
# install_github("cran/rpytools")

library(reticulate)
os <- import("os")
os$listdir(".")



