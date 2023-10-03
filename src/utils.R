if (!exists("utils")) {
  utils <- new.env()
}

assign("packages_check", function(packages){
    # take vector of packages required by this module and install them if they're not installed
    # load all of installed packages
    for (package in packages){
        if (!require(package, character.only = TRUE, quietly = TRUE)){
            install.packages(package, dependencies = TRUE)
            library(package, character.only = TRUE)
        }
    }
}, envir = utils)