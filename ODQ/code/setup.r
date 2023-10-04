pacman = function(pkg) {
  new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

pkgs = c(
  "xml2",
  "tibble",
  "dplyr",
  "parallel"
)

options(scipen = 999) # Turn off scientific notation
pacman(pkgs)
rm(pkgs)
