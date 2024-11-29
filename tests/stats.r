# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

# set modules path
project.root()

# imports
box::use(
  st = mod / stats
)
