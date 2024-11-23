# install box if not installed
if (!any(utils::installed.packages()[, 1] == 'box')) {
  install.packages('box', dependencies = T)
}

# imports
box::use(dsds = ./stats)
