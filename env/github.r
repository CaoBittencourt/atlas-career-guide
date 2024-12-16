# setup
library(devtools)

# read PAT
gh_token <- scan("./.ghtoken", character(), quote = "")

# temporarily set PAT
Sys.setenv("GITHUB_PAT" = gh_token)

# read github package files
c(
  # atlas packages
  scan("./atlas.txt", character(), quote = ""),
  # additional github packages
  scan("./github.txt", character(), quote = "")
) -> github_packages

# install github packages
install_github(
  github_packages,
  auth_token = gh_token,
  dependencies = T,
  upgrade = F,
  force = T
)

# remove PAT
Sys.unsetenv("GITHUB_PAT")
