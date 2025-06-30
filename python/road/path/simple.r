# modules
options(box.path = Sys.getenv("ATLAS_MOD"))

# imports

# optimize cost and career path transition
# assuming morphing hypothesis
# relax morphing with mcts later
cost <- function(skq, xk, xq, tk, tq) {
  # switch then work
  # switch then study
  # work then switch
  # study then switch
  # reset then switch
}
