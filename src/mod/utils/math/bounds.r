# box::use(
#   stats[uniroot],
#   cubature[cubintegrate],
# )

# w = integral_{lb}^{ub} ta dl

# # get interval bounds from pdf and percentages
# bounds <- function(pdf) {
#   # solve integral equation for ub
#   uniroot(
#     function(ub) {
#       cubintegrate(
#         pdf,
#         lower = lb,
#         upper = ub
#       )$integral - wtilde[[1]]
#     },
#     interval = c(0, 1)
#     # , interval = c(-.5,1.5)
#     , extendInt = "yes"
#   )$root -> ub

#   pmax(ub, 0) -> ub
#   pmin(ub, 1) -> ub

#   # return optimal responsibility upper bound
#   return(ub)
# }
