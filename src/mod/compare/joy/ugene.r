# region: imports
box::use(
  describe / gene[...]
)

# endregion
# region: utility generality function
ugene <- function(pref_set) {
  return(pref_set |> gene())
}

# endregion
# region: exports
box::export(ugene)

# endregion
