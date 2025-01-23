# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(qa = mod / compare / qa)

library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |> readRDS() -> df_occupations

# sample skill set
df_occupations[[2]] -> skill_set

# endregion
# model
# region: unweighted overqualification model
skill_set |> qa$oqa(df_occupations[-1])
df_occupations[1:3] |> qa$oqa(df_occupations[1:19])

# endregion
# region: equivalent overqualification model
skill_set |> qa$oqa(df_occupations[-1], aeq_method = "linear-logistic")
df_occupations[1:3] |> qa$oqa(df_occupations[1:19], aeq_method = "linear-logistic")

# endregion
# region: unweighted underqualification model
skill_set |> qa$uqa(df_occupations[-1])
df_occupations[1:3] |> qa$uqa(df_occupations[1:19])

# endregion
# region: equivalent underqualification model
skill_set |> qa$uqa(df_occupations[-1], aeq_method = "linear-logistic")
df_occupations[1:3] |> qa$uqa(df_occupations[1:19], aeq_method = "linear-logistic")

# endregion
# region: unweighted sufficient qualification model
skill_set |> qa$sqa(df_occupations[-1])
df_occupations[1:3] |> qa$sqa(df_occupations[1:19])

# endregion
# region: equivalent sufficient qualification model
skill_set |> qa$sqa(df_occupations[-1], aeq_method = "linear-logistic")
df_occupations[1:3] |> qa$sqa(df_occupations[1:19], aeq_method = "linear-logistic")

# endregion
