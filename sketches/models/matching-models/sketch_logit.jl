#region SETUP
#endregion
#region packages
import Pkg

Pkg.add("DataFrames")
Pkg.add("JuliaDB")
Pkg.add("GLM")
Pkg.add("CSV")

using DataFrames
using JuliaDB
using GLM
using CSV

#endregion
#region data

df_occupations =
    DataFrame(CSV.File("C:/Users/Cao/Documents/Github/atlas-research/data/df_atlas.complete_equamax_15.csv"))

first(df_occupations, 10)

#endregion
#region MODEL
#endregion
#region data wrangling
unstack(
    JuliaDB.colnames(df_occupations)
    df_occupations.colnames
    :
)
#endregion
#region logistic regression

#endregion
