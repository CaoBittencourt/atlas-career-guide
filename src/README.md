# Atlas Career Guide Source Code
This source code is organized in modules with the `box` R package.
The `box.path` option is set to `path/to/project/root/src/`, which is objectively defined with the `modular` package.
Within the `src/` directory, then, imports are declared relative to `box.path`, as `box` searches for modules in this path.

Each R script in the rest of the repository (i.e. in `path/to/project/root/`, but not in `path/to/project/root/src/`), which imports from `src/` modules must begin with the following preamble, so as to load the project's options and set `box.path` to `src/`:
```
# install box if not installed
if (!any(utils::installed.packages()[, 1] == "box")) {
  install.packages("box", dependencies = T)
}

# install modular if not installed
if (!any(utils::installed.packages()[, 1] == "modular")) {
  devtools::install_github("CaoBittencourt/modular")
}

library(modular)

# objective project root
project.options(
  project.name = "atlas",
  relative.paths = list(
    # source code
    atlas.src = "src",
    box.path = "src",
    atlas.mod = "src/mod",
    # database
    atlas.db = "database",
    atlas.data = "database/data",
    atlas.oldata = "database/data/old/occupations/df_occupations_2022.csv",
    # questionnaires
    atlas.quest = "database/quest",
    # rds
    atlas.rds = "database/data/output/rds",
    atlas.skills_mtx = "database/output/rds/skill_set_mtx.rds",
    atlas.skills = "database/output/rds/skill_sets.rds",
    atlas.labor = "database/output/rds/labor.rds",
    atlas.education = "database/output/rds/education.rds",
    atlas.cao = "database/quest/rds/cao.rds"
  ),
  root.name = ".atlas",
  start.path = ".",
  end.path = "."
)
```

This will conditionally create root and options files, if they do not exist; and load them otherwise. 
Alternatively, one might simply write
```
modular::project.options(project.name = 'atlas')
```
or, even,
```
readRDS('atlas.rds') |> options()
```
if `box` is installed, and `.atlas` and `atlas.rds` options have already been created.