# c compiler
cc := g++

# build directory
build := ./build

# source directory
src := ./src


# modules
## mctspp
mctsppDir := "$(src)/mctspp/dataframe/examples"
mctsppBin = "mctspp"

$(mctsppBin):
	@echo Compiling $(mctsppBin)...
	@cd $(mctsppDir) && sudo make -o $(build)/$(mctsppBin)
	@echo Compiled $(mctsppBin) to $(build)/$(mctsppBin).

## lalala
lalalaDir := "$(src)/mctspp/dataframe/examples"
lalalaBin = "lalala"

$(lalalaBin):
	@echo Compiling $(lalalaBin)...
	@cd $(lalalaDir) && sudo make -o $(build)/$(lalalaBin)
	@echo Compiled $(lalalaBin) to $(build)/$(lalalaBin).
