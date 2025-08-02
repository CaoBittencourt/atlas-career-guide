# executable
bin := mctspp_example

# c compiler
cc := g++

# project root (for absolute paths)
root := ./../../

# source code
src := ./examples

# build directory
build := ./build

# compile
$(build)/$(bin):
	$(cc) -I $(root) examples/main.cc ../../dsds/dsds.cc ../../lalala/lalala.cc -o $(build)/$(bin)
# 	$(cc) -I $(root) examples/main.cc imports/libDataFrame.a -o $(build)/$(bin)
