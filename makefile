cc := g++

build := ./build
mctspp := ./src/mctspp
src := $(mctspp)/mcts

target:
	cd $(mctspp) && make
