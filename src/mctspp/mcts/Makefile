FLAGS = -O2 -g3 -pedantic -std=c++11 -pthread # -Wall -Wextra
TICTACTOE_EXE = tictactoe
QUORIDOR_EXE = quoridor
COMMON_OBJ = JobScheduler.o mcts.o
SKETCH_EXE = sketch


all: TicTacToe sketch Quoridor


mcts.o: mcts/src/mcts.cpp mcts/include/mcts.h mcts/include/state.h
	g++ -c $(FLAGS) mcts/src/mcts.cpp

JobScheduler.o: mcts/src/JobScheduler.cpp mcts/include/JobScheduler.h
	g++ -c $(FLAGS) mcts/src/JobScheduler.cpp


TicTacToe: mcts.o JobScheduler.o examples/TicTacToe/main.cpp examples/TicTacToe/TicTacToe.cpp examples/TicTacToe/TicTacToe.h
	g++ -o $(TICTACTOE_EXE) $(FLAGS) examples/TicTacToe/main.cpp examples/TicTacToe/TicTacToe.cpp $(COMMON_OBJ)

sketch: mcts.o JobScheduler.o examples/sketch/main.cpp examples/sketch/sketch.cpp examples/sketch/sketch.h
	g++ -o $(SKETCH_EXE) $(FLAGS) examples/sketch/main.cpp examples/sketch/sketch.cpp $(COMMON_OBJ)

Quoridor: mcts.o JobScheduler.o examples/Quoridor/main.cpp examples/Quoridor/Quoridor.cpp examples/Quoridor/Quoridor.h
	g++ -o $(QUORIDOR_EXE) $(FLAGS) examples/Quoridor/main.cpp examples/Quoridor/Quoridor.cpp $(COMMON_OBJ)


clean:
	rm -f *.o
