default : run

program.cpp program.hpp : program.rkt
	mglc --cxx $<
	touch program.cpp program.hpp

program.o : program.cpp program.hpp program_config.hpp ../../erda.hpp
	g++ -c -Wall -std=c++0x -iquote../.. program.cpp

main.o : main.cpp program.hpp program_config.hpp ../../erda.hpp
	g++ -c -Wall -std=c++0x -iquote../.. main.cpp

main : main.o program.o
	g++ $^ -o $@

run : main
	./main

clean :
	-rm main *.o program.cpp program.hpp *~
