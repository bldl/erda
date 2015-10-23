default : run

program.cpp program.hpp : program.rkt
	mglc --cxx $<

%.o : %.cpp program.hpp program_config.hpp ../../erda.hpp
	clang++ -c -Wall -I../.. -std=c++14 $<

main : main.o program.o overflow.o
	clang++ $^ -o $@

run : main
	./main

clean :
	-rm main *.o program.cpp program.hpp *~
