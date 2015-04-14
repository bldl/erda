default : run

program.cpp program.hpp : program.rkt
	mglc --cxx $<
	touch program.cpp program.hpp

%.o : %.cpp program.hpp program_config.hpp ../../erda.hpp
	clang++ -c -Wall -std=c++14 -iquote../.. $<

main : main.o program.o overflow.o
	clang++ $^ -o $@

run : main
	./main

clean :
	-rm main *.o program.cpp program.hpp *~
