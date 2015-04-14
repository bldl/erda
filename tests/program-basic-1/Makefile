default : run

erda.o : ../../erda.cpp ../../erda.hpp
	g++ -c -Wall -std=c++0x -iquote../.. $< -o $@

program.cpp program.hpp : program.rkt
	mglc --cxx $<

program.o : program.cpp program.hpp program_config.hpp ../../erda.hpp
	g++ -c -Wall -std=c++0x -iquote../.. $<

main.o : main.cpp program.hpp program_config.hpp ../../erda.hpp
	g++ -c -Wall -std=c++0x -iquote../.. $<

main : main.o program.o erda.o
	g++ $^ -o $@

run : main
	./main

clean :
	-rm main *.o program.cpp program.hpp *~
