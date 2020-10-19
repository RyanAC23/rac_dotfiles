all: main clean

main: emacs.o fact.o combination.o
	g++ -o main emacs.o fact.o combination.o

main.o: emacs.cpp fact.h
	g++ -c emacs.cpp

fact.o: fact.cpp fact.h
	g++ -c fact.cpp

combination.o: combination.cpp fact.h
	g++ -c combination.cpp

clean:
	rm *.o main
