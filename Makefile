all: sbasic
sbasic: sbasic.o linalg.o liboffice.o ; g++ -std=c++17 -O2 -Wall -Wextra sbasic.o linalg.o liboffice.o -o sbasic -s -static -static-libgcc -static-libstdc++
sbasic.o: sbasic.cpp ; g++ -std=c++17 -O2 -Wall -Wextra -c sbasic.cpp -o sbasic.o
linalg.o: linalg.cpp ; g++ -std=c++17 -O2 -Wall -Wextra -c linalg.cpp -o linalg.o
liboffice.o: liboffice.cpp ; g++ -std=c++17 -O2 -Wall -Wextra -c liboffice.cpp -o liboffice.o
clean: ; rm -f sbasic.o linalg.o liboffice.o sbasic