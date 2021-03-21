CC       = gcc
CPPFLAGS =
CFLAGS   = -Wall -Wextra -std=c11 -O2
LDFLAGS  =

.PHONY: all clean

all: similar_lines

similar_lines: main.o line.o linearray.o process.o
	$(CC) -o similar_lines $(CFLAGS) main.o line.o linearray.o process.o
line.o: line.c line.h
linearray.o: linearray.c linearray.h line.h
process.o: process.c process.h line.h
main.o: main.c line.h linearray.h

clean:
	rm -f *.o similar_lines
