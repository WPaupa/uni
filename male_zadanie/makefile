CC       = gcc
CFLAGS   = -Wall -Wextra -std=c11 -O2
OBJECTS  = main.o line.o process.o output.o dynarray.o comparers.o

.PHONY: all clean

all: similar_lines

similar_lines: $(OBJECTS)
	$(CC) -o similar_lines $(CFLAGS) $(OBJECTS)
comparers.o: comparers.c comparers.h dynarray.h line.h
dynarray.o: dynarray.c dynarray.h
line.o: line.c line.h dynarray.h
main.o: main.c dynarray.h output.h process.h line.h
output.o: output.c output.h dynarray.h comparers.h line.h
process.o: process.c process.h line.h dynarray.h

clean:
	-rm -f *.o similar_lines
