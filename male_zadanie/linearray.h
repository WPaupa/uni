#ifndef MALE_ZADANIE_LINEARRAY_H
#define MALE_ZADANIE_LINEARRAY_H
#include <stdlib.h>
#include "line.h"

typedef struct
{
    size_t linecount, linealloc;
    line* lines;
} linearray;

linearray newarray();

void add(line line, linearray* target);
void empty(linearray c);
void sort(linearray target);


#endif //MALE_ZADANIE_LINEARRAY_H
