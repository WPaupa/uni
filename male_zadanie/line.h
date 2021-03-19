#ifndef MALE_ZADANIE_LINE_H
#define MALE_ZADANIE_LINE_H
#include <stdlib.h>
#include <stdbool.h>

typedef struct
{
    long double* nums;
    char** nans;

    size_t numcount, numalloc;
    size_t nancount, nanalloc;
    size_t number;
} line;

line newline(size_t number);
void addnum(long double num, line* line);
void addnan(char* word, line* line);
void clear(line line);
bool equal(line line1, line line2);

#endif //MALE_ZADANIE_LINE_H
