#include "linearray.h"
#include <string.h>

linearray newarray()
{
    linearray l;
    l.linecount = 0;
    l.linealloc = 1;

    l.lines = (line*)malloc(sizeof(line));
    if (l.lines == NULL)
        exit(1);
    return l;
}

void add(line line, linearray* target)
{
    if (target->linecount + 1 >= target->linealloc)
    {
        target->lines = realloc(target->lines,2 * target->linealloc * sizeof(line));
        if (target->lines == NULL)
            exit(1);
        target->linealloc *= 2;
    }

    target->lines[target->linecount] = line;
    target->linecount ++;
}

void empty(linearray c)
{
    for (size_t i = 0; i < c.linecount; i++) clear(c.lines[i]);
    free(c.lines);
}

int nancomp(const void* a, const void* b)
{
    return (strcmp((char *)a,(char *)b));
}

int numcomp(const void* a, const void* b)
{
    long double* x = (long double*) a;
    long double* y = (long double*) b;
    if (*x < *y) return -1;
    if (*x > *y) return 1;
    return 0;
}


void sort(linearray* target)
{
    for (size_t i = 0; i < target->linecount; i++)
    {
        qsort(target->lines[i].nums, target->lines[i].numcount, sizeof(long double), numcomp);
        qsort(target->lines[i].nans, target->lines[i].nancount, sizeof(char*), nancomp);
    }
}