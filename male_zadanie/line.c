#include <string.h>
#include "line.h"



line newline(size_t number)
{
    line l;
    l.nums = (long double*)malloc(sizeof(long double));
    if (l.nums == NULL)
        exit(1);
    l.numcount = 0; l.numalloc = 1;

    l.nans = (char**)malloc(sizeof(char*));
    if (l.nans == NULL)
        exit(1);
    l.nancount = 0; l.nanalloc = 1;


    l.number = number;

    return l;
}


void addnum(long double num, line* line)
{
    if (line->numcount + 1 >= line->numalloc)
    {
        line->nums = realloc(line->nums,2 * line->numalloc * sizeof(long double));
        if (line->nums == NULL)
            exit(1);
        line->numalloc *= 2;
    }

    line->nums[line->numcount] = num;
    line->numcount ++;
}

void addnan(char* word, line* line)
{
    size_t length = strlen(word)+1;
    if (line->nancount + 1 >= line->nanalloc)
    {
        line->nans = realloc(line->nans,2 * line->nanalloc * sizeof(char*));
        if (line->nans == NULL)
            exit(1);
        line->nanalloc *= 2;
    }
    line->nans[line->nancount]=(char*)malloc(length*sizeof(char));
    if (line->nans[line->nancount] == NULL)
        exit(1);
    strcpy(line->nans[line->nancount],word);
    line->nancount ++;
}


void clear(line line)
{
    free(line.nums);
    for (size_t i = 0; i < line.nancount; i++) free(line.nans[i]);
    free(line.nans);
}

bool equal(line line1, line line2)
{
    if (line1.numcount != line2.numcount) return false;
    if (line1.nancount != line2.nancount) return false;

    for (size_t i = 0; i < line1.numcount; i++) if (line1.nums[i] != line2.nums[i]) return false;
    for (size_t i = 0; i < line1.nancount; i++) if (strcmp(line1.nans[i],line2.nans[i]) != 0) return false;

    return true;
}