#define  _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "process.h"
#include <ctype.h>
#include "linearray.h"

const char* delims = " \t\v\f\r\n";
linearray data;

bool correct(char k)
{
    if (33 <= k && k <= 126) return true;
    switch (k)
    {
        case 32:
        case 9:
        case 10:
        case 11:
        case 12:
        case 13:
            return true;
        default:
            return false;
    }
}

bool checkstr(char* k, size_t size)
{
    for (size_t i = 0; i < size; i++)
        if (!correct(k[i]))
        {
            //printf("error at %zu, character %d\n",i,k[i]);
            return false;
        }
    return true;
}

void debug(line processed)
{
    printf("%zu USED, %zu ALLOCATED NANS\n",processed.nancount,processed.nanalloc);
    for (size_t i = 0; i < processed.nancount; i++)
    {
        printf("%s\n",processed.nans[i]);
    }
    printf("%zu USED, %zu ALLOCATED NUMS\n",processed.numcount,processed.numalloc);
    for (size_t i = 0; i < processed.numcount; i++)
    {
        printf("%Lf\n",processed.nums[i]);
    }

    puts("");
}

bool read(size_t number)
{
    char *input = NULL;
    size_t size = 0;
    int wyn = getline(&input, &size, stdin);
    if (ferror(stdin)) exit(1);
    if (wyn<0)
    {
        free(input);
        return false;
    }
    if(strcmp("debug\n",input) == 0)
    {
        free(input);
        return false;
    }
    size_t length = wyn;
    line processed = newline(number);
    for (size_t i = 0; i < length; i++)
        input[i] = (char)tolower(input[i]);
    if (input[0]!='#')
    {

        if (!checkstr(input, length))
        {
            clear(processed);
            fprintf(stderr, "ERROR %zu\n", number);
        }
        else
        {
            char *word = strtok(input, delims);
            if (word != NULL)
            {
                while (word != NULL)
                {
                    process(word, &processed);
                    word = strtok(NULL, delims);
                }
                add(processed, &data);
            } else clear(processed);
            free(word);
        }
    } else clear(processed);
    free(input);
    return true;
}

void out()
{
    bool odw[data.linecount];
    memset(odw,false,data.linecount);
    for (size_t i = 0; i < data.linecount; i++)
    {
        if (odw[i]) continue;
        printf("%zu",data.lines[i].number);
        for (size_t j = i+1; j < data.linecount; j++)
        {
            if (equal(data.lines[i],data.lines[j]))
            {
                odw[j] = true;
                printf(" %zu",data.lines[j].number);
            }
        }
        puts("");
    }
}

int main()
{
    data = newarray();
    int i = 0;
    while(read(++i));
    sort(data);
    for(size_t it = 0; it < data.linecount; it++)
    {
        //printf("==== LINE NUMBER %zu =====\n",data.lines[it].number);
        //debug(data.lines[it]);
    }
    out();
    empty(data);
    return 0;
}
