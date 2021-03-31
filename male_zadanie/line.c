#include "line.h"
#include <stdlib.h>
#include <string.h>

//na poczatku wiersz ma tylko swoj numer
//i sklada sie z dwoch pustych tablic
line newLine(size_t number)
{
    line l;

    l.nums = newArray(sizeof(long double));
    l.nans = newArray(sizeof(char *));
    l.number = number;

    return l;
}

//dodanie liczby do wiersza to po prostu
//dopisanie jej do tablicy w tym wierszu
void addNum(long double word, line *line) 
{ 
    addItem(&line->nums, &word); 
}

//zeby dodac nieliczbe, potrzebujemy przekopiowac cala zawartosc wskaznika.
//w tym celu musimy zaalokowac dodatkowa pamiec na przekopiowane slowo
//nie mozna zapomniec o dodaniu 1 do strlen, zeby nie bylo problemu z \0
void addNan(char *word, line *line)
{
    size_t length = strlen(word) + 1;

    char *wordCopy = (char *)malloc(length * sizeof(char));
    if (wordCopy == NULL)
        exit(1);

    strcpy(wordCopy, word);
    addItem(&line->nans, &wordCopy);
}

//zanim mozemy zwolnic tablice z nieliczbami,
//musimy po kolei zwolnic kazda nieliczbe (bo to tez tablice)
//tablice z liczbami mozemy zwolnic bez problemu
void clearLine(line line)
{
    for (size_t i = 0; i < line.nans.itemCount; i++)
    {
        char **pointerToWord = (char **)at(line.nans, i);
        free(*pointerToWord);
    }

    free(line.nums.items);
    free(line.nans.items);
}
