#ifndef MALE_ZADANIE_LINE_H
#define MALE_ZADANIE_LINE_H
#include <stdlib.h>
#include "dynarray.h"

//implementacja struktury wiersza -
//moze zawierac dowolna liczbe liczb i nieliczb
//oraz ma przypisany sobie numer
typedef struct
{
    array nums;
    array nans;

    size_t number;
} line;

//nowy, pusty wiersz o danym przypisanym numerze
line newLine(size_t number);

//dodaja odpowiednio liczbe i nieliczbe do tablic z wiersza
//nalezy pamietac, ze addNan nie kopiuje
//wskaznika na slowo, ale kopiuje cale slowo
void addNum(long double word, line* line);
void addNan(char* word, line* line);

//dealokuje pamiec przyznana wierszowi
void clearLine(line line);

#endif //MALE_ZADANIE_LINE_H