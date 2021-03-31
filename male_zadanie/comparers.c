#include "comparers.h"
#include "dynarray.h"
#include "line.h"
#include <string.h>

//kazdy komparator dziala wedlug tego samego schematu:
//castujemy przekazane wskazniki na wskazniki na dany typ,
//a potem uzywamy prostej funkcji do porownania ich wartosci

//jesli pierwszy int jest wiekszy, to roznica jest dodatnia, jesli drugi - to ujemna itd.
int intCompare(const void *a, const void *b)
{
    int *x = (int *)a;
    int *y = (int *)b;

    return (*x - *y);
}

int intArrayCompare(const void *a, const void *b)
{
    array *x = (array *)a;
    array *y = (array *)b;

    return arrayCompare(*x, *y, intCompare);
}

int stringCompare(const void *a, const void *b)
{
    char **x = (char **)a;
    char **y = (char **)b;

    return strcmp(*x, *y);
}

//nie mozemy po prostu zwrocic roznicy, bo ona jest long doublem, a potrzebujemy inta. dlatego manualnie porownujemy x i y
int floatCompare(const void *a, const void *b)
{
    long double *x = (long double *)a;
    long double *y = (long double *)b;

    if (*x > *y)
        return 1;

    if (*x < *y)
        return -1;

    return 0;
}

//nie ma znaczenia, czy najpierw porownujemy nieliczby, czy liczby. wazne, zeby to bylo spojne.
//w drugim warunku moglibysmy po prostu od razu zwrocic numcompare, ale tak wydaje sie przejrzysciej
int lineCompare(const void *a, const void *b)
{
    line *x = (line *)a;
    line *y = (line *)b;

    int nancompare = arrayCompare(x->nans, y->nans, stringCompare);
    if (nancompare != 0)
        return nancompare;

    int numcompare = arrayCompare(x->nums, y->nums, floatCompare);
    if (numcompare != 0)
        return numcompare;

    return 0;
}
