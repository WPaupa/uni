#include "dynarray.h"
#include <stdlib.h>
#include <string.h>
#define INITIAL_ARRAY_SIZE 4

//na poczatek alokujemy jedno miejsce w pamieci i nie zapelniamy zadnych komorek
array newArray(size_t size)
{
    array a;

    a.itemsAllocated = INITIAL_ARRAY_SIZE;
    a.itemCount = 0;
    a.itemSize = size;
    a.items = malloc(size * INITIAL_ARRAY_SIZE);

    if (a.items == NULL)
        exit(1);

    return a;
}

//kazde przesuniecie sie do przodu o jedna komorke odpowiada zwiekszeniu wskaznika o target.itemSize
void *at(array target, size_t pos)
{
    return target.items + pos * target.itemSize;
}

//gdy skonczy sie miejsce w pamieci, alokujemy dwa razy tyle, ile do tej pory mielismy
//w ten sposob sumaryczny czas alokacji nie przekracza 2*n;
//funkcja dostaje wskaznik na element, ktory ma byc dodany, wiec musi uzyc memcpy, zeby dane pod tym wskaznikiem
//przekopiowac do tablicy
void addItem(array *target, void *item)
{
    if (target->itemCount + 1 >= target->itemsAllocated)
    {
        size_t newSize = 2 * target->itemsAllocated * target->itemSize;
        target->items = realloc(target->items, newSize);

        if (target->items == NULL)
            exit(1);

        target->itemsAllocated *= 2;
    }

    memcpy(at(*target, target->itemCount), item, target->itemSize);
    target->itemCount++;
}

//arrayCompare porownuje tablice "leksykograficznie", tzn. najpierw po kolejnych elementach tablicy w sensie funkcji comp,
//a potem dopiero po dlugosci.
int arrayCompare(array array1, array array2, COMP_FUNC comp)
{
    size_t size = array1.itemSize;

    //program nie powinien porownywac dwoch tablic, w ktorych itemSize jest inny
    //mimo to na wszelki wypadek warto dodac takie sprawdzenie
    if (size < array2.itemSize)
        return -1;
    if (size > array2.itemSize)
        return 1;

    for (size_t i = 0; i < array1.itemCount; i++)
    {
        if (i >= array2.itemCount)
            return 1;

        int x = comp(at(array1, i), at(array2, i));
        if (x != 0)
            return x;
    }

    if (array1.itemCount < array2.itemCount)
        return -1;

    return 0;
}
