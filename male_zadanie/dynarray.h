#ifndef MALE_ZADANIE_DYNARRAY_H
#define MALE_ZADANIE_DYNARRAY_H
#include <stdlib.h>

//wszystkie funkcje zwiazane z tablicami dynamicznymi, ktore mozna zaimplementowac polimorficznie

//typ COMP_FUNC to wskaznik na funkcje, ktora przyjmuje dwa wskazniki i porownuje wartosci pod tymi wskaznikami,
//zwracajac cos dodatniego, ujemnego lub zero (jesli sa rowne)
typedef int (*COMP_FUNC)(const void *, const void *);

//implementacja tablicy dynamicznej
//itemCount to liczba zapelnionych komorek, itemsAllocated to liczba zaalokowanych,
//itemSize to wielkosc jednej komorki w bajtach
typedef struct
{
    void *items;
    size_t itemCount, itemsAllocated;
    size_t itemSize;
} array;

//poprawne uzycie: newArray(sizeof(typ)) - tworzy tablice elementow danego typu
array newArray(size_t size);

//zwraca wskaznik na element na pozycji pos w tablicy target
void *at(array target, size_t pos);

//dodaje element item do tablicy target
void addItem(array *target, void *item);

//tak jak funkcje w comparers.c, porownuje dwie tablice zgodnie z funkcja comp
int arrayCompare(array array1, array array2, COMP_FUNC comp);

#endif //MALE_ZADANIE_DYNARRAY_H
