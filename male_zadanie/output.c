#include "output.h"
#include "comparers.h"
#include "dynarray.h"
#include "line.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

//dwuwymiarowa tablica zawierajaca wyjscie
//kazdy wiersz to osobna tablica liczb
//odpowiadajaca wierszowi wyjscia
array outputLines;

//funkcja sortujaca po kolei tablice liczb i nieliczb
//w kazdym wierszu wejscia.
//posortowane wiersze mozemy porownywac "leksykograficznie", bo
//jesli dwa posortowane wiersze maja ten sam
//pierwszy element, drugi element itd.,
//to skladaja sie z tych samych liczb i nieliczb
static void sortLines(array target)
{
    for (size_t i = 0; i < target.itemCount; i++)
    {
        line *x = (line *)at(target, i);

        int* items = x->nums.items;
        size_t itemCount = x->nums.itemCount;
        qsort(items, itemCount, sizeof(long double), floatCompare);

        items = x->nans.items;
        itemCount = x->nans.itemCount;
        qsort(items, itemCount, sizeof(char *), stringCompare);
    }
}

//funkcja, ktora na podstawie posortowanej tablicy posortowanych wierszy
//tworzy tablice outputLines.
//porownuje ona kolejne linie
//i jedzie az nie natrafi na jakas linie,
//ktora jest inna niz obecnie porownywane.
//wtedy numery wszystkich takich samych linii
//zapisuje do jednego wiersza.
//te wiersze funkcja zapisuje do tablicy outputLines,
//ktora po wywolaniu tej funkcji powinna zawierac
//poprawna odpowiedz w niepoprawnej kolejnosci.
static void buildOutput(array data)
{
    array current = newArray(sizeof(int));
    line *x = (line *)at(data, 0);
    addItem(&current, &x->number);

    for (size_t i = 1; i < data.itemCount; i++)
    {
        if (lineCompare(at(data, i), at(data, i - 1)) != 0)
        {
            addItem(&outputLines, &current);
            current = newArray(sizeof(int));
        }
        x = (line *)at(data, i);
        addItem(&current, &x->number);
    }
    addItem(&outputLines, &current);
}

//sortOutput sortuje kazdy wiersz
//i potem sortuje tablice wierszy "leksykograficznie", czyli
//de facto po ich najmniejszych elementach.
//dzieki temu wiersze beda posortowane tak, jak powinny byc wg zadania
static void sortOutput()
{
    for (size_t N = 0; N < outputLines.itemCount; N++)
    {
        array *lineN = (array *)at(outputLines, N);

        int* items = (*lineN).items;
        size_t itemCount = (*lineN).itemCount;
        qsort(items, itemCount, sizeof(int), intCompare);
    }

    array* items = outputLines.items;
    size_t itemCount = outputLines.itemCount;
    qsort(items, itemCount, sizeof(array), intArrayCompare);
}

//za kazdym wypisaniem wiersza tablicy czyscimy ten wiersz,
//na koniec czyscimy cala tablice.
//zeby nie wypisywac dodatkowych spacji,
//ostatni element wiersza wypisujemy osobno (bez spacji na koncu)
static void printAndCleanArray()
{
    for (size_t i = 0; i < outputLines.itemCount; i++)
    {
        array *output = (array *)at(outputLines, i);

        for (size_t j = 0; j < output->itemCount - 1; j++)
        {
            int *number = at(*output, j);
            printf("%d ", *number);
        }

        if (output->itemCount > 0)
        {
            int *number = at(*output, output->itemCount - 1);
            printf("%d", *number);
        }

        puts("");

        free(output->items);
    }

    free(outputLines.items);
}

//funkcja output sortuje wiersze wejscia oraz tablice wierszy wejscia,
//a potem te posortowana tablice przekazuje
//do powyzszych funkcji, ktore generuja wyjscie i je wypisuja.
void output(array data)
{
    if (data.itemCount == 0)
        return;

    outputLines = newArray(sizeof(array));

    sortLines(data);
    qsort(data.items, data.itemCount, sizeof(line), lineCompare);

    buildOutput(data);

    sortOutput();

    printAndCleanArray();
}
