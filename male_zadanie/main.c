/* ZADANIE: similar_lines
 * AUTOR: Wojciech Paupa
 * Oczekiwana zlozonosc pamieciowa:
 * O(k), k - sumaryczna dlugosc wejscia
 * Oczekiwana zlozonosc obliczeniowa:
 * O(k log k)
 * Pomysl na rozwiazanie:
 * Program wczytuje kazdy wiersz
 * i rozdziela go na liczby i nieliczby.
 * Kazdy wiersz jest przechowywany jako
 * struktura, ktora trzyma tablice dynamiczne liczb oraz nieliczb
 * i swoj numer. Te wszystkie wiersze sa trzymane
 * w duzej dynamicznej tablicy. Kazdy z tych wierszy
 * sortujemy po jego liczbach i nieliczbach po to, zebysmy mogli
 * efektywnie porownywac ze soba pary wierszy.
 * Dzieki temu mozemy posortowac tablice wierszy.
 * Przechodzimy potem po posortowanej tablicy wierszy
 * i zliczamy wiersze, ktore sa takie same. Zapisujemy
 * ich numery do osobnej dynamicznej tablicy dwuwymiarowej,
 * ktora w kazdym wierszu zawiera zbior takich samych wierszy wejscia.
 * Sortujemy z osobna wiersze tablicy, a potem cala tablice
 * i otrzymujemy wynik.
 */

#define _GNU_SOURCE
#include "dynarray.h"
#include "output.h"
#include "process.h"
#include <ctype.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

//limity tego, co program uznaje za znak
#define MIN_CHAR 33
#define MAX_CHAR 126

//tablica ze wszystkimi znakami uznawanymi za bialy znak
const char *delims = " \t\v\f\r\n";

//tablica wartosci typu line
//ze wszystkimi wierszami wprowadzonymi do programu
array data;

//znak zgodnie z trescia jest poprawny,
//jesli ma kod od MIN_CHAR do MAX_CHAR albo jest bialym znakiem
bool isCharCorrect(char k)
{
    if (MIN_CHAR <= k && k <= MAX_CHAR)
        return true;

    size_t length = strlen(delims);
    for (size_t i = 0; i < length; i++)
        if (k == delims[i])
            return true;

    return false;
}

//ciag znakow jest poprawny, jesli wszystkie jego znaki sa poprawne
bool checkString(char *k, size_t size)
{
    for (size_t i = 0; i < size; i++)
        if (!isCharCorrect(k[i]))
            return false;

    return true;
}

//procedura splitWord dzieli wiersz na liczby i nieliczby,
//ktore zapisuje w obiekcie line dodanym do globalnej tablicy.
//w przypadku, w ktorym w wierszu sa same biale znaki,
//procedura nie doda nic do tablicy
void splitWord(char *input, int number)
{
    char *word = strtok(input, delims);
    if (word != NULL)
    {
        line processed = newLine(number);
        while (word != NULL)
        {
            process(word, &processed);
            word = strtok(NULL, delims);
        }
        addItem(&data, &processed);
    }
    free(word);
}

//funkcja read sprawdza, czy wejscie jest poprawne,
//jesli tak, to prosi o dodanie wiersza do globalnej tablicy
//jesli znalazla bledny wiersz, wypisuje error,
//jesli znalazla koniec pliku, zwraca falsz

//ta funkcja korzysta ze ze zmiennych input i size,
//ktore deklaruje za pierwszym wywolaniem (static)
//i potem za kazdym razem nadpisuje getline'em,
//ewentualnie zwiekszajac pamiec.
//to zmniejsza sumaryczna liczbe alokacji.

bool read(size_t number)
{
    static char *input = NULL;
    static size_t size = 0;

    errno = 0;
    int result = getline(&input, &size, stdin);

    //co jesli nie udalo sie zaalokowac pamieci
    if (errno == ENOMEM)
        exit(1);

    //co jesli natrafilismy na koniec pliku
    if (result < 0)
    {
        free(input);
        return false;
    }

    size_t length = result;

    //input[0] jest dobrze zdefiniowane
    //nawet dla length = 0 (wtedy to jest \0)
    if (input[0] == '#')
        return true;

    if (!checkString(input, length))
    {
        fprintf(stderr, "ERROR %zu\n", number);
        return true;
    }

    for (size_t i = 0; i < length; i++)
        input[i] = (char)tolower(input[i]);

    splitWord(input, number);
    return true;
}

int main()
{
    //inicjalizacja glownej tablicy z wierszami wejscia
    data = newArray(sizeof(line));

    //petla wywolujaca funkcje read z kolejnymi liczbami wierszy.
    //zatrzymuje sie, gdy read znajdzie koniec pliku
    int i = 0;
    //ta instrukcja moze sie wydawac brzydka, ale wedlug mnie jest urocza
    while (read(++i));

    //robi faktyczne obliczenia i wypisuje wynik
    output(data);

    //zwolnienie calej pamieci w tablicy data
    for (size_t it = 0; it < data.itemCount; it++)
    {
        line *x = (line *)at(data, it);
        clearLine(*x);
    }
    free(data.items);

    return 0;
}
