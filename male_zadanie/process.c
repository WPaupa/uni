#include "process.h"
#include <stdbool.h>
#include <string.h>

//slowo jest liczba w systemie osemkowym, jesli zaczyna sie od 0 i ma cyfry od 0 do 7
static bool base8(const char *word, size_t length)
{
    if (word[0] != '0')
        return false;

    for (size_t i = 0; i < length; i++)
        if ('0' > word[i] || word[i] > '7')
            return false;

    return true;
}

//slowo jest liczba w systemie osemkowym, jesli zaczyna sie od 0x i ma cyfry od 0 do 9 i od a do f
static bool base16(const char *word, size_t length)
{
    if (length < 1 || word[0] != '0' || word[1] != 'x')
        return false;

    for (size_t i = 2; i < length; i++)
        if (('0' > word[i] || word[i] > '9') &&
            ('a' > word[i] || word[i] > 'f'))
            return false;

    return true;
}

//slowo jest dodatnia liczba w systemie dziesiatkowym, jesli zaczyna sie od + lub nie
//i poza tym sklada sie tylko z cyfr od 0 do 9 i ma choc jedna cyfre
static bool base10Positive(const char *word, size_t length)
{
    size_t firstDigitPosition = 0;
    if (word[0] == '+')
        firstDigitPosition = 1;

    if (length < firstDigitPosition + 1)
        return false;

    for (size_t i = firstDigitPosition; i < length; i++)
        if ('0' > word[i] || word[i] > '9')
            return false;

    return true;
}

//slowo jest ujemna liczba w systemie dziesiatkowym, jesli zaczyna sie od -
//i sklada sie tylko z cyfr od 0 do 9 i ma choc jedna cyfre
static bool base10Negative(const char *word, size_t length)
{
    if (word[0] != '-')
        return false;

    if (length < 2)
        return false;

    for (size_t i = 1; i < length; i++)
        if ('0' > word[i] || word[i] > '9')
            return false;

    return true;
}

//zeby slowo bylo liczba zmiennoprzecinkowa, moze:
//    - byc slowem inf, +inf lub -inf (nan liczymy jako nieliczbe)
//    - zawierac maksymalnie jedna kropke w srodku
//    - zawierac maksymalnie jedno e w srodku, przy czym przed i po tym e musza wystepowac liczby, przed e moze
//      sie pojawic kropka, ale po e - nie
//    - zawierac + lub - na poczatku lub bezposrednio po literce e
//    - zawierac co najmniej jedna cyfre, a jesli zawiera literke e, musi zawierac co najmniej jedna cyfre
//      przed i po literce e
// dlatego trzymam boole, ktore mowia o tym, czy dany typ symbolu juz wystapil / moze wystapic
// i za kazdym znakiem sprawdzam powyzsze warunki
static bool floatNum(const char *word, size_t length)
{
    if ((!strcmp(word,  "inf")  ||
         !strcmp(word, "-inf")) ||
         !strcmp(word, "+inf"))
        return true;

    //czy moga wystapic dane znaki
    bool decimalPoint = true, exponent = true, sign = true;

    //czy juz wystapily liczby (od poczatku slowa lub od e)
    bool numbers = false;

    for (size_t i = 0; i < length; i++)
    {
        if (word[i] == 'e')
        {
            if (!exponent || !numbers)
                return false;

            exponent = false;
            sign = true;
            decimalPoint = false;
            numbers = false;
        }
        else if (word[i] == '.')
        {
            if (!decimalPoint)
                return false;

            sign = false;
            decimalPoint = false;
        }
        else if (word[i] == '+' || word[i] == '-')
        {
            if (!sign)
                return false;

            sign = false;
        }
        else if (word[i] >= '0' && word[i] <= '9')
        {
            sign = false;
            numbers = true;
        }
        else
            return false;
    }

    if (!numbers)
        return false;

    return true;
}

//po kolei sprawdzamy, czy word jest liczba w kazdym systemie
//trzeba osobno sprawdzic dodatnie i ujemne liczby dziesietne, bo dodatnie wykraczaja poza zakres
//signed long long, a ujemne wykraczaja poza zakres unsigned long long (oczywiscie)
//jesli slowo nie pasuje do zadnego warunku liczby, dodajemy je jako nieliczbe
void process(char *word, line *line)
{
    size_t length = strlen(word);

    if (base8(word, length))
    {
        long double x = (long double)strtoull(word, NULL, 8);
        addNum(x, line);

        return;
    }

    if (base16(word, length))
    {
        long double x = (long double)strtoull(word, NULL, 16);
        addNum(x, line);

        return;
    }

    if (base10Positive(word, length))
    {
        long double x = (long double)strtoull(word, NULL, 10);
        addNum(x, line);

        return;
    }

    if (base10Negative(word, length))
    {
        long double x = (long double)strtoll(word, NULL, 10);
        addNum(x, line);

        return;
    }

    if (floatNum(word, length))
    {
        long double x = strtold(word, NULL);
        addNum(x, line);

        return;
    }

    addNan(word, line);
}
