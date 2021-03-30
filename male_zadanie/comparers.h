#ifndef MALE_ZADANIE_COMPARERS_H
#define MALE_ZADANIE_COMPARERS_H

//potoczna nazwa typu + Compare = funkcja porownujaca dwie zmienne tego typu

int intCompare(const void *a, const void *b);
int intArrayCompare(const void *a, const void *b);
int floatCompare(const void *a, const void *b);
int stringCompare(const void *a, const void *b);
int lineCompare(const void *a, const void *b);

#endif //MALE_ZADANIE_COMPARERS_H
