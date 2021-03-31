#ifndef MALE_ZADANIE_PROCESS_H
#define MALE_ZADANIE_PROCESS_H
#include "line.h"

//funkcja process rozpoznaje,
//czy ciag znakow word to liczba, czy nieliczba
//oraz w jakim systemie jest zapisany.
//potem dodaje go do *line.
//liczby poza zakresem traktujemy tak,
//jakby byly rowne zakresowi (tak jak strtoX)
void process(char *word, line *line);

#endif //MALE_ZADANIE_PROCESS_H
