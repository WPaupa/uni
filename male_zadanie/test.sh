#!/bin/bash

# tymczasowy folder na testy
tempdir=$(mktemp -d /tmp/simlines_XXX)

# przetwarzanie testow
for f in "$2"/*.in
do
    # odzyskiwanie nazwy testow
    fname=$(basename "$f")
    tname="${fname%.in}"
    echo "Test: $tname"

    # testy poprawnosciowe
    time ./"$1"<"$f" > "$tempdir/$tname.out" 2> "$tempdir/$tname.err"
    if [[ $(diff -b -B "$tempdir/$tname.out" "${f%in}out") ]]; then
        echo "OUT ERROR"
    else 
        echo "OUT OK"
    fi
    if [[ $(diff -b -B "$tempdir/$tname.err" "${f%in}err") ]]; then
	echo "ERR ERROR"
    else 
        echo "ERR OK"
    fi
 
    # testy pamieciowe
    # grepowanie po logach valgrinda to glupia metoda,
    # ale na potrzeby tego zadania sie nie psuje
    valgrind --log-file="$tempdir/$tname.log" ./"$1"<"$f" > "$tempdir/$tname.out" 2> "$tempdir/$tname.err" 
    if ! grep -q "All heap blocks were freed" "$tempdir/$tname.log"; then
	echo "MEMORY PROBLEMS"
    else
	echo "MEMORY OK"
    fi

    # sprawdzam, czy valgrind znalazl problemy w dzialaniu programu
    if ! grep -q "ERROR SUMMARY: 0 errors" "$tempdir/$tname.log"; then
        echo "VALGRIND FOUND ERRORS"
    fi

    # sprzatanie po sobie
    rm "$tempdir/$tname.log"
    rm "$tempdir/$tname.out"
    rm "$tempdir/$tname.err"
done

# czyscimy folder
rm -r "$tempdir"
