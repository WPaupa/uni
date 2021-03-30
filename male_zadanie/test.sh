#!/bin/bash
for f in "$2"/*.in
do
    echo Test: "$(basename "$f")"
    time ./"$1"<"$f" > "${f%.in}_prog.out" 2> "${f%.in}_prog.err"
    if [[ $(diff "${f%.in}_prog.out" "${f%in}out") ]]; then
        echo "OUT ERROR"
    else 
        echo "OUT OK"
    fi
    if [[ $(diff "${f%.in}_prog.err" "${f%in}err") ]]; then
	echo "ERR ERROR"
    else 
        echo "ERR OK"
    fi

    valgrind --log-file="${f%.in}_valgrind.out" ./"$1"<"$f" > "${f%.in}_prog.out" 2> "${f%.in}_prog.err" 
    if ! grep -q "All heap blocks were freed" "${f%.in}_valgrind.out"; then
	echo "MEMORY PROBLEMS"
    else
	echo "MEMORY OK"
    fi

    # rm "${f%.in}_valgrind.out"
    rm "${f%.in}_prog.out"
    rm "${f%.in}_prog.err"
done
