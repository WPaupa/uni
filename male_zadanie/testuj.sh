cd "$(dirname "$0")"

echo Kompilowanie

make all

for f in tests/*.in
do
    echo Przetwarzam: $(basename "$f")
    time(./similar_lines<"$f" > "${f%in}out" 2> "${f%in}err")
done

for f in difftests/*.in
do
    echo Przetwarzam: $(basename "$f")
    time(./similar_lines<"$f" > "${f%.in}_prog.out" 2> "${f%.in}_prog.err")
    diff "${f%.in}_prog.out" "${f%in}out"
    diff "${f%.in}_prog.err" "${f%in}err"
done
