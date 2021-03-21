cd "$(dirname "$0")"

echo Kompilowanie

make all

for f in tests/*.in
do
    echo Przetwarzam: $(basename "$f")
    time(./similar_lines<"$f" > "${f%in}out" 2> "${f%in}err")
done


