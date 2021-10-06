#include <iostream>
#include <vector>
#include <map>
#include <unordered_map>
#include <string>
#include <deque>

using std::cout;
using std::vector;
using std::map;
using std::unordered_map;
using std::string;
// używam dla łatwiejszego kopiowania zawartości vectora
using std::deque;
using std::endl;

// aliasy zapewne się zmienią, nazwy zmiennych też możemy chcieć zmienić
using zapalony_t = unordered_map<long long, bool>;
using synowie_t = unordered_map<long long, vector<long long> >
using ojcowie_t = map<long long, long long>
using wejscia_t = vector<long long>
using bramki_t = unordered_map<long long, char>


// funkcja dostaje do policzenia konkretny odwiedzany sygnał wyjściowy
// oraz typ bramki, która jest do niego przypisana i drzewo, z którego
// może odczytać, jakie są sygnały wchodzące do tej bramki. Potem
// w zależności od bramki wykonuje odpowiednie operacje logiczne
// i zapisuje je do drzewa.
void oblicz_bramke(zapalony_t &zapalony,
                   synowie_t &synowie,
                   char bramka, long long odwiedzany)
{
    switch (bramka)
    {
        case 'N':
            zapalony[odwiedzany] = !zapalony[synowie[odwiedzany][0]];
            break;
        case 'A':
            zapalony[odwiedzany] = true;
            for (size_t j = 0; j < synowie[odwiedzany].size(); j++)
            {
                zapalony[odwiedzany] &= zapalony[synowie[odwiedzany][j]];
            }
            break;
        case 'O':
            zapalony[odwiedzany] = false;
            for (size_t j = 0; j < synowie[odwiedzany].size(); j++)
            {
                zapalony[odwiedzany] |= zapalony[synowie[odwiedzany][j]];
            }
            break;
        case 'D':
            zapalony[odwiedzany] = true;
            for (size_t j = 0; j < synowie[odwiedzany].size(); j++)
            {
                zapalony[odwiedzany] &= !zapalony[synowie[odwiedzany][j]];
            }
            break;
        case 'R':
            zapalony[odwiedzany] = false;
            for (size_t j = 0; j < synowie[odwiedzany].size(); j++)
            {
                zapalony[odwiedzany] |= !zapalony[synowie[odwiedzany][j]];
            }
            break;
        case 'X':
            zapalony[odwiedzany] = zapalony[synowie[odwiedzany][0]] !=
                                   zapalony[synowie[odwiedzany][1]];
            break;
    }
}

// funkcja bierze maskę bitową i tworzy z niej zbiór zapalonych
// i zgaszonych sygnałów, który zapisuje w wyjściowej mapie
void maska_do_danych(size_t maska, zapalony_t &zapalony,
                     wejscia_t &wejscia, size_t m)
{
	// robimy pętlę od tyłu, żeby stworzyły nam się układy w kolejności
	// odpowiadającej kolejnym liczbom w systemie dwójkowym. 
	// warunek stopu tej pętli korzysta z faktu, że gdy zmniejszymy
	// zmienną typu size_t do zera i potem jeszcze o jeden, to otrzymamy
	// ulong_max, czyli -1
	size_t j = m-1;
    do
    {
        if (maska % 2 == 1)
            zapalony[wejscia[j]] = true;
        else
            zapalony[wejscia[j]] = false;
        maska /= 2;
        j--;
    } while (j != (size_t)-1);
}

// funkcja bierze drzewo i sprawdza każdy wierzchołek pod względem tego,
// czy wychodzi z jakiejś bramki czy może jest wierzchołkiem wejściowym.
// Wszystkie wierzchołki wejściowe zapisuje w strukturze wejscia.
void wygeneruj_wejscia(wejscia_t &wejscia,
                         ojcowie_t &ojcowie,
                         bramki_t &bramki)
{
    // tutaj ten for taki śmieszny dosyć, nie dość że przechodzi po
    // elementach mapy, to jeszcze je od razu pattern-matchuje
    // i tworzy w każdej iteracji dwie pomocnicze zmienne, nazwa i _.
    // tę drugą ignoruję, więc muszę dać [[maybe_unused]], żeby się
    // ani nie pruło, że używam pierwszą, ani nie pruło, że nie używam
    // drugiej.
    for ([[maybe_unused]] auto&&[nazwa, _]: ojcowie)
    {
        if (!bramki.contains(nazwa))
            wejscia.push_back(nazwa);
    }
}

// ogólna funkcja wypisująca wynik na podstawie drzewa. najpierw
// liczy wierzchołki wejściowe, a potem generuje kolejne możliwe
// stany zapalenia tych wierzchołków i na ich podstawie idąc bfs
// z liści w stronę korzenia drzewa liczy wszystkie pozostałe stany
// zapalenia, na koniec wszystkie te stany wypisując. 
void wypisz_wynik(synowie_t &synowie,
                  ojcowie_t &ojcowie,
                  bramki_t &bramki)
{
    
    wejscia_t wejscia;
    wygeneruj_wejscia(wejscia, ojcowie, bramki);
    // liczę, ile wierszy mam wypisać
    size_t m = wejscia.size(), rozmiar_wyjscia = 1;
    for (size_t i = 0; i < m; i++)
        rozmiar_wyjscia *= 2;
    
    for (size_t i = 0; i < rozmiar_wyjscia; i++)
    {
        zapalony_t zapalony;
        maska_do_danych(i, zapalony, wejscia, m);
        
        deque<long long> do_odwiedzenia(wejscia.begin(), wejscia.end());
        while (!do_odwiedzenia.empty())
        {
            long long odwiedzany = do_odwiedzenia.front();
            do_odwiedzenia.pop_front();
            if (bramki.contains(odwiedzany))
                oblicz_bramke(zapalony, synowie, bramki[odwiedzany],
                              odwiedzany);
            if (ojcowie.contains(odwiedzany))
                do_odwiedzenia.push_back(ojcowie[odwiedzany]);
        }
        
        for ([[maybe_unused]] auto&&[_, wartosc]: zapalony)
        {
            if (wartosc)
                cout << "1";
            else
                cout << "0";
        }
        cout << endl;
    }
}

// To będzie zapewne zadeklarowane wcześniej w kodzie, więc ja zrobiłem
// globalne, żeby się nie myliło z tymi moimi.
synowie_t synowie;
// ta mapa musi być ordered, żeby można było z niej w odpowiedniej
// kolejności wygenerować vector sygnałów wejściowych
ojcowie_t ojcowie;
bramki_t bramki;

int main()
{
    
    // Jak na razie w mainie jest szybka funkcja wczytująca dla debugu
    // Zmienną ign deklaruję po to, żeby się nie pruło o scanfy.
    [[maybe_unused]]size_t ign;
    int n;
    ign = scanf("%d", &n);
    for (int i = 0; i < n; i++)
    {
        long long x;
        int k;
        ign = scanf("%lld%d", &x, &k);
        wejscia_t nw;
        synowie[x] = nw;
        for (int j = 0; j < k; j++)
        {
            long long s;
            ign = scanf("%lld", &s);
            synowie[x].push_back(s);
            ojcowie[s] = x;
        }
        char c;
        ign = scanf(" %c", &c);
        bramki[x] = c;
    }
    
    // i wywołanie głównej funkcji wypisującej
    wypisz_wynik(synowie, ojcowie, bramki);
}
