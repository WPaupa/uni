#include <bits/stdc++.h>

using std::cout;
using std::vector;
using std::map;
using std::unordered_map;
using std::string;
// dla łatwiejszego kopiowania
using std::deque;
using std::endl;

unordered_map<long long,vector<long long> > synowie;
unordered_map<long long,long long> ojcowie;
unordered_map<long long, char> bramki;

int main() {
	
	/*int n; scanf("%d",&n);
	for (int i = 0; i < n; i++) {
		long long x; int k; scanf("%lld%d",&x,&k);
		vector<long long> nw; synowie[x] = nw;
		for (int j = 0; j < k; j++) {
			long long s; scanf("%lld",&s); synowie[x].push_back(s);
			ojcowie[s] = x;
		}
		char c; scanf(" %c",&c); bramki[x] = c;
	}*/
	
	vector<long long> wejscia;
	for ([[maybe_unused]] auto&& [nazwa,_] : ojcowie) {
		if (!bramki.contains(nazwa))
			wejscia.push_back(nazwa);
	}
	
	size_t m = wejscia.size();
	size_t rozmiar_wyjscia = 1;
	for (size_t i = 0; i < m; i++)
		rozmiar_wyjscia *= 2;
	
	for (size_t i = 0; i < rozmiar_wyjscia; i++) {
		map<long long, bool> zapalony;
		
		size_t maska = i;
		for (size_t j = 0; j < m; j++) {
			if (maska % 2 == 1)
				zapalony[wejscia[j]] = true;
			else
				zapalony[wejscia[j]] = false;
			maska /= 2;
		}
		
		deque<long long> do_odwiedzenia(wejscia.begin(),wejscia.end());
		while (!do_odwiedzenia.empty()) {
			long long odwiedzany = do_odwiedzenia.front();
			do_odwiedzenia.pop_front();
			if (bramki.contains(odwiedzany)) {
				switch(bramki[odwiedzany]) {
					case 'N':
						zapalony[odwiedzany] = !zapalony[synowie[odwiedzany][0]];
						break;
					case 'A':
						zapalony[odwiedzany] = true;
						for (size_t j = 0; j < synowie[odwiedzany].size(); j++) {
							zapalony[odwiedzany] &= zapalony[synowie[odwiedzany][j]];
						}
						break;
					case 'O':
						zapalony[odwiedzany] = false;
						for (size_t j = 0; j < synowie[odwiedzany].size(); j++) {
							zapalony[odwiedzany] |= zapalony[synowie[odwiedzany][j]];
						}
						break;
					case 'D':
						zapalony[odwiedzany] = true;
						for (size_t j = 0; j < synowie[odwiedzany].size(); j++) {
							zapalony[odwiedzany] &= !zapalony[synowie[odwiedzany][j]];
						}
						break;
					case 'R':
						zapalony[odwiedzany] = false;
						for (size_t j = 0; j < synowie[odwiedzany].size(); j++) {
							zapalony[odwiedzany] |= !zapalony[synowie[odwiedzany][j]];
						}
						break;
					case 'X':
						zapalony[odwiedzany] = zapalony[synowie[odwiedzany][0]] != zapalony[synowie[odwiedzany][1]];
				}
			}
			if (ojcowie.contains(odwiedzany))
				do_odwiedzenia.push_back(ojcowie[odwiedzany]);
		}
		
		for ([[maybe_unused]] auto&& [_,wartosc] : zapalony) {
			if (wartosc)
				cout << "1";
			else
				cout << "0";
		}
		cout << endl;
	}
}
