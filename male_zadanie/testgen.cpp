#include <bits/stdc++.h>

using namespace std;

int main()
{
	srand(0);
	for (int i = 0; i < 100000; i++)
	{
		for (int j = 0; j < 4; j++)
		{
			for (int k = 0; k < 1; k++)
				printf("%c",(rand()%(126-33))+33);
			printf(" ");
		}
		puts("");
	}
}
