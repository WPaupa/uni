/*
 * Zadanie Boulder Dash
 * Mateusz Stańczak
 * Prowadzący ćwiczenia profesor Michał Startek
 * 22.12.2020 r.
 */


#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>


size_t wymiar_y;
size_t wymiar_x;
char **T;


void wczytywanie_wymiarow(){

    (void)!scanf("%zu", &wymiar_y);
    (void)!scanf("%zu", &wymiar_x);
}


void wczytywanie_planszy(){
    
    getchar();
    T = calloc(wymiar_y+1, sizeof(char*));
    for(size_t iy = 0; iy < wymiar_y; iy++){
        
        T[iy] = calloc(wymiar_x+1, sizeof(char));
        size_t ix = 0;
         
            do{
                (void)!scanf("%c",T[iy]+ix);
            
            }   while(T[iy][ix++] != '\n');
    }
}


void wypisywanie_planszy(){
 
    printf("%zu %zu\n", wymiar_y, wymiar_x);
    for(size_t iy = 0; iy < wymiar_y; iy++){
     
        for(size_t ix = 0; ix < wymiar_x; ix++){
         
            putchar(T[iy][ix]);
        }
                putchar('\n');
    }
}


void pozycja_rockforda(size_t *y, size_t *x){
    
    for(size_t iy = 0; iy < wymiar_y; iy++){
     
        for(size_t ix = 0; ix < wymiar_x; ix++){
         
            if(T[iy][ix] == '@'){
                
                *y = iy;
                *x = ix;
                return;
            }
        }
    }
    return;
}


void stabilizacja_mapy(){

    for(size_t iy = wymiar_y - 2; iy != (size_t)(-1); iy--){
     
        for(size_t ix = wymiar_x - 1; ix != (size_t)(-1); ix--){
         
            if(((T[iy][ix] == '$')||(T[iy][ix] == 'O'))&&(T[iy + 1][ix] == ' ')){
                
                T[iy + 1][ix] = T[iy][ix];
                T[iy][ix] = ' ';
                iy = wymiar_y - 2;
                ix = wymiar_x - 1;
        
            }
        }
    }
}


bool czy_zebralem_wszystkie_diamenty(){

    for(size_t iy = 0; iy < wymiar_y; iy++){
     
        for(size_t ix = 0; ix < wymiar_x; ix++){
         
            if(T[iy][ix] == '$'){
            
                return 0;
            }
        }
    }
    return 1;
}


bool czy_rockford_zyje(){

    for(size_t iy = 0; iy < wymiar_y; iy++){
     
        for(size_t ix = 0; ix < wymiar_x; ix++){
         
            if(T[iy][ix] == '@'){
            
                return 1;
            }
        }
    }
    return 0;
}


void ide_na_prawo(){

    size_t iy, ix;
    pozycja_rockforda(&iy, &ix);
    size_t nowa_pozycja_rockforda = ix + 1;
    if(T[iy][nowa_pozycja_rockforda] == ' '){

        T[iy][nowa_pozycja_rockforda] ='@';
        T[iy][ix] = ' ';
    }
    else if(T[iy][nowa_pozycja_rockforda] == '#'){

        T[iy][ix] = '@';
    }
    else if((T[iy][nowa_pozycja_rockforda] == 'O')&&(T[iy][nowa_pozycja_rockforda + 1] == ' ')){

        T[iy][nowa_pozycja_rockforda] = '@';
        T[iy][nowa_pozycja_rockforda + 1] = 'O';
        T[iy][ix] = ' ';
    }
    else if((T[iy][nowa_pozycja_rockforda] == 'O')&&(T[iy][nowa_pozycja_rockforda + 1] != ' ')){

        T[iy][ix] = '@';
        T[iy][nowa_pozycja_rockforda] = 'O';
    }
    else if(T[iy][nowa_pozycja_rockforda] == '$'){

        T[iy][nowa_pozycja_rockforda] = '@';
        T[iy][ix] = ' ';
    }
    else if(T[iy][nowa_pozycja_rockforda] == '+'){

        T[iy][nowa_pozycja_rockforda] = '@';
        T[iy][ix] = ' ';
    }
    else if((T[iy][nowa_pozycja_rockforda] == 'X')&&(czy_zebralem_wszystkie_diamenty() == 1)){

        T[iy][ix] = ' ';
        T[iy][nowa_pozycja_rockforda] = 'X';
    }
    else if((T[iy][nowa_pozycja_rockforda] == 'X')&&(czy_zebralem_wszystkie_diamenty() != 1)){

        T[iy][ix] = '@';
        T[iy][nowa_pozycja_rockforda] = 'X';
    }
}


void ide_na_lewo(){

    size_t iy, ix;
    pozycja_rockforda(&iy, &ix);
    size_t nowa_pozycja_rockforda = ix - 1;
    if(T[iy][nowa_pozycja_rockforda] == ' '){

        T[iy][nowa_pozycja_rockforda] = '@';
        T[iy][ix] = ' ';
    }
    else if(T[iy][nowa_pozycja_rockforda] == '#'){

        T[iy][ix] ='@';
    }
    else if((T[iy][nowa_pozycja_rockforda] == 'O')&&(T[iy][nowa_pozycja_rockforda - 1] == ' ')){

        T[iy][nowa_pozycja_rockforda] = '@';
        T[iy][nowa_pozycja_rockforda - 1] = 'O';
        T[iy][ix] = ' ';
    }
    else if((T[iy][nowa_pozycja_rockforda] == 'O')&&(T[iy][nowa_pozycja_rockforda - 1] != ' ')){

        T[iy][ix] = '@';
        T[iy][nowa_pozycja_rockforda] = 'O';
    }
    else if(T[iy][nowa_pozycja_rockforda] == '$'){

        T[iy][nowa_pozycja_rockforda] = '@';
        T[iy][ix] = ' ';
    }
    else if(T[iy][nowa_pozycja_rockforda] == '+'){

        T[iy][nowa_pozycja_rockforda] = '@';
        T[iy][ix] = ' ';
    }
    else if((T[iy][nowa_pozycja_rockforda] == 'X')&&(czy_zebralem_wszystkie_diamenty() == 1)){

        T[iy][ix] = ' ';
        T[iy][nowa_pozycja_rockforda] = 'X';
    } 
    else if((T[iy][nowa_pozycja_rockforda] == 'X')&&(czy_zebralem_wszystkie_diamenty() != 1)){

        T[iy][ix] = '@';
        T[iy][nowa_pozycja_rockforda] = 'X';
    }
}


void ide_na_gore(){

    size_t iy, ix;
    pozycja_rockforda(&iy, &ix);
    size_t nowa_pozycja_rockforda = iy - 1;
    if(T[nowa_pozycja_rockforda][ix] == ' '){

        T[nowa_pozycja_rockforda][ix] ='@';
        T[iy][ix] = ' ';
    }
    else if(T[nowa_pozycja_rockforda][ix] == '#'){

        T[iy][ix] ='@';
    }
    else if(T[nowa_pozycja_rockforda][ix] == 'O'){

        T[iy][ix] = '@';
    }
    else if(T[nowa_pozycja_rockforda][ix] == '$'){

        T[nowa_pozycja_rockforda][ix] = '@';
        T[iy][ix] = ' ';
    }
    else if(T[nowa_pozycja_rockforda][ix] == '+'){

        T[nowa_pozycja_rockforda][ix] = '@';
        T[iy][ix] = ' ';
    }
    else if((T[nowa_pozycja_rockforda][ix] == 'X')&&(czy_zebralem_wszystkie_diamenty() == 1)){

        T[iy][ix] = ' ';
        T[nowa_pozycja_rockforda][ix] = 'X';
    }
    else if((T[nowa_pozycja_rockforda][ix] == 'X')&&(czy_zebralem_wszystkie_diamenty() != 1)){

        T[iy][ix] = '@';
        T[nowa_pozycja_rockforda][ix] = 'X';
    }
}


void ide_na_dol(){

    size_t iy, ix;
    pozycja_rockforda(&iy, &ix);
    size_t nowa_pozycja_rockforda = iy + 1;
    if(T[nowa_pozycja_rockforda][ix] == ' '){

        T[nowa_pozycja_rockforda][ix] ='@';
        T[iy][ix] = ' ';
    }
    else if(T[nowa_pozycja_rockforda][ix] == '#'){

        T[iy][ix] = '@';
    }
    else if(T[nowa_pozycja_rockforda][ix] == 'O'){

        T[iy][ix] = '@';
    }
    else if(T[nowa_pozycja_rockforda][ix] == '$'){

        T[nowa_pozycja_rockforda][ix] = '@';
        T[iy][ix] = ' ';
    }
    else if(T[nowa_pozycja_rockforda][ix] == '+'){

        T[nowa_pozycja_rockforda][ix] = '@';
        T[iy][ix] = ' ';
    }
    else if((T[nowa_pozycja_rockforda][ix] == 'X')&&(czy_zebralem_wszystkie_diamenty() == 1)){

        T[iy][ix] = ' ';
        T[nowa_pozycja_rockforda][ix] = 'X';
    }
    else if((T[nowa_pozycja_rockforda][ix] == 'X')&&(czy_zebralem_wszystkie_diamenty() != 1)){

        T[iy][ix] = '@';
        T[nowa_pozycja_rockforda][ix] = 'X';
    }
}


void wczytywanie_ruchow_i_granie(){

    char c;
    while(scanf("%c", &c) != -1){
     
        if((c != '\n')&&(czy_rockford_zyje() == 1)){
            
            switch(c){
        
                case 'a':
                    ide_na_lewo();
                    break;
                
                case 's':
                    ide_na_dol();
                    break;
                
                case 'd':
                    ide_na_prawo();
                    break;
                
                case 'w':
                    ide_na_gore();
                    break;
            }
            stabilizacja_mapy();
        }
        
        else if(c == '\n'){
            
            wypisywanie_planszy();
        }
    }
}


int main() {
    
    wczytywanie_wymiarow();
    wczytywanie_planszy();
    stabilizacja_mapy();
    wypisywanie_planszy();
    wczytywanie_ruchow_i_granie();
    
    for(size_t i = 0; i < wymiar_y; i++){
     
        free(T[i]);
    }
    free(T);
    
    return 0;
}
