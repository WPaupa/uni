#include "process.h"
#include <string.h>
#include <stdbool.h>
#include <errno.h>

bool base8(const char* word, size_t length)
{
    if (word[0]!='0') return false;
    else
    {
        for (size_t i = 0; i < length; i++)
            if ('0'>word[i] || word[i]>'7') return false;
    }
    return true;
}

bool base16(const char* word, size_t length)
{
    if (length<1 || word[0]!='0' || word[1]!='x') return false;
    else
    {
        for (size_t i = 2; i < length; i++)
            if (('0'>word[i] || word[i]>'9') && ('a'>word[i] || word[i]>'f')) return false;
    }

    return true;
}

bool base10positive(const char* word, size_t length)
{
    if (length < ((word[0]=='+')?2:1)) return false;
    for (size_t i = (word[0]=='+')?1:0; i < length; i++)
        if ('0'>word[i] || word[i]>'9') return false;

    return true;
}

bool base10negative(const char* word, size_t length)
{
    if (length < ((word[0]=='-')?2:1)) return false;
    for (size_t i = (word[0]=='-')?1:0; i < length; i++)
        if ('0'>word[i] || word[i]>'9') return false;

    return true;
}

bool floatnum(const char* word, size_t length)
{
    if ((!strcmp(word,"inf") || !strcmp(word,"-inf")) || !strcmp(word,"+inf"))
        return true;

    bool decpoint = true, e = true, numbers = false, minus = true, plus = true;
    for (size_t i = 0; i < length; i++)
    {
        if (word[i]=='e')
        {
            if (!e || !numbers) return false;
            else
            {
                e = false;
                minus = true;
                plus = true;
                decpoint = false;
                numbers = false;
            }
        }
        else if (word[i]=='.')
        {
            if (!decpoint) return false;
            else
            {
                minus = false;
                plus = false;
                decpoint = false;
            }
        }
        else if (word[i]=='+')
        {
            if (!plus) return false;
            {
                plus = false;
                minus = false;
            }
        }
        else if (word[i]=='-')
        {
            if (!minus) return false;
            else
            {
                plus = false;
                minus = false;
            }
        }
        else if (word[i]>='0' && word[i]<='9')
        {
            minus = false;
            plus = false;
            numbers = true;
        }
        else return false;
    }
    if (!numbers) return false;

    return true;
}

void process(char* word, line* line)
{
    size_t length = strlen(word);

    if (base8(word, length))
    {
        errno = 0;
        long double x = (long double)strtoull(word, NULL, 8);
        if (errno == ERANGE) addnan(word,line);
        else addnum(x, line);
    }

    else if (base16(word, length))
    {
        errno = 0;
        long double x = (long double)strtoull(word, NULL, 16);
        if (errno == ERANGE) addnan(word,line);
        else addnum(x, line);
    }

    else if (base10positive(word,length))
    {
        errno = 0;
        long double x = (long double)strtoull(word, NULL, 10);
        if (errno == ERANGE) addnan(word,line);
        else addnum(x, line);
    }

    else if (base10negative(word, length))
    {
        errno = 0;
        long double x = (long double)strtoll(word, NULL, 10);
        if (errno == ERANGE) addnan(word,line);
        else addnum(x, line);
    }

    else if (floatnum(word, length))
    {
        errno = 0;
        long double x = strtold(word,NULL);
        if (errno == ERANGE) addnan(word,line);
        addnum(x,line);
    }


    else addnan(word,line);
}