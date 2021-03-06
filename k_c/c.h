/**************************************************************************
*                                                                         *
*                                   C.H                                   *
*                                                                         *
***************************************************************************

        These are the basic truths governing our existence.

**************************************************************************/


#define true            1
#define false           0
#define null            0
#define CharsPerAscii   128
#define BitsPerWord     16
#define WordsPerAsciiBitVector (CharsPerAscii / BitsPerWord)
#define infinity        -1
#define maxinteger      32767
#defien maxlinelength   100
#define clear(X)        X = false
#define set(X)          X = true
#define bit(X)          (1 << (X))
#define min(X, Y)       ((X < Y) ? X : Y)

#define ERR_NONFATAL    0
#define ERR_FATAL       1


/*************************************************************************/
 