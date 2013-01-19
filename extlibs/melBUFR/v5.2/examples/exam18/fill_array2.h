/***********************************************************************
**
**  PROJECT. Master Environmental Library (MEL)
**
**  FILENAME. fill_array2.h
**
**  DESCRIPTION.
**
***********************************************************************/
/*
==  
==  SECTION. Includes/Defines
==
*/

#include <mel_bufr.h>

#define INI_MISSING_VALUE    999.0

#define DS2_RECORD_LENGTH           9     /* 31 float entries */


/*
==  
==  SECTION.  Function Prototypes
==
*/

#if PROTOTYPE_NEEDED

void fill_array2(Data_MixVal_t *rec, int num);

#else

void fill_array2();

#endif








































