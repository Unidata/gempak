/***********************************************************************
**
**  PROJECT. Master Environmental Library (MEL)
**
**  FILENAME. datsav2_io.h
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

/*
==  
==  SECTION.  Data Structures/Types
==
*/

#define DS2_RECORD_LENGTH           9     /* 9 entries */

/*
==  
==  SECTION.  Function Prototypes
==
*/

#if PROTOTYPE_NEEDED

void fill_array(Data_MixVal_t *rec);

#else

void fill_array();

#endif








































