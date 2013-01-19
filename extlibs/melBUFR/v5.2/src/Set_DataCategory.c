/*
 * Set_DataCategory  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int Set_DataCategory ( BUFR_Info_t* BI, int data_category, int data_sub_category )

#else

int Set_DataCategory ( BI, data_category, data_sub_category )
BUFR_Info_t* BI;
int data_category;
int data_sub_cateory;

#endif
{

    BI->DataCategory = data_category;
    BI->DataSubCategory = data_sub_category;

    return (0);
}
