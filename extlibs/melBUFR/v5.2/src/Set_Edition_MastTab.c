/*
 * Set_Edition_MasterTab  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int Set_Edition_MasterTab ( BUFR_Info_t* BI, int edition, int master_table_num )

#else

int Set_Edition_MasterTab ( BI, edition, master_table_num )
BUFR_Info_t* BI;
int edition;
int master_table_num;

#endif
{

    BI->BUFR_Edition = edition;
    BI->BUFR_MasterTable = master_table_num;

    return (0);
}
