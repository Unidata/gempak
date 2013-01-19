/*
 * Set_MasterTableVer  - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int Set_MasterTableVer ( BUFR_Info_t* BI, int table_ver )

#else

int Set_MasterTableVer ( BI, table_ver)
BUFR_Info_t* BI;
int table_ver;

#endif
{

    BI->VersionNumberOfMasterTables = table_ver;

    return (0);
}
