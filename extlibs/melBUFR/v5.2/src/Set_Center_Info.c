/*
 * Set_Center_Info - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int Set_Center_Info( BUFR_Info_t* BI, int center, int sub_center, int local_ver )

#else

int Set_Center_Info( BI, center, sub_center, local_ver )
BUFR_Info_t* BI;
int center;
int sub_center;
int local_ver;

#endif
{

    BI->OriginatingCenter = center;
    BI->SubCenter = sub_center;
    BI->VersionNumberOfLocalTables = local_ver;

    return (0);
}
