/*
 * FXY_IsReplicator - VERSION: %I%  %E% %T%
 */
/*
 * FXY_IsReplicator - Return 1 if given descriptor is a replication
 * descriptor, otherwise return 0.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int FXY_IsReplicator( FXY_t FXYval )

#else

int FXY_IsReplicator( FXYval )
FXY_t FXYval;

#endif
{
    if( FXYval > (FXY_t)MAX_FXY_VAL )
        return 0;
    else
        return FXY_F_Value( FXYval ) == REPLICATION_VAL;
}
