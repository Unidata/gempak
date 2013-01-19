/*
 * TableA_Value - VERSION: %I%  %E% %T%
 */
/*
 * TableA_Value(): Given a table index value, return the corresponding
 * category name.  Return NULL for invalid indices or reserved categories.
 * 
 * CHANGE LOG
 * 
 * 022498 LAH:  Added prints to bufr_log file.
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

TableA_t TableA_Value( int TableA_index )

#else

TableA_t TableA_Value( TableA_index )
int TableA_index;

#endif
{
    extern TableA_t TableA[MAX_TABLE_A_ENTRIES];
    extern BUFR_Cntl_t BUFR_Cntl;

    TableA_t category;

    if( TableA_index < 0 || TableA_index >= MAX_TABLE_A_ENTRIES )
    {
        BUFR_Err_Set( "TableA_Value", "Invalid index" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "TableA_Value", 
	     "Invalid index" );
        return NULL;
    }

    /* Check for "reserved" string. */

    category = TableA[ TableA_index ];

    if( category == NULL )
        return NULL;

    if( strncmp( category, "RESERVED", strlen( "RESERVED" ) ) == 0 )
        return NULL;

    return category;
}
