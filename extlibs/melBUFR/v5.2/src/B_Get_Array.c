/*
 * BUFR_Get_Array - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Get_Array - Get the requested number of values from a BUFR message
 * and store them in the given array.  Return the number of values actually
 * stored.  If the number of values returned is less than that requested,
 * then the input was exhausted or an error occurred.
 *
 * NOTE: Memory allocated to each BUFR_Val_t in the given array must be freed
 * manually or with a call to BUFR_Val_Array_free().
 */
/* 
 * CHANGE LOG
 *
 * 100897 LAH:  Added int and uint-t casts
 * 022498 LAH:  Added prints to bufr_log file.
 *
 */

#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BUFR_Get_Array( BUFR_Val_t* ValArray, int NumVals, int IgnoreAFs )

#else

int BUFR_Get_Array( ValArray, NumVals, IgnoreAFs )
BUFR_Val_t* ValArray;
int         NumVals;    /* Number of values in ValArray */
int         IgnoreAFs;  /* Don't process associated fields. */

#endif
{
    extern BUFR_Msg_t BUFR_Msg;
    extern BUFR_Cntl_t BUFR_Cntl;

    BUFR_Val_t* vp;             /* Pointer to ValArray */
    BUFR_Val_t  bv;
    int         vals_assigned;

    int i, n;

    if( BUFR_Msg.Section4_Data.buffer == NULL )
    {
        BUFR_Err_Set( "BUFR_Get_Array", "BUFR_Decode() hasn't been called" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Array", 
          "BUFR_Decode() hasn't been called" );
        return 0;
    }

    if( BUFR_ProcType() == TYPE_ENCODE )
    {
        BUFR_Err_Set( "BUFR_Get_Array",
            "Decoding function called while encoding data" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Array", 
            "Decoding function called while encoding data" );
        return 0;
    }

    switch( BUFR_ProcMethod() )
    {
        case METHOD_VALUES:
            break;

        case METHOD_RAW:
            BUFR_Err_Set( "BUFR_Get_Array",
                "Can't mix raw and value-based processing methods." );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Array", 
                "Can't mix raw and value-based processing methods." );
            return 0;

        case METHOD_TEMPLATE:
            BUFR_Err_Set( "BUFR_Get_Array",
                "Can't mix template and value-based processing methods." );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Array", 
                "Can't mix template and value-based processing methods." );
            return 0;

        /*
         * If the processing method hasn't been set, then this is the first
         * BUFR_Get function to be called. no get.  Set the process flag.
         */

        case METHOD_UNKNOWN:
        default:
            BUFR_Msg.ProcFlag = TYPE_DECODE;
            BUFR_Msg.MethFlag = METHOD_VALUES;
    }

    /* Fill ValArray */

    vals_assigned = 0;

    for( i=0, vp=ValArray; i < NumVals; i++, vp++ )
    {
        if( (n= (int) BUFR_Get_Value( &bv, IgnoreAFs )) != BUFR_OK && n != BUFR_EOD )
        {
            if( n == BUFR_ERROR )
            {
                BUFR_Err_Log( "BUFR_Get_Array" );
                return vals_assigned;
            }
            else    /* Must be EOM */
            {
                BUFR_Err_Set( "BUFR_Get_Array",
                    "Premature End of Message (EOM)"  );
                fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Array", 
                    "Premature End of Message (EOM)" );
                return vals_assigned;
            }
        }

        if( IgnoreAFs == 0 && bv.num_AFs > 0 )
        {
            /* Copy associated field data. */

            /* 100897 LAH: Added int cast */
            n = bv.num_AFs * (int) sizeof(double);

            /* 100897 LAH: Added uint_t cast */
            if( (vp->AF=(double*)malloc( (uint_t) n )) == NULL )
            {
                BUFR_Err_Set( "BUFR_Get_Array",
                    "Can't allocate data for associated fields" );
                fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Array", 
                    "Can't allocate data for associated fields" );
                 break;
            }
            else
                /* 100897 LAH: Added uint_t cast */
                (void) memcpy( (char*)vp->AF, (char*)bv.AF, (uint_t) n );

            /* Copy associated field significances. */

            /* 100897 LAH: Added int cast */
            n = bv.num_AFs * (int) sizeof(int);

            /* 100897 LAH: Added uint_t cast */
            if( (vp->AF_sig=(int*)malloc( (uint_t) n )) == NULL )
            {
                BUFR_Err_Set( "BUFR_Get_Array",
                  "Can't allocate data for associated fields significance" );

                free( (void*) vp->AF );
                fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Get_Array", 
                   "Can't allocate data for associated fields significance" );
                break;
            }
            else
                /* 100897 LAH: Added uint_t cast */
                (void) memcpy( (char*)vp->AF_sig, (char*)bv.AF_sig, (uint_t)n );

            vp->num_AFs = bv.num_AFs;
        }
        else
        {
            vp->AF      = (double*) NULL;
            vp->AF_sig  = (int*) NULL;
            vp->num_AFs = 0;
        }

        /* Copy data from bv to vp. */

        vp->FXY_Val  = bv.FXY_Val;
        vp->Val_Type = bv.Val_Type;
        vp->Val      = bv.Val;

        vals_assigned++;
    }

    return vals_assigned;
}
