/*
 * Create_TableD - VERSION: %I%  %E% %T%
 */
/*
 * Create_TableD - Read descriptors for Code Table D from the given filename.
 * Return 1 on error, else 0.
 */
/*
 * CHANGE LOG
 *
 * 102097 LAH: Added void argument to function prototype
 *             Removed unused variable BM
 *             Added ulong_t and int casts
 * 022498 LAH:  Added prints to bufr_log file.
 *            
 */
 
#include <mel_bufr.h>
extern BUFR_Cntl_t BUFR_Cntl;

#define MAX_BUF 1024

#define SEQUENCE_TERMINATOR -1

/* 102097 LAH: Added void argument */
#if PROTOTYPE_NEEDED

int Create_TableD( void )

#else

int Create_TableD(  )

#endif
{
 
    BUFR_Val_t	bv;
    char  buf[MAX_BUF];
    int   n, full;

    int                fxy, f, x, y, i;
    FXY_t              FXY_Val;
    TableD_Sequence_t* DSeq;

    i = 0;
    f = 3;
    /****************************/
    /* Collect FXY descriptors. */
    /****************************/

    /* Get each value (and any associated fields) from the BUFR message. */
    /* 102097 LAH: Added int cast */
    while( (n= (int) BUFR_Get_Value( &bv, 1 )) != BUFR_EOM && n != BUFR_EOF )
    {
        /* 102097 LAH: Added int casts */
        if( n == (int)BUFR_ERROR || n == (int)BUFR_EOF) /* same as if( BUFR_IsError() ) */
        {
            fprintf(BUFR_Cntl.bufr_log,
                "Error getting decoded value! 2\n" );
 
            /* Print the reason for the error and exit. */
            BUFR_Err_Set( "Create_TableD",
                 "Error getting decoded value! 2\n" );
            BUFR_perror( "Create_TableD" );
            /* LAH 112902 added to correct intermediate error */
            BUFR_Val_free(&bv);
            BUFR_Destroy(1);
            return 1;
        }

        i++;
        switch(FXY_Unpack_Dec(bv.FXY_Val))
        {
            case 10:
              f = atoi(bv.Val.string );
              full = 0;
              continue;
            case 11:
              x = atoi(bv.Val.string );
              full++;
              continue;
            case 12:
              y = atoi(bv.Val.string );
              full++;
              break;
            case 30:
              /* 102097 Added ulong_t cast */
              fxy = atoi(bv.Val.string );
              goto PROCESS;
	          case 205064:
		          continue;
        }


        if( (FXY_Val=FXY_Pack(f,x,y)) == (FXY_t)BAD_FXY_VAL ||
            !FXY_IsTableD(FXY_Val) )
        {

            BUFR_Err_Set( " Create_TableD", buf );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "Create_TableD", buf );

            /*LAH 112902 free value to correct intermitent error */
            BUFR_Val_free(&bv);
            return 1;
        }

        /* Check for descriptor duplication. */
/* ?????????? */
        if( TableD_Match( FXY_Val ) != NULL )
        {

/*            BUFR_Err_Set( "TableD_Read", buf );j
            return 1; */
        } 

        /*
         * Found the start of a descriptor sequence.  Create a new
         * descriptor sequence list (with this FXY value stored in
         * the head and tail) and gobble FXY values until F == -1.
         * Since an FXY value can be an index to a Table D descriptor,
         * don't expand 3-XX-YYY values until all Table D descriptors have
         * been read.
         */

        if( (DSeq = TableD_Sequence_Init( FXY_Val )) == NULL )
        {
            sprintf( buf, "Can't create sequence for descriptor %s",
                FXY_String( FXY_Val ) );

            BUFR_Err_Set( "TableD_Read", buf );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "Create_TableD", buf );

            /*LAH 112902 free value to correct intermitent error */
            BUFR_Val_free(&bv);
            return 1;
        }
        if (BUFR_Cntl.Print_New_Entries == YES) 
            fprintf(BUFR_Cntl.bufr_log, "%s \n", FXY_String(FXY_Val));
               continue;

	PROCESS:

        /* 102097 LAH:  Added ulong_t cast */
        if( (FXY_Val=FXY_Pack_Dec(fxy)) == (ulong_t) BAD_FXY_VAL )
        {

            BUFR_Err_Set( "TableD_Read", buf );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "Create_TableD", buf );

            /*LAH 112902 free value to correct intermitent error */
            BUFR_Val_free(&bv);
            return 1;
        }
        if( TableD_Sequence_Put( DSeq, FXY_Val ) )
        {
            BUFR_Err_Set( "TableD_Read", "Can't add descriptor to list" );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "Create_TableD", 
                "Can't add descriptor to list" );
            /*LAH 112902 free value to correct intermitent error */
            BUFR_Val_free(&bv);
            return 1;
        }
        if (BUFR_Cntl.Print_New_Entries == YES) 
            fprintf(BUFR_Cntl.bufr_log, "         %s \n", FXY_String(FXY_Val));
    }


    /*LAH 112902 free value to correct intermitent error */
    BUFR_Val_free(&bv);

    return 0;
}
