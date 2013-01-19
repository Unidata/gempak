/*
 * BUFR_Destroy - VERSION: %I%  %E% %T%
 */
#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
extern BUFR_Val_t LastVal;
extern BUFR_Cntl_t BUFR_Cntl;
/*
 * CHANGE LOG
 *
 * 102097 LAH:  Removed unused variable n
 * 022498 LAH:  Added prints to bufr_log file.
 *
 */

#if PROTOTYPE_NEEDED

void BUFR_Destroy( int DestroyAll )

#else

void BUFR_Destroy( DestroyAll)

int DestroyAll;
#endif
{
#if MEMORY_DEBUG
    extern MemoryNode_t Mem_Head, Mem_Tail;
    MemoryNode_t *mem_ptr, *next_ptr;
#endif

    BUFR_Val_t *vpr;
    int i,j;
/*    int DestroyAll; */

    BUFR_Msg_t* BM;

    BM = &BUFR_Msg;

    fprintf(BUFR_Cntl.bufr_log, "BUFR_Destroy: Destroying BUFR message\n");

    /* Destroy associated field list. */

    AF_List_Destroy( BM->af_list );
    free( (void*) BM->af_list );

    /* Destroy data list. */

    DataList_Destroy( BM->data_list );
    free( (void*) BM->data_list );

    if( BM->subset_fxys != NULL )
        free( (void*) BM->subset_fxys );

    /* If Decoding must destroy the expanded FXY list */
    if ( BUFR_ProcType() == TYPE_DECODE )
    {
       FXY_List_Destroy(BM->exp_fxy_list);
       /* if message was compressed, free decompressed data */
       if ( BUFR_Msg.Compress_Flag == 1) 
       {
         vpr = BM->decom_vals;
         j = BM->dc_numb * Int2ToInt(BM->Section3.data_subsets);
         for ( i =0; i < j; i++){
	   if ( vpr->Val_Type == DT_STRING )
	   {
	       free(vpr->Val.string);
	   }
	   vpr++;
         }
         free(BM->decom_vals);
      }
    }
    
    /* Destroy stacks used for Table C operators. */

    ValStack_Destroy( &BM->DataWidthStack );
    ValStack_Destroy( &BM->ScaleStack );

    /* if template was used, destroy structures used to save FXYs */

    if ( BM->MethFlag == METHOD_TEMPLATE) {
        FXY_List_Destroy(BM->exp_fxy_list);
        /*
         * JRA020797 - This is a redundant free.
        free( (void*) BM->subset_fxys);
         */
    }

    /* Destroy bit streams */

    BitStream_Destroy( &BM->Section1_Data );
    BitStream_Destroy( &BM->Section2_Data );
    BitStream_Destroy( &BM->Section3_Data );
    BitStream_Destroy( &BM->Section4_Data );

    
    /* the following code was added in an attempt to reset LastVal */
    /* between messages included in one file   */
    /* NOTE: LastVal is referenced in BUFR_Get_Value */
    /* destroy or reset LastVal structure */

    if ( LastVal.AF != NULL )
    {
        LastVal.AF = NULL;
        LastVal.AF_sig = NULL;
    }

    if ( LastVal.Val_Type == DT_STRING)
    {
        if ( LastVal.Val.string != NULL)
        {
            LastVal.Val.string = NULL;
        }
    }

    /*  end of new block */

    if( DestroyAll )
    {
        /* Deallocate code tables. */

        TableD_Destroy();
        TableB_Destroy();
        TableA_Destroy();
        Table0_Destroy();

        /* Free table names. */

        if( BM->MasterTable0 != NULL )
            free( (void*) BM->MasterTable0 );

        if( BM->MasterTableA != NULL )
            free( (void*) BM->MasterTableA );

        if( BM->MasterTableB != NULL )
            free( (void*) BM->MasterTableB );

        if( BM->MasterTableD != NULL )
            free( (void*) BM->MasterTableD );

        if( BM->LocalTableB != NULL )
            free( (void*) BM->LocalTableB );

        if( BM->LocalTableD != NULL )
            free( (void*) BM->LocalTableD );

        BUFR_Msg.InitStatus = NOT_INITIALIZED;
    }
    else
    {
        /*
         * BUFR_Init() re-reads tables even if it isn't necessary.
         * Until it's fixed, destroy the tables.
         */
/*  VLP 7/10/97.... BUFR_Init now does not re-read the tables so these next */
/*  lines are commented out as they are really not needed */

/*        TableD_Destroy();
        TableB_Destroy();
        TableA_Destroy();
        Table0_Destroy(); */

        /* Free table names. */

/*        if( BM->MasterTable0 != NULL )
            free( (void*) BM->MasterTable0 );

        if( BM->MasterTableA != NULL )
            free( (void*) BM->MasterTableA );

        if( BM->MasterTableB != NULL )
            free( (void*) BM->MasterTableB );

        if( BM->MasterTableD != NULL )
            free( (void*) BM->MasterTableD );

        if( BM->LocalTableB != NULL )
            free( (void*) BM->LocalTableB );

        if( BM->LocalTableD != NULL )
            free( (void*) BM->LocalTableD ); 
*/
    }

    /*
     * Don't clear the error, BUFR_Initialize will reinitialize the error
     * message(s).
     *
    BUFR_Err_Clear();
    */

#if MEMORY_DEBUG
    printf( "Outstanding memory references after BUFR_Destroy():\n" );
    printf( "\n" );
    BUFR_mem_print( stdout );

    /* Destroy memory references linked list. */

    mem_ptr = Mem_Head.next;

    while( mem_ptr != &Mem_Tail )
    {
        if( mem_ptr->ref_count > 0 )
            (void) free( mem_ptr->beg_addr );

        next_ptr = mem_ptr->next;
        (void) free( mem_ptr );
        mem_ptr = next_ptr;
    }

    Mem_Head.next = &Mem_Tail;

#endif

}
