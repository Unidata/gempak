/*
 * BUFR_Define_Dataset - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Define_Dataset() - Define a dataset based on the given array of
 * FXY values or declare previously encoded FXY values as making up a
 * dataset.  Return 1 on error, else 0.
 *
 * Dataset definition can be performed in one of two ways: all at once or
 * one value at a time.
 *
 * If the dataset is being defined all at once, then FXY_Vals must be an
 * array containing at least one FXY value.
 *
 * If the dataset is being defined one value at a time, then calling this
 * function is simply a way of declaring that the dataset is made up of all
 * previously encoded FXY values.  If this is the case, then the following
 * conditions must be met:
 *
 * 1) Functions BUFR_Put_Array() or BUFR_Put_Value() must have been previously
 *    called (these functions will have set the processing flag to
 *    METHOD_VALUES).
 *
 * 2) FXY_Vals is a NULL pointer and/or NumVals set to 0.
 *
 * Once a dataset has been defined it may not be altered -- subsequent calls
 * to this function will be ignored.
 */
/*
 * CHANGE LOG 
 *
 * 100897 LAH: Added uint_t casts in mallocs
 * 022498 LAH:  Added prints to bufr_log file.
 * 101398 VLP:  Removed return after Method Unknown - pre-definition 
 *              of dataset FXYs was not working
 */
#include <mel_bufr.h>

#if PROTOTYPE_NEEDED

int BUFR_Define_Dataset( FXY_t* FXY_Vals, int NumVals )

#else

int BUFR_Define_Dataset( FXY_Vals, NumVals )
FXY_t* FXY_Vals;    /* NULL if marking the end of the dataset. */
int    NumVals;

#endif

{
    extern BUFR_Msg_t BUFR_Msg;
    extern BUFR_Cntl_t BUFR_Cntl;

    char buf[256];      /* Arbitrarily large size. */

    int i;

    int          terminating_dataset = 0;
    DataEntry_t* de;
    FXY_t*       fp;
    FXY_List_t*  exp_list;


    if( BUFR_ProcType() == TYPE_DECODE )
    {
        BUFR_Err_Set( "BUFR_Define_Dataset",
            "Encoding function called while decoding data" );
        fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", "BUFR_Define_Dataset", 
	     "Encoding function called while decoding data" );
        return 1;
    }
    
    fprintf(BUFR_Cntl.bufr_log, " Defining data set based on usage.\n");
    
    switch( BUFR_ProcMethod() )
    {
        case METHOD_VALUES:

            if( FXY_Vals == NULL || NumVals == 0 )
            {
                /*************************************************************
                * Dataset is being terminated.  All FXY values previously
                * passed to BUFR_Put_Array(), BUFR_Put_String(), or
                * BUFR_Put_Value() now make up the definition of the
                * dataset.
                ************************************************************/
                fprintf(BUFR_Cntl.bufr_log,
                   "BUFR_Define_Dataset: Dataset defined by usage\n");
    
                /*
                * Create an FXY array from the values stored in the
                * BUFR message data list.
                */

                if( (NumVals=DataList_NumFXYs( BUFR_Msg.data_list )) == 0 )
                {
                    BUFR_Err_Set( "BUFR_Define_Dataset",
                        "Attempt to terminate a non-existent dataset" );
                    fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", 
                        "BUFR_Define_Dataset",
                        "Attempt to terminate a non-existent dataset");
                    return 1;
                }

                /* 100897 LAH: Added uint_t cast */
                FXY_Vals = (FXY_t*) malloc( sizeof(FXY_t) * (uint_t) NumVals );

                if( FXY_Vals == NULL )
                {
                    BUFR_Err_Set( "BUFR_Define_Dataset",
                        "Can't allocate memory for FXY values." );
                    fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", 
                        "BUFR_Define_Dataset",
                        "Can't allocate memory for FXY values.");
                    return 1;
                } else
                    terminating_dataset = 1;

                /* Copy FXY values from the BUFR message data list. */

                de = BUFR_Msg.data_list->head->next;
                fp = FXY_Vals;

/**************************************************************************
                for( ; de != BUFR_Msg.data_list->tail; de=de->next, fp++ )
***************************************************************************/
                for( ; de != BUFR_Msg.data_list->tail; de=de->next )
                {
                    for( i=0; i < de->num_fxys; i++ )
                    {
                        *fp = de->fxy_list[i];

                        if( FXY_IsReplicator(*fp) && FXY_Y_Value(*fp) == 0 )
                            BUFR_Msg.rebuild_exp_fxy_list = 1;

                        fp++;
                    }
                }

                /* Use the FXY values copied to form a dataset. */

                break;
            } else
            {
                /*
                * Is the user trying to terminate the dataset
                * or defining a new one?  It's unclear.
                */

                sprintf( buf, "%s%s%s\n",
                    "Ambiguous operation: A dataset must be terminated ",
                    "with a NULL FXY_t\n",
                    "pointer or NumVals set to 0." );

                BUFR_Err_Set( "BUFR_Define_Dataset", buf );
                fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", 
                  "BUFR_Define_Dataset",  buf);
                return 1;
            }

        case METHOD_RAW:
            BUFR_Err_Set( "BUFR_Define_Dataset",
                "Can't mix raw and template-based processing methods." );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", 
                "BUFR_Define_Dataset",
                "Can't mix raw and template-based processing methods."  );
            return 1;

        case METHOD_TEMPLATE:

            /*
             * The dataset has already been defined.  This call is
             * probably being made inside the loop that initially defined
             * the dataset -- ignore the redefinition.
             */
            fprintf(BUFR_Cntl.bufr_log,
               "BUFR_Define_Dataset: Dataset already defined\n");
            return 0;

        /*
        * If the processing method hasn't been set, then this is the first
        * BUFR_Put function to be called (calling routine is defining the
        * dataset all at once).
        */

        case METHOD_UNKNOWN:
        default:
            fprintf(BUFR_Cntl.bufr_log,
               "BUFR_Define_Dataset: Predefinitaion\n");
            break;
    }

    /* Expand the given (or copied) array of FXY values. */

    if( (exp_list = FXY_List_Expand( FXY_Vals, NumVals )) == NULL )
    {
        BUFR_Err_Log( "BUFR_Define_Dataset" );

        if( terminating_dataset )
            free( (void*) FXY_Vals );

        return 1;
    }

    if( terminating_dataset )
    {
        BUFR_Msg.subset_index = 2;
        BUFR_Msg.subset_size  = DataList_NumFXYs( BUFR_Msg.data_list );
        BUFR_Msg.subset_fxys  = FXY_Vals;
    } else
    {
        BUFR_Msg.subset_index = 1;
        BUFR_Msg.subset_size  = NumVals;

        /* Copy the list of unexpanded FXY values forming the subset. */

        /* 100897 LAH: Added uint_t cast */
        BUFR_Msg.subset_fxys = (FXY_t*) malloc( sizeof(FXY_t) * (uint_t)NumVals );

        if( BUFR_Msg.subset_fxys == NULL )
        {
            BUFR_Err_Set( "BUFR_Define_Dataset",
                "Can't allocate memory for FXY values." );
            fprintf(BUFR_Cntl.bufr_log,"%s: %s\n", 
                "BUFR_Define_Dataset",
                "Can't allocate memory for FXY values." );
            return 1;
        }

        for( i=0; i < NumVals; i++ )
            BUFR_Msg.subset_fxys[i] = FXY_Vals[i];
    }

    BUFR_Msg.exp_fxy_list = exp_list;
    BUFR_Msg.exp_ptr      = FXY_List_First( exp_list );

    BUFR_Msg.ProcFlag = TYPE_ENCODE;
    BUFR_Msg.MethFlag = METHOD_TEMPLATE;

    /*
    * Even though the number of FXY values making up a template (data
    * subset) remains constant, if any of those FXY values are delayed
    * replicators (1-XX-000) then the size of the fully expanded FXY list
    * may vary from subset to subset.
    *
    * During processing, when a delayed replicator is encountered,
    * exp_fxy_list is expanded even further.  Once the delayed
    * replicator is expanded, there is no way to unexpanded it.
    * Consequently, if any delayed replication occurs, the expanded
    * list of FXY values must be rebuilt each time the template is
    * exhausted.
    *
    * If any delayed replicators appear in subset_fxys, set the
    * rebuild_exp_fxy_list flag so that FXY_PtrInc() will create a
    * new exp_fxy_list whenever the template is exhausted.
    */

    BUFR_Msg.rebuild_exp_fxy_list = 0;

    for( i=0, fp=BUFR_Msg.subset_fxys; i < BUFR_Msg.subset_size; i++, fp++ )
    {
        if( FXY_IsReplicator(*fp) && FXY_Y_Value(*fp) == 0 )
        {
            BUFR_Msg.rebuild_exp_fxy_list = 1;
            break;
        }
    }

#if DEBUG_PRINT
    if( BUFR_DebugLevel() )
    {
        PrintDivider( '-', 72, stdout );
        fprintf(BUFR_Cntl.bufr_log, "%s %d %s\n", 
            "Original data subset definition (", 
            BUFR_Msg.subset_size, "FXY values)" );

        FXY_PrintVals( BUFR_Msg.subset_fxys, BUFR_Msg.subset_size,
        BUFR_Cntl.bufr_log);

        fprintf(BUFR_Cntl.bufr_log, "\n" );

        fprintf(BUFR_Cntl.bufr_log, 
            "Expanded data subset definition (%d FXY values)\n",
            FXY_List_Size( BUFR_Msg.exp_fxy_list ) );

        FXY_List_Print( BUFR_Msg.exp_fxy_list, BUFR_Cntl.bufr_log );
        PrintDivider( '-', 72, BUFR_Cntl.bufr_log );
    }
#endif

    return 0;
}
