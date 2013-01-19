/*
 * BUFR_Put_Value - VERSION: %I%  %E% %T%
 */
/*
 * BUFR_Put_Value() - Encode an Table B FXY and data value pair.
 */
/*
 * CHANGE LOG
 *
 * 083198  VLP:  Check to see if fxy is a local table B fxy.  If so put a
                 206(data width) in front of the fxy.
 */

#include <mel_bufr.h>
extern BUFR_Msg_t BUFR_Msg;
extern BUFR_Cntl_t BUFR_Cntl;

#if PROTOTYPE_NEEDED

int BUFR_Put_Value( FXY_t FXY_Val, double Val )

#else

int BUFR_Put_Value( FXY_Val, Val )
FXY_t       FXY_Val;
double      Val;

#endif
{
    BUFR_Msg_t* BM;

    EncVal_t enc_val;
    int dw;
    FXY_t new_fxy;

    char buf[64];   /* Arbitrary size. */
    BM = &BUFR_Msg;

/*  VLP 60497 Reset the missing value indicator in case is was a short. */
 
    if(BUFR_Cntl.Missing_User_Set != 2) {
      BM->Missing_Value = BUFR_MISSING_VALUE;
    } else {
      BM->Missing_Value = BUFR_Cntl.User_Missing_Value;
    }

    if( BUFR_ProcType() == TYPE_DECODE )
    {
        BUFR_Err_Set( "BUFR_Put_Value",
            "Encoding function called while decoding data" );
        return 1;
    }

    switch( BUFR_ProcMethod() )
    {
        case METHOD_VALUES:
            break;

        case METHOD_RAW:
            BUFR_Err_Set( "BUFR_Put_Value",
                "Can't mix raw and value-based processing methods." );
            return 1;

        case METHOD_TEMPLATE:

            /*
             * A dataset has been defined.  Ignore the given FXY value and get
             * the next one from the dataset.
             */

            FXY_Val = BUFR_Msg.exp_ptr->fxy;
            FXY_PtrInc();

            break;

        /*
         * If the processing method hasn't been set, then this is the first
         * BUFR_Put function to be called. no get.  Set the process flag.
         */

        case METHOD_UNKNOWN:
        default:
            BUFR_Msg.ProcFlag = TYPE_ENCODE;
            BUFR_Msg.MethFlag = METHOD_VALUES;
    }

    if( !FXY_IsTableB( FXY_Val ) )
    {
        sprintf( buf, "Given FXY value (%s) is not a Table B FXY",
            FXY_String( FXY_Val ) );
        BUFR_Err_Set( "BUFR_Put_Value", buf  );
        return 1;
    }

    enc_val = EncodeValue( (void*)&Val, DT_DOUBLE, FXY_Val );

#if DEBUG_PRINT
    if( BUFR_DebugLevel() > 4 )
    {
        printf( "%f encodes as ", Val );
        EncVal_Print( enc_val, stdout );
    }
#endif

    if( EncVal_IsBad( enc_val ) )
        return 1;

/* If the descriptor is a local table descriptor, put a 206(dw) fxy before
   it so another location can at least read the data correctly.  */
    if(FXY_IsLocal(FXY_Val)){
      dw = FXY_Get_DataWidth(FXY_Val);
      new_fxy = FXY_Pack(2,6,dw);
	    DataList_Put( BUFR_Msg.data_list, &new_fxy,1,NULL,0);
    }
    if( DataList_Put( BUFR_Msg.data_list, &FXY_Val, 1, &enc_val, 1 ) )
    {
       BUFR_Err_Log( "BUFR_Put_Value" );
       return 1;
    }

    return 0;
}
