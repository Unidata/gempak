#ifndef _MEL_BUFR_FUNCTIONS_H
#define _MEL_BUFR_FUNCTIONS_H

/**********************************************************************/
/*****************                                    *****************/
/*****************   DO NOT CHANGE ANY VALUES BELOW   *****************/
/*****************                                    *****************/
/**********************************************************************/
/*
*    CORRECTION LOG 
*
*    092997  LAH:  Added definition of TableC_Find
*                  Corrected definition of Int2ToInt
*    100897  LAH:  Changed Int3ToInt function type from uint_t to int
*                  Changed IntToInt3 argument type from uint_t to int
*    100997  LAH:  Added Find_Status prototypes
*                  Added Set_Staus prototypes
*                  Added BUFR_Find_End prototypes
*    102097  LAH:  Added Create_TableB prototypes
*                  Added BUFR_Set_Multiple_Msg_Flag() prototypes
*                  Added Create_TableD prototypes
*    121297  LAH:  Added argument to FXY_List_Insert to accodimate
*                  new additionas to FXY_List_t structure
*    022398  LAH:  Added Set_Flag prototype
*    022498  LAH:  Added functions Flag_Print and Get_BUFR_Log_File_ptr
*                  prototypes
*    120202  LAH:  Renamed B_Val_free.c to B_Val_Array_Free.c and created new 
*                  function B_Val_Free.c.  
*    120302  VAL:  Changed argument type for B_Destroy
*    120402  LAH:  Add prototype for Get_BUFR_Log_File
*/

/* Check compiler: ANSI-C and C++ need function prototypes. */

/*
#if defined( __cplusplus ) || defined( __STDC__ ) || defined( __GNUC__ )
#define PROTOTYPE_NEEDED 1
#else
#define PROTOTYPE_NEEDED 0
#endif
*/

/* Always use prototypes */
#define PROTOTYPE_NEEDED 1

/**************************/
/* BUFR Library functions */
/**************************/

#if PROTOTYPE_NEEDED

int    BUFR_Init( BUFR_Info_t*, char*, ProcFlag_t );
int    BUFR_Init_Sub( BUFR_Info_t*, char*, ProcFlag_t);
void   BUFR_Destroy( int );
int        BUFR_DataSet_Empty(BUFR_DataSet_t*);
int        BUFR_DataSet_Init(BUFR_DataSet_t*);
int        BUFR_DataSetEntryPut( BUFR_DataSet_t*, BUFR_Val_t, int);
int        BUFR_DataSet_Free(BUFR_DataSet_t*);
int        BUFR_BitWidth( int );
void       BUFR_Abort( void );
int        BUFR_Add_AF( int, int );
int        BUFR_AtEOD( void );
int        BUFR_AtEOF( void );
int        BUFR_AtEOM( void );
int        BUFR_Cancel_AF( void );
int        BUFR_Change_DataWidth( int );
int        BUFR_Change_RefVal( FXY_t, int );
int        BUFR_Change_Scale( int );
void       BUFR_Close( void );
void       BUFR_Debug( int );
int        BUFR_DebugLevel( void );
int        BUFR_Decode( void );
int        BUFR_Define_Dataset( FXY_t*, int );
int        BUFR_Encode( BUFR_Info_t* );
int        BUFR_FindSequence( FXY_t*, int, FXY_t* );
int        BUFR_Get_Array( BUFR_Val_t*, int, int );
int        BUFR_Get_Dataset( int, BUFR_DataSet_t*, int );
int        BUFR_Get_OptionalData( char*, int );
int        BUFR_Get_S3Data( char*, int );
int        BUFR_Get_S4Data( char*, int );
Status_t   BUFR_Get_Value( BUFR_Val_t*, int );
int        BUFR_IsComment( char* );
int        BUFR_IsError( void );
uint_t     BUFR_MaxVal( int );
int        BUFR_NumDatasets( void );
int        BUFR_Position_Msg( void );
void       BUFR_Print( int, int, FILE* );
MethFlag_t BUFR_ProcMethod( void );
ProcFlag_t BUFR_ProcType( void );
int        BUFR_Put_Array( void*, int, DataType_t, FXY_t*, int );
int        BUFR_Put_MixArray( Data_MixVal_t*, int, FXY_t*, int );
int        BUFR_Put_OptionalData( char*, int );
int        BUFR_Put_S3Data( char*, int );
int        BUFR_Put_S4Data( char*, int );
int        BUFR_Put_String( char* );
int        BUFR_Put_Value( FXY_t, double );
int        BUFR_Read_Msg( void );
int        BUFR_Read_Tables( void );
int        BUFR_Reset_DataWidth( void );
int        BUFR_Reset_RefVals( void );
int        BUFR_Reset_Scale( void );
void       BUFR_Set_Multiple_Msg_Flag(void);
int strcasecmp (const char *, const char *);
void strupper (char *dst_ptr, char *src_ptr);
void       BUFR_Trace( int );
int        BUFR_TraceLevel( void );
void       BUFR_ValArray_Destroy(BUFR_Val_t **, int);
int        BUFR_Write_Msg( void );

void BUFR_Err_Init( void );
void BUFR_Err_Clear( void );
void BUFR_Err_Log( char* func );
void BUFR_Err_Print( char* );
void BUFR_Err_Set( char*, char* );

int BUFR_FTP_GetFile( char*, char*, char* );

int  BUFR_Info_Init( BUFR_Info_t* );
void BUFR_Info_Print( BUFR_Info_t, FILE* );
int  BUFR_Info_Verify( BUFR_Info_t );

void BUFR_Val_Print( BUFR_Val_t, int, int, FILE* );
void BUFR_Val_free( BUFR_Val_t*);
void BUFR_Val_Array_free( BUFR_Val_t*, int );

void          BUFR_perror( char* );
void*         BUFR_realloc( void*, size_t );
char*         BUFR_strdup( char* );

int  BitStream_Init( BitStream_t*, int );
void BitStream_Destroy( BitStream_t* );
int      BitStream_Flush( BitStream_t* );
int      BitStream_Get( BitStream_t*, EncVal_t*, int );
int      BitStream_Increase( BitStream_t* );
int      BitStream_Position( BitStream_t*, int );
int      BitStream_Put( BitStream_t*, EncVal_t );
int      BitStream_Rewind( BitStream_t* );

int         AF_List_Init( AF_List_t* );
void        AF_List_Destroy( AF_List_t* );
void            AF_List_Clear( AF_List_t* );
AF_Entry_t*     AF_List_First( AF_List_t* );
AF_Entry_t*     AF_List_Last( AF_List_t* );
void            AF_List_Print( AF_List_t*, FILE* );
int             AF_List_Put( AF_List_t*, int, int );
int             AF_List_Remove( AF_List_t* );
int             AF_List_Size( AF_List_t* );
int B_Mix_Print(Data_MixVal_t *,  int);

int          Create_TableB( void );
int          Create_TableD( void );
int          DataList_Init( DataList_t* );
void         DataList_Destroy( DataList_t* );
int              DataList_Encode( void );
DataEntry_t*     DataList_First( DataList_t* );
DataEntry_t*     DataList_Last( DataList_t* );
int              DataList_NumFXYs( DataList_t* );
void             DataList_Print( FILE* );
int              DataList_Put( DataList_t*, FXY_t*, int, EncVal_t*, int );
int              DataList_Size( DataList_t* );

EncVal_t EncodeValue( void*, DataType_t, FXY_t );

void    EncVal_Init( EncVal_t* );
void    EncVal_Destroy( EncVal_t );
int         EncVal_Get( double*, EncVal_t, int, int, int* );
int         EncVal_IsBad( EncVal_t );
void        EncVal_Print( EncVal_t, FILE* );
int         EncVal_Set( EncVal_t*, double, int, int, int );
int         EncVal_RVSet( EncVal_t*, double, int );

int     FXY_Expand( FXY_t, FXY_t** );
int     FXY_F_Value( FXY_t );
int     FXY_Get_DataWidth( FXY_t );
int     FXY_Get_RefVal( FXY_t );
int     FXY_Get_Scale( FXY_t );
int     FXY_Get_Values( FXY_t, int*, int*, int* );
int     FXY_IsReplicator( FXY_t );
int     FXY_IsTableB( FXY_t );
int     FXY_IsTableC( FXY_t );
int     FXY_IsTableD( FXY_t );
FXY_t   FXY_Pack( int, int, int );
FXY_t   FXY_Pack_Dec( int );
void    FXY_Print_Recursive( FXY_t,  FILE*);
void    FXY_Print( FXY_t, FILE* );
void    FXY_PrintVals( FXY_t*, int, FILE* );
int     FXY_PtrInc( void );
char*   FXY_String( FXY_t );
char*   FXY_Units( FXY_t );
Units_t FXY_UnitsType( FXY_t );
int     FXY_Unpack( FXY_t, uint_t*, uint_t*, uint_t* );
int     FXY_Unpack_Dec( FXY_t );
int     FXY_X_Value( FXY_t );
int     FXY_Y_Value( FXY_t );

FXY_List_t*  FXY_List_Init( void );
void         FXY_List_Destroy( FXY_List_t* );
FXY_List_t*      FXY_List_Expand( FXY_t*, int );
FXY_Entry_t*     FXY_List_First( FXY_List_t* );
int              FXY_List_Get( FXY_t** );
int              FXY_List_Insert( FXY_Entry_t*, FXY_t, FXY_List_t*);
FXY_Entry_t*     FXY_List_Last( FXY_List_t* );
FXY_Entry_t*     FXY_List_Prev( FXY_List_t*, FXY_Entry_t* );
void             FXY_List_Print( FXY_List_t*, FILE* );
int              FXY_List_Put( FXY_List_t*, FXY_t );
FXY_Entry_t*     FXY_List_Remove( FXY_List_t*, FXY_Entry_t* );
int              FXY_List_Size( FXY_List_t* );
int              Get_BUFR_Edition (void);
int              Get_BUFR_MasterTableNum(void);
int              Get_Century (void);
int              Get_DataCategory (void);
int              Get_DataSubCategory(void);
int              Get_Day(void);
int              Get_Hour(void);
int              Get_HourMinute(void);
int		 Get_LocalTableVer (void);
int		 Get_MasterTableVer (void);
int		 Get_Minute(void);
int		 Get_Month(void);
int              Get_ObservedDataFlag(void);
char*            Get_OriginatingCenter(void);
int              Get_OriginatingCenterID(void);
int              Get_SubCenter(void);
int              Get_UpdateSeqNum(void);
double           Get_Val_Double(BUFR_Val_t);
FXY_t            Get_Val_FXY(BUFR_Val_t);
int              Get_Val_Int(BUFR_Val_t);
int              Get_Val_Scale(BUFR_Val_t );
char*            Get_Val_String(BUFR_Val_t );
DataType_t	 Get_Val_Type(BUFR_Val_t BV);
int		 Get_Year (void);
int		 Get_YMD(Date_Format_t);

int      HexStr_Destroy( EncVal_t*, int );
int      HexStr_Get( HexStr_t, int, double*, int* );
int      HexStr_GetBit( HexStr_t, int );
HexStr_t HexStr_Set( double, int );
HexStr_t HexStr_RVSet( double, int );
int      HexStr_SetBit( HexStr_t, int );

int   Missing_Value_Replace( double*, FXY_t );

int Is_Val_Missing (BUFR_Val_t );
int Set_Center_Info( BUFR_Info_t* , int , int , int );
int Set_DataCategory ( BUFR_Info_t* , int, int);
int Set_Date( BUFR_Info_t* , int , Date_Format_t );
int Set_Edition_MasterTab ( BUFR_Info_t* , int , int );
int Set_MasterTableVer ( BUFR_Info_t* , int );
int Set_ObservedData ( BUFR_Info_t* , int );
int Set_Time( BUFR_Info_t* , int );
int Set_UpdateSequence ( BUFR_Info_t* , int );


void  Table0_Init( void );
void  Table0_Destroy( void );
void      Table0_Print( FILE* );
int       Table0_Read( char* );
Table0_t  Table0_Value( int );

void  TableA_Init( void );
void  TableA_Destroy( void );
void      TableA_Print( FILE* );
int       TableA_Read( char* );
TableA_t  TableA_Value( int );

int           TableB_Init( void );
void          TableB_Destroy( void );
Descriptor_t*     TableB_Get( FXY_t );
Units_t           TableB_GetUnits( char* );
void              TableB_Print( FILE* );
int               TableB_Put( Descriptor_t );
int               TableB_Read( char* );

int                TableD_Init( void );
void               TableD_Destroy( void );
int                    TableD_Expand( TableD_Entry_t* );
TableD_Sequence_t*     TableD_Match( FXY_t );
void                   TableD_Print( FILE* );
int                    TableD_Read( char* );

int  TableC_Find( FXY_t, FILE*, int);

TableD_Sequence_t* TableD_Sequence_Init( FXY_t );
void               TableD_Sequence_Destroy( TableD_Sequence_t* );
int                TableD_Sequence_Put( TableD_Sequence_t*, FXY_t );

int  ValStack_Init( ValStack_t* );
void ValStack_Destroy( ValStack_t* );
void ValStack_Clear( ValStack_t* );
int  ValStack_Get( ValStack_t* );
int  ValStack_IsEmpty( ValStack_t* );
int  ValStack_Pop( ValStack_t* );
int  ValStack_Push( ValStack_t*, int );

int  BytesInBits( int );
int  FileExists( char* );
int  Find_Status( void );
int  BUFR_Find_End( void );
int  Set_Status( void );
int  Int2ToInt( Int2_t );
Int2_t IntToInt2( int );
int    Int3ToInt( Int3_t );
Int3_t IntToInt3( int );
void   PrintDivider( char, int, FILE* );
void   Flag_Print(FILE *);
FILE   *Get_BUFR_Log_File_Ptr( void );
char   *Get_BUFR_Log_File( void );
double TenPow( int );
double TruncateValue( double );
double TwoPow( int );
void   VoidInc( void**, DataType_t );
int    VoidVal( void*, DataType_t, void*, DataType_t );
int    Set_Flag( CNTL_OPT_ENUM);
void print_compressed( FILE* fp, int range1, int range2, BUFR_Val_t* D_con,
                        int num_fxys, int no_sets, FXY_t* whole_fxys );

/* Functions written by Valerie Pastor of SAIC. */

void BUFR_Set_Missing_Value( double );
void BUFR_Turn_Off_Multiple_Msg_Flag(void);
int Find_Data_Type( char*, int * );
int message_read( FILE*, FILE*, char*, int, int  );
int make_name(FILE*, char*, char, int, int, int *, int * );
int write_msg( FILE*, FILE*, int, int );
int mess_write(FILE*, int, int *  );
int Loop_T_Data_Type( FILE*, char*, int * );
int Num_Messages( char* );
void dump_print(int, int, int, FILE*, int *);
int Check_Compress( void );
int D_Compression(void);
int Compression( void );
void Set_Compress(void);
void Prn_Comprss(FILE*, int, int, BUFR_Val_t*,int, int, FXY_t*);
int FXY_IsLocal( FXY_t FXY_Val );

#else

int B_Mix_Print(int);
int    BUFR_Init();
int    BUFR_Init_Sub();
void   BUFR_Destroy();
int    BUFR_DataSet_Empty();
int    BUFR_DataSet_Init();
int    BUFR_DataSetEntryPut();
int    BUFR_DataSet_Free();
int    BUFR_Get_Dataset();
int    BUFR_BitWidth();
void   BUFR_Abort();
int    BUFR_Add_AF();
int    BUFR_AtEOD();
int    BUFR_AtEOF();
int    BUFR_AtEOM();
int    BUFR_Cancel_AF();
int    BUFR_Change_AF();
int    BUFR_Change_DataWidth();
int    BUFR_Change_RefVal();
int    BUFR_Change_Scale();
void   BUFR_Close();
void   BUFR_Debug();
int    BUFR_DebugLevel();
int    BUFR_Decode();
int    BUFR_Define_Dataset();
int    BUFR_Encode();
int    BUFR_FindSequence();
int    BUFR_Get_Array();
int    BUFR_Get_OptionalData();
int    BUFR_Get_S3Data();
int    BUFR_Get_S4Data();
Status_t   BUFR_Get_Value();
int    BUFR_IsComment();
int    BUFR_IsError();
uint_t BUFR_MaxVal();
int    BUFR_NumDatasets();
void   BUFR_Print();
MethFlag_t BUFR_ProcMethod();
ProcFlag_t BUFR_ProcType();
int    BUFR_Position_Msg();
int    BUFR_Put_Array();
int    BUFR_Put_MixArray();
int    BUFR_Put_OptionalData();
int    BUFR_Put_S3Data();
int    BUFR_Put_S4Data();
int    BUFR_Put_String();
int    BUFR_Put_Value();
int    BUFR_Read_Msg();
int    BUFR_Read_Tables();
int    BUFR_Reset_DataWidth();
int    BUFR_Reset_RefVals();
int    BUFR_Reset_Scale();
void   BUFR_Set_Multiple_Msg_Flag();
int strcasecmp ();
void strupper ();
void   BUFR_Trace();
int    BUFR_TraceLevel();
void   BUFR_ValArray_Destroy();
int    BUFR_Write_Msg();

int BUFR_FTP_GetFile();

void BUFR_Err_Init();
void BUFR_Err_Clear();
void BUFR_Err_Log();
void BUFR_Err_Print();
void BUFR_Err_Set();

int  BUFR_Info_Init();
void BUFR_Info_Print();
int  BUFR_Info_Verify();

void BUFR_Val_Print();
void BUFR_Val_free();
void BUFR_Val_Array_free();

void  BUFR_perror();
void* BUFR_realloc();
char* BUFR_strdup();

int  BitStream_Init();
void BitStream_Destroy();
int  BitStream_Flush();
int  BitStream_Get();
int  BitStream_Increase();
int  BitStream_Position();
int  BitStream_Put();
int  BitStream_Rewind();

int         AF_List_Init();
void        AF_List_Destroy();
void        AF_List_Clear();
AF_Entry_t* AF_List_First();
AF_Entry_t* AF_List_Last();
void        AF_List_Print();
int         AF_List_Put();
int         AF_List_Remove();
int         AF_List_Size();

int      Create_TableB();
int      Create_TableD( );
int          DataList_Init();
void         DataList_Destroy();
int          DataList_Encode();
DataEntry_t* DataList_First();
DataEntry_t* DataList_Last();
int          DataList_NumFXYs();
void         DataList_Print();
int          DataList_Put();
int          DataList_Size();

EncVal_t EncodeValue();

void    EncVal_Init();
void    EncVal_Destroy();
int     EncVal_Get();
int     EncVal_IsBad();
void    EncVal_Print();
int     EncVal_Set();
int     EncVal_RVSet();

int     FXY_Expand();
int     FXY_F_Value();
int     FXY_Get_DataWidth();
int     FXY_Get_RefVal();
int     FXY_Get_Scale();
int     FXY_Get_Values();
int     FXY_IsReplicator();
int     FXY_IsTableB();
int     FXY_IsTableC();
int     FXY_IsTableD();
FXY_t   FXY_Pack();
FXY_t   FXY_Pack_Dec();
void    FXY_Print();
void    FXY_Print_Recursive();
void    FXY_PrintVals();
int     FXY_PtrInc();
char*   FXY_String();
char*   FXY_Units();
Units_t FXY_UnitsType();
int     FXY_Unpack();
int     FXY_Unpack_Dec();
int     FXY_X_Value();
int     FXY_Y_Value();

FXY_List_t*  FXY_List_Init();
void         FXY_List_Destroy();
FXY_List_t*      FXY_List_Expand();
FXY_Entry_t*     FXY_List_First();
int              FXY_List_Get();
int              FXY_List_Insert();
FXY_Entry_t*     FXY_List_Last();
FXY_Entry_t*     FXY_List_Prev();
void             FXY_List_Print();
int              FXY_List_Put();
FXY_Entry_t*     FXY_List_Remove();
int              FXY_List_Size();

int      HexStr_Destroy();
int      HexStr_Get();
int      HexStr_GetBit();
HexStr_t HexStr_Set();
HexStr_t HexStr_RVSet();
int      HexStr_SetBit();
int      Get_BUFR_Edition ();
int      Get_Century ();
int      Get_DataCategory ();
int      Get_DataSubCategory();
int      Get_Day();
int      Get_Hour();
int      Get_HourMinute();
int      Get_BUFR_MasterTableNum();
int      Get_ObservedDataFlag();
char*    Get_OriginatingCenter();
int      Get_OriginatingCenterID();
int      Get_SubCenter();
int      Get_UpdateSeqNum();
double   Get_Val_Double();
FXY_t    Get_Val_FXY();
int      Get_Val_Int();
int      Get_Val_Scale ();
char*    Get_Val_String ();
int      Get_year();
int      Get_YMD(Date_Format_t);

int Missing_Value_Replace();
int Is_Val_Missing ();
int Set_Center_Info( );
int Set_DataCategory ( );
int Set_Date( );
int Set_Edition_MasterTab ( );
int Set_MasterTableVer ( );
int Set_ObservedData ( );
int Set_Time( );
int Set_UpdateSequence ( );

void  Table0_Init();
void  Table0_Destroy();
void  Table0_Print();
int   Table0_Read();
Table0_t  Table0_Value();

void  TableA_Init();
void  TableA_Destroy();
void  TableA_Print();
int   TableA_Read();
TableA_t  TableA_Value();

int   TableB_Init();
void  TableB_Destroy();
Descriptor_t*     TableB_Get();
Units_t           TableB_GetUnits();
void      TableB_Print();
int       TableB_Put();
int       TableB_Read();

int       TableD_Init();
void      TableD_Destroy();
int       TableD_Expand();
TableD_Sequence_t*     TableD_Match();
void      TableD_Print();
int       TableD_Read();

int   TableC_Find();

TableD_Sequence_t* TableD_Sequence_Init();
void               TableD_Sequence_Destroy();
int                TableD_Sequence_Put();

int  ValStack_Init();
void ValStack_Destroy();
void ValStack_Clear();
int  ValStack_Get();
int  ValStack_IsEmpty();
int  ValStack_Pop();
int  ValStack_Push();

int  BytesInBits();
int  FileExists();
int  Find_Status( );
int  BUFR_Find_End(  );
int  Set_Status( );
int  Int3ToInt();
int  Int2ToInt();
Int2_t IntToInt2();
Int3_t IntToInt3();
void PrintDivider();
void Flag_Print();
FILE *Get_BUFR_Log_File_Ptr();
char *Get_BUFR_Log_File();
double TenPow();
double TruncateValue();
double TwoPow();
void VoidInc();
int  VoidVal();
int  Set_Flag();

/* Functions written by Valerie Pastor of SAIC. */

void BUFR_Set_Missing_Value( );
void BUFR_Turn_Off_Multiple_Msg_Flag();
int Find_Data_Type();
int message_read( );
int make_name();
int write_msg();
int mess_write();
int Loop_T_Data_Type();
int Num_Messages();
void dump_print();
int Check_Compress();
int D_Compression();
int Compression( );
void Set_Compress( );
void Prn_Comprss();
int  FXY_IsLocal();

#endif

#endif      /* #ifndef _MEL_BUFR_FUNCTIONS_H */
