/*
 * EXAMPLE 14: This example is similiar to Example 13, except that two
 * BUFR_Put_* functions are called per dataset.  One is used to enter a 
 * a character string data and the other to enter numeric data. 
 *
 *************************
 * THe following paragraphs are coppied from Example 13.

 * Templates is the
 * method used by the library to define BUFR data subsets.  In BUFR, a data 
 * subset is defined as "the subset of data described by one single application
 * of" the collection of descriptors defined in Section 3 of the BUFR message.
 *
 * Templates are used to indicate to the library that the definition of the
 * descriptor sequence has been completed and that all subsequence data entry 
 * will use the same descriptor sequence.  There are two ways to define a
 * "template" (data subset).  Both approaches make use of the function
 *
 *             BUFR_Define_Dataset( FXY_T *fxy_array, int num_fxys)
 *
 * PREDEFINED: In the first approach, the fxy_array is defined and a call to
 * BUFR_Define_Dataset is made after the call to BUFR_Init and before any data 
 * has been entered.  This is the only time this function is called.  In all
 * subsequence calls to enter data into the message using BUFR_Put_* functions,
 * the associated FXY arhuments are ignored.  The user must ensure that a 
 * complete sets of data are entered.
 *
 * ON_THE_FLY: In the second approach, the data is entered normally for the first data
 * subset.  After the first data subset has been entered, a call is made to 
 * BUFR_Define_Dataset with NULL values for both arguments.  This informs the
 * library to use the FXY sequence defined by the previous BUFR_Put_* calls 
 * to define the FXY sequence.  In all subsequence calls to enter data into 
 * the message using BUFR_Put_* functions, the associated FXY arhuments are
 * ignored.  Also any subsequence calls to BUFR_Define_Dataset are ignored.
 *
 **********
 *
 * This example is based on Example 13 (and Example 3) and uses the second
 * (ON_THE_FLY) approach.  The aircraft registration number (FXY = 0-01-008)
 * which has a data type of CCITT_IA5 has been added to the message and is
 * encoded using BUFR_Put_Array.   
 *
 * For an example of the first (PREDEFINED) approach for the same data, see Example 15.
 */

#include <mel_bufr.h>           /* BUFR library include file */

#if PROTOTYPE_NEEDED

int main( void )

#else

int main( )

#endif
{
    BUFR_Info_t bufr_info;  /* Largely Section 1 information. */
    float *values;        /* array of data values  */
    int   NUM_FXYS;         /* number of FXYs          */
    int   i,j;                /*  loop counter */
    char  tstr[65];
    FXY_t *fxy;
    int   *fxy_i;
    float *fpr;
    int   *ipr;
    FXY_t *xpr, *xpr2;
    FILE  *fd;
    char  fxy_file[40];
    char  encode_file[50];
    char  data_file[40];   
    int   num_str,  num_num, num_subsets;

    printf("\n *********************************** \n");
    printf(" *           EXAMPLE 14            *\n");
    printf(" *           TEMPLATES             *\n"); 
    printf(" *     (complex - on the fly)      *\n");
    printf(" *  (two+ BUFR_Put_* per dataset)  *\n");    
    printf(" *                                 *\n"); 
    printf(" *   BUFR_Define_Dataset           *\n"); 
    printf(" *   FXY_Pack_Dec                  *\n"); 
    printf(" *   BUFR_Put_Array                *\n");   
    printf(" *************************** ******** \n"); 

    /****  define file names  *****/

    strcpy(data_file,"exam14.dat");
    strcpy(fxy_file,"exam14.fxy");
    strcpy(encode_file,"ex14.enc");
    /********************************************************/
    /*             read FXYs and data                       */
    /********************************************************/

    /* open FXY file */
    fd = fopen(fxy_file,"r");

    /* how many FXYs ?? */
    fscanf(fd,"%d",&NUM_FXYS);
    printf(" NUM_FXYS = %d\n",NUM_FXYS);

    /* allocate memory of FXY input */
    fxy_i = (int *)malloc ( NUM_FXYS * sizeof(int));
    
    /* allocate memory for packed FXYs */
    fxy = (FXY_t *)malloc( NUM_FXYS * sizeof(FXY_t));
    
    /* read FXYs */
    printf(" echo of FXYs read\n");
    ipr = fxy_i;
    for (i=0; i<NUM_FXYS; i++){
      fscanf(fd,"%d",ipr);
      printf(" %06d\n",*ipr++);
      fflush(stdout);
    }
    /* close FXY file */
    fclose(fd);
    
    /* open data file */
    fd = fopen(data_file,"r");
    
    /* read number of char variables */
    fscanf(fd, "%d",  &num_str);
    
    /* read number of non characher variables */
    fscanf(fd, "%d",  &num_num);
    
    values = (float *)malloc(num_num * sizeof(float));
    /* read number of data subsets */
    fscanf(fd, "%d",  &num_subsets);
 
    /* Initialize the BUFR information structure. */

    if( BUFR_Info_Init( &bufr_info ) )
    {
        BUFR_perror( "main" );
        return 1;
    }

    bufr_info.BUFR_MasterTable            =  0; /* Use WMO standard */
    bufr_info.BUFR_Edition            =  3; /* Use WMO standard */
    bufr_info.OriginatingCenter           =128; /* NRL Monterey, CA */
    bufr_info.UpdateSequenceNumber        =  0; /* 0 for original message */
    bufr_info.DataCategory                =  0; /* Surface land data */
    bufr_info.DataSubCategory             =  0; /* BUFR message sub-type */
    bufr_info.VersionNumberOfMasterTables =  5;
    bufr_info.VersionNumberOfLocalTables  =  0;
    bufr_info.MinorLocalVersion           =  0;
    bufr_info.Year                        = 95;
    bufr_info.Month                       = 12;
    bufr_info.Day                         = 31;
    bufr_info.Hour                        = 23;
    bufr_info.Minute                      = 59;
    bufr_info.ObservedData                =  1;
 
    Set_Flag(NoAuto_FTP);
    Set_Flag(Allow_Dup_Tab_Entry);
    Set_Flag(No_Warn_Dup_Tab_Entry);
    
    /* Initialize BUFR message.  Read standard tables. */

    if( BUFR_Init( &bufr_info, encode_file, ENCODING ) )
    {
        BUFR_perror( "main" );  /* Print reason for failure to stderr. */
        return 1;
    }


    /*  pack FXYs using FXY_Pack_Dec */
    
    /* pack FXYs for data */
    ipr = fxy_i;
    xpr = fxy;
    for ( i=0; i<NUM_FXYS; i++)
       *xpr++ = FXY_Pack_Dec( *ipr++);

    /*  loop on number of data subsets */
    for (j=0; j< num_subsets; j++){
    printf(" data set  %d\n", j+1);
       /* enter character variable  */
        for ( i = 0;i<num_str; i++){
	  fscanf(fd, "%s", tstr);
          printf("%s\n",tstr);
	  if (BUFR_Put_Array(tstr, strlen(tstr), DT_CHAR, fxy, 1) ){
	    BUFR_perror(" Error on call to BUFR_Put_Array puting string");
            return 1;
          } 
	}
	
	/* enter remaining data for subset */
	/* read data */
	fpr = values;
	for ( i = 0; i<num_num; i++){
	    fscanf(fd, "%f", fpr);
	    fpr++; 
	}
        for (i = 0; i< num_num; i++){
          printf("%f\n",values[i]);
        }
	/* Enter array of data into bufr message */
	
	/* point to start of FXYs for float data */
	xpr2 = fxy + num_str;
        if ( BUFR_Put_Array(values, num_num, DT_FLOAT, xpr2, num_num) ){
           BUFR_perror(" Error on call to BUFR_Put_Array put floats");
           return 1;
        }
	
	/* define data subset */
	BUFR_Define_Dataset( NULL,  NULL);
    }

   /* create message  */
    if( BUFR_Encode( &bufr_info ) )
    {
        BUFR_perror( "main" );
        BUFR_Destroy(1);
        return 1;
    }

    BUFR_Destroy(1);

    return 0;
}
