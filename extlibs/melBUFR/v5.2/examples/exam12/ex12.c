/*
 * EXAMPLE 12: Change data width and scale with in BUFR_Put_Array call.
 * This uses the descriptor 2-01-xxx explicity.  In Example 11 the use of
 * descriptor is hidden from the user by the call to Change_DataWidth.
 * This descriptor changes the data width for all not descritpors that do
 * have an ASCII data type.  The change is specified in the xxx field
 * as a delta of +/-d bits and is bised by a value of 128.  A positive d
 * increases the data width and a negative data width decreases it.
 * To increase the data width by two bits the xxx field would be 130. 
 * The data width change stays in effect until canceled.
 * To cancel the increase in data width the xxx set set to 000 (2-01-000).
 * This causes the data width to revert back to the default value.
 * As an example if you want to encode a sequence of dry bulb temperature
 * wind speed and wind direction, and wanted to increase the data width
 * of the temperature field by 1 bit, but not change the data width of the 
 * wind variables. The FXY and data arrays should look something like
 * the folowing.
 *
 *  -- exam12a --
 *         FXY         DATA ARRAY
 *       0-05-002      latitude
 *       0-06-002      longitude
 *       0-04-004      hour
 *       2-01-129      increase data width 1 bit - no corresponding entry
 *       0-22-039      meteorologicl residual tidal elevation
 *       2-01-000      cancel data width change - no corresponding entry
 *       0-04-004      hour
 *       2-01-130      increase data width 1 bit - no corresponding entry
 *       0-22-039      meteorologicl residual tidal elevation
 *       2-01-000      cancel data width change - no corresponding entry
 *
 *
 *  -- exam12b -- 
 *         FXY         DATA ARRAY
 *          .              .
 *          .              .
 *       0-04-004      hour
 *       2-01-131      increase data width 1 bit - no corresponding entry
 *       2-02-129      increase scale by 10 - no corresponding entry
 *       0-12-001      temperature
 *       2-01-000      cancel data width change - no corresponding entry
 *       2-02-000      cancel increase scale by 10 - no corresponding entry
 *       0-11-001      wind direction
 *       0-11-002      wind speed
 *       0-04-004      hour
 *       2-01-131      increase data width 1 bit - no corresponding entry
 *       2-02-129      increase scale by 10 - no corresponding entry
 *       0-12-001      temperature
 *       2-01-000      cancel data width change - no corresponding entry
 *       2-02-000      cancel increase scale by 10 - no corresponding entry
 *       0-11-001      wind direction
 *       0-11-002      wind speed
 *          .              .
 *          .              .
 *
 *  -- exam12c --
 *         FXY         DATA ARRAY
 *          .              .
 *          .              .
 *       0-04-004      hour
 *       0-12-001      temperature
 *       0-11-001      wind direction
 *       0-11-002      wind speed
 *       0-04-004      hour
 *       0-12-001      temperature
 *       0-11-001      wind direction
 *       0-11-002      wind speed
 *          .              .
 *          .              .
 *
 *  -- exam12d --
 *         FXY         DATA ARRAY
 *          .              .
 *          .              .
 *       1-08-002      replication of 8 descriptors two time
 *       0-04-004      hour
 *       2-01-131      increase data width 1 bit - no corresponding entry
 *       2-02-129      increase scale by 10 - no corresponding entry
 *       0-12-001      temperature
 *       2-01-000      cancel data width change - no corresponding entry
 *       2-02-000      cancel increase scale by 10 - no corresponding entry
 *       0-11-001      wind direction
 *       0-11-002      wind speed
 *
 *  The above examples corresponds examples a, b, c, and d.  Example a changes the
 *  data width of the 0-22-39 descriptor.  The default data width of 12 is not
 *  large enough to encode numbers greateg than -.904.  Therefor zero and 
 *  positive elevations could not be encoded.  By increasing the data width
 *  by two bits positive elevations up to +11.384 meters using the same 
 *  referance value of -5000.  A 13 bit data width would have allowed poistive
 *  values up to 3.192.  Notice that the change in data width is canceled
 *  before the hour is encoded for the second point (time).
 *
 *  Example b we change both the data width and scale to allow for more
 *  accuracy in the temperature.  The data width had to be increased by 3 bits
 *  to handle the increase in scale.
 *
 *  Example c is the same as b except no increases were made to the data width
 *  and scale.  The data input file is the same for both, but when the data is
 *  dumped the decoded temperature has one less decimal of accuracy in
 *  example c.
 *
 *  Example d is also the same as example b except a replication descriptor
 *  is used to replicate the hour, temperature, wind direction, and wind
 *  speed.  Note that the descriptors used to change scale and data width
 *  are included in the number of descriptors to be replicated.
 *
 *  To execute the example enter
 * 
 *      example11 {a,b,c,d}
 */

#include <mel_bufr.h>           /* BUFR library include file */

#if PROTOTYPE_NEEDED

int main(int argc, char *argv[])
 
#else
 
int main( argc, argv )
int   argc;
char* argv[];
 
#endif
{
    BUFR_Info_t bufr_info;  /* Largely Section 1 information. */
    float *values;        /* array of data values  */
    int   *fxy_i;         /*  array of decimal FXYs  */
    FXY_t *fxy;           /*  array of packed FXYs   */
    int   num_vals;         /*  number of values in array */
    int   i;                /*  loop counter */
    char  fxy_file[40];
    char  data_file[40];
    char  enc_file[30];
    float *fpr;
    int   *ipr;
    FXY_t *xpr;
    FILE  *fd;
    int   NUM_FXYS;         /* number of FXYs          */
 
    /* test for correct numbner of command line arguments */
    if ( argc != 2) {
      printf(" ********************************\n");
      printf("   To execute example12 enter \n");
      printf("  >example11 [a,b]\n\n");
      printf("  a = data set a (change residual tidal height datawidth\n");
      printf("  b = data set b (change temperature datawidth and scale\n");
      printf("  c = data set c (no changes)\n");
      printf("  d = data set d (change temperature datawidth, scale, with replication\n");
      printf(" ********************************\n");
      exit(1);
    }

    printf("\n **************************** \n");
    printf(" *        EXAMPLE 12        *\n");
    printf(" *    Changing datawidth    *\n");
    printf(" *      and scale using     *\n");  
    printf(" *       BUFR_Put_array     *\n");
    printf(" *       --------------     * \n"); 
    switch ( *argv[1]){
      case 'a':
         printf(" *          CASE a          *\n");
         printf(" *     change datawidth     *\n");
         break;
      case 'b':
         printf(" *          CASE b          *\n");
         printf(" * change datawidth & scale *\n");
         break;
      case 'c':
         printf(" *          CASE c          *\n");
         printf(" *        no changes        *\n");
         break;
      case 'd': 
         printf(" *         CASE d           *\n");
         printf(" * change datawidth & scale *\n");
         printf(" *     with replication     *\n");
         break;
      default:
         printf("\n  <<<<<< ERROR >>>>>>\n");
         printf("   To execute example12 enter \n");
         printf("  >example11 [a,b]\n\n");
         printf("  a = data set a (change residual tidal height datawidth\n");
         printf("  b = data set b (change temperature datawidth and scale\n");
         printf("  c = data set c (no changes)\n");
         printf("  d = data set d (change temperature datawidth, scale, with replication\n");
         printf(" ********************************\n");
         exit(1);     
    }
    printf(" *                          * \n"); 
    printf(" *    FXY_Pack_Dec          *\n"); 
    printf(" *    BUFR_Put_Array        *\n");   
    printf(" **************************** \n"); 

    /* build file name for fxy file */
    strcpy(fxy_file,"exam12");
    strcat(fxy_file,argv[1]);
    strcat(fxy_file,".fxy");

    /* build file name for data file */
    strcpy(data_file,"exam12");
    strcat(data_file,argv[1]);
    strcat(data_file,".dat");

    /* build output file name  */
    sprintf(enc_file,"ex12%s.enc",argv[1]);
     
    /********************************************************/
    /*             read FXYs and data                       */
    /********************************************************/

    /* open FXY file */
    fd = fopen(fxy_file,"r");

    /* how many FXYs ?? */
    fscanf(fd,"%d",&NUM_FXYS);
    printf(" NUM_FXYS = %d\n",NUM_FXYS);

    /* allocate memory of FXY input */
    fxy_i = (int *)malloc ( (uint_t)NUM_FXYS * sizeof(int));
    
    /* allocate memory for packed FXYs */
    fxy = (FXY_t *)malloc( (uint_t)NUM_FXYS * sizeof(FXY_t));
    
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

    /* read number of data values to be read  */  
     fscanf(fd,"%d",&num_vals);
    
    values = (float *)malloc( sizeof(float) * (uint_t)num_vals);
    fpr = values;
    /*****  end memory allocation *****/
    
    /* read data sets */
    printf(" echo of data read\n");
    for ( i=0; i< num_vals; i++){
       fscanf(fd,"%f",fpr);
       printf(" i=%d  value=%.3f\n",i,*fpr++);
    }
    
    fclose(fd);
 
    /************************************************************/
    /* enter values into message */
    
    /* Initialize BUFR message.  Read standard tables. */

    /* Initialize the BUFR information structure. */

    if( BUFR_Info_Init( &bufr_info ) )
    {
        BUFR_perror( "main" );
        return 1;
    }

    bufr_info.BUFR_MasterTable            =  0; /* Use WMO standard */
    bufr_info.BUFR_Edition            =  3; /* Use WMO standard */
    bufr_info.OriginatingCenter           = 58; /* FNOC Monterey, CA */
    bufr_info.UpdateSequenceNumber        =  0; /* 0 for original message */
    bufr_info.DataCategory                =  0; /* Surface land data */
    bufr_info.DataSubCategory             =  0; /* BUFR message sub-type */
    bufr_info.VersionNumberOfMasterTables = 5;
    bufr_info.VersionNumberOfLocalTables  =  0;
    bufr_info.MinorLocalVersion          = 0;
    bufr_info.Year                        = 95;
    bufr_info.Month                       = 12;
    bufr_info.Day                         = 31;
    bufr_info.Hour                        = 23;
    bufr_info.Minute                      = 59;
    bufr_info.ObservedData                =  1;
  
    Set_Flag(NoAuto_FTP);
    Set_Flag(Allow_Dup_Tab_Entry);
    Set_Flag(No_Warn_Dup_Tab_Entry);
    
    if( BUFR_Init( &bufr_info,enc_file, ENCODING) )
    {
        BUFR_perror( "main" );  /* Print reason for failure to stderr. */
        return 1;
    }
   
    /* pack FXYs for data */
    ipr = fxy_i;
    xpr = fxy;
    for ( i=0; i<NUM_FXYS; i++)
       *xpr++ = FXY_Pack_Dec( *ipr++);

    /* Enter array of data into bufr message */
    if ( BUFR_Put_Array( values, num_vals, DT_FLOAT, fxy, NUM_FXYS) ){
      BUFR_perror(" Error on call to BUFR_Put_Array in main");
      return 1;
    }


    /* create message  */
    if( BUFR_Encode(  &bufr_info ) )
    {
        BUFR_perror( "main" );
        BUFR_Destroy(1);
        return 1;
    }
    free (fxy);
    free (fxy_i);
    free (values);
    BUFR_Destroy(1);

    return 0;
}
