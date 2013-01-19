/* these are functions that are called/shared within the AODT library */
/* defined in odtfuncs.c */
extern double aodtv72_calctime( int, int );
extern float aodtv72_slopecal( double,int );
extern float aodtv72_getpwval( int,float );
extern float aodtv72_PWlandsea( float );
extern float aodtv72_ptovmax( float );
extern float aodtv72_ptotno( float );
extern int aodtv72_cmonth2julian( char * );
extern void aodtv72_julian2cmonth( int, char * );
extern void aodtv72_distance( float,float,float,float,int,float *,float * );
extern void aodtv72_distance2( float, float, float, float, float *, float * );
extern float aodtv72_atoif( char *, int, int );
extern void aodtv72_calcskew( float *, int, float *, float *, float * );
extern int aodtv72_idmyyd( int,int,int );
extern void aodtv72_yddmy( int,int *,int *,int * );
extern int  aodtv72_oceanbasin( float,float );
extern int  aodtv72_sattypes( int,char * );
extern int aodtv72_initcurrent(int);
extern int aodtv72_rmw(float *,float *);
extern int aodtv72_atcffilename(char *,int,char *);


/* defined in odtintensity.c */
extern int aodtv72_calcintensity(void);

/* defined in odtscene.c */
extern void aodtv72_classifyredo(void);

/* defined in odtfft.c */
extern int aodtv72_fft( float *, float *, int * );

/* defined in odtauto.c */
extern void aodtv72_logspiral( float, float, float, int, int *, float *, float * );

/* defined in odthistory.c */
extern int aodtv72_datetime(int,int,char *,char *,int);
extern int aodtv72_deletehistoryrec(int *);
extern int aodtv72_listhistory(struct odtdata *,int,char *,char *,char *);
