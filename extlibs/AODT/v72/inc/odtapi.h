struct irdata {
  int date;
  int time;
  float TrawO;
  float Traw;
  float Tfinal;
  float Tfinal3;
  float CI;
  float eyet;
  float warmt;
  float cloudt;
  float cloudt2;
  float cwcloudt;
  float latitude;
  float longitude;
  float warmlatitude;
  float warmlongitude;
  float eyecdosize;
  float eyestdv;
  float cloudsymave;
  int sattype;
  int eyescene;
  int cloudscene;
  int eyesceneold;
  int cloudsceneold;
  int rule9;
  int rule8;
  int land;
  int eyefft;
  int cloudfft;
  int cwring;
  int ringcb;
  int ringcbval;
  int ringcbvalmax;
  float ringcblatmax;
  float ringcblonmax;
  float CIadjp;
  /*float sst;*/
  /*float TIEraw;*/
  /*float TIEavg;*/
  /*int   TIEflag;*/
  int autopos;
  int LBflag;
  int rapiddiss;
  float rmw;
  char comment[50];
};

struct odtdata {
  struct irdata IR;
  struct odtdata *nextrec; };

/* library functions defined in odtapi */
extern int aodtv72_sethistoryfile(char *);
extern int aodtv72_gethistoryfile(char *);
extern int aodtv72_setforecastfile(char *,int);
extern int aodtv72_getforecastfile(char *,int *);
extern int aodtv72_setdomain(int);
extern int aodtv72_getdomain(int *);
extern int aodtv72_setIRimageinfo(int,int,int);
extern int aodtv72_getIRimageinfo(int *,int *,int *,char *);
extern int aodtv72_setlocation( float, float, int );
extern int aodtv72_getlocation( float *, float *, int * );
extern int aodtv72_setmiscoptions(int,int,int);
extern int aodtv72_getmiscoptions(int *,int *,int *);
extern int aodtv72_setstartstr(int,float);
extern int aodtv72_getstartstr(int *,float *);
extern int aodtv72_setsstvalue(float);
extern int aodtv72_getsstvalue(float *);
extern int aodtv72_settopovalue(int);
extern int aodtv72_gettopovalue(int *);
extern int aodtv72_getversion(char *);

extern int aodtv72_runautomode1(float *,float *,int *);
extern int aodtv72_runautomode2(float,float,float *,float *,int *);   /* changed 1-10-05 */
extern int aodtv72_getwarmeyetemplocation(float *,float *);

extern int aodtv72_readtopofile(char *,float,float,int *);
extern int aodtv72_loadIRimage(float **,float **,float **,int,int);

extern int aodtv72_seteyecloudtemp(void);
extern int aodtv72_scenetype(void);
extern int aodtv72_intensity(void);
extern int aodtv72_getscenetypes(int *,int *,int *,int *);
extern int aodtv72_setscenetypes(int,int,int,int);
extern int aodtv72_scenemap(int,int,char *);

extern int aodtv72_setdatetime(int,int,char *,char *,int);
extern int aodtv72_historylistfmt(struct odtdata *,int,char *,char *,char *);
extern int aodtv72_historybullfmt(struct odtdata *,char *);
extern int aodtv72_historygetnextrec(int,struct odtdata **);
extern int aodtv72_historydeleterec(int *,int *);
extern int aodtv72_historyrecordinsert(int *,int *);
extern int aodtv72_historywritefile(int *);
extern int aodtv72_bulletinoutput(char []);
extern int aodtv72_historyaddcomment(char *,int *);

extern int aodtv72_atcfoutputfile(char *,int,char *,char *);

extern int aodtv72_qmessage(int,int,char *,char *);
extern int aodtv72_qdiagnostics(char *);

extern int aodtv72_initialize(void);
extern int aodtv72_freememory(void);
