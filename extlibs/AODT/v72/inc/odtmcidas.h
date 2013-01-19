/* external McIDAS application functions defined in odtmcidas directory */
extern int mcidas_initenv(int, char **);
extern int mcidas_getpaths(char *,char *,char *,char *,char *,char *);
extern int mcidas_setpaths(char *,char *,char *,char *,char *,char *);
extern int mcidas_setgraphoptions(int,int,int,int,int,int,int,int,int,int,int,int);
extern int mcidas_getinputs(int, char **,
                  int *,int *,int *,int *,int *,int *,
                  int *,int *,int *,int *,int *,
                  int *,int *,int *,int *,int *,
                  int *,int *,int *,int *,int *,int *,
                  int *,int *,int *,int *,int *,int *,int *,int *,
                  int *,int *,int *,int *,int *,
                  float *,
                  char *,char *,char *,char *,char *,char *,char *,char *,char *,
                  char *,char *,char *,char *);
extern int mcidas_getcursorloc(char *,float *,float *);
extern int mcidas_getsatdatainfo(char *,int,char *,int *,int *,int *);
extern int mcidas_getsatimagedata(char *,float,float,int,float **,float **,float **,int *,int *);
extern int mcidas_overridescenetype(int *,int *,int *,int *);
extern int mcidas_graphhistoryrec(void);
extern void mcidas_qmessage(int,int,char *,char *);
extern int mcidas_writegrid(char *,int,float ***,int,int,int,int);
