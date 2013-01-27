typedef struct nldn_file 
{
char *gemfil;
int iflsrc;
int maxtim;
char *curtim;
char *ifltyp;
int itype, ifactor;
int ibin;
} nldn_file;


typedef struct nldn_flash
{
int sec,nsec;
int mult;
float lat,lon,sgnl,semimaj,eccent,angle,chisqr;
} nldn_flash;

void	dcnldn_input(nldn_file ltgf, int *retval);
void    decode_nldn (char *curtim, char *gemfil, char *prmfil, char *filetyp, 
                int maxtim, int nhours, int *iret);
void	nldninit ( );
void	nldnflush (int *iflno, int *iret);
int	write_nldn(nldn_file ltgf, nldn_flash flashdat, int *iret);

void	sfnldn ( int *ISFFLN, int *IDATE, int *ITIME, char *STID, float *SLAT, 
		 float *SLON, int *IHHMM, float *SFDATA, int *IRET, size_t );
void	setcparm ( char CPARM[][4], int *NPARM, int *JPARMS, size_t);
