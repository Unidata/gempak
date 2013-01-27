#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <bzlib.h>
#include "mkdirs_open.h"

#include "geminc.h"
#include "gemprm.h"
#include "bridge.h"
#include "dccmn.h"

int bufread(int fd, char *buf, int bsiz, int *blen);


struct packet {
  short junk1[6];
  unsigned short size;
  unsigned char id, type;
  unsigned short seq, gen_date;
  unsigned int gen_time;
  unsigned short num_seg, seg;
  unsigned int coll_time;
  unsigned short coll_date, range, angle, radial, rad_status, elev_angle;
  unsigned short elev_num;
  short first_refl, first_dopp;
  unsigned short refl_size, dopp_size;
  unsigned short num_refl_gate, num_dopp_gate, sector;
  float gain;
  unsigned short refl_ptr, vel_ptr, spec_ptr, dopp_res, pattern;
  short junk2[4];
  unsigned short refl_ptr_rda, vel_ptr_rda, spec_ptr_rda, nyquist, atten;
  short thresh;
  short junk3[17];
  unsigned char data[2304];
  float dbz[460];
};

static char *compression_type = "BZIP2";


/*
   nexradII outfile
*/
static void
usage(
	char *av0 /*  id string */
)
{
  (void)fprintf(stderr,
		"Usage: %s [options] [filename]\t\nOptions:\n", av0);
  (void)fprintf(stderr,
		"\t-v		Verbose, tell me about each product\n");
  (void)fprintf(stderr,
		"\t-l logfile	Default logs to syslogd\n");
  (void)fprintf(stderr,
		"\t-C type	Compression BZIP2)\n");
  (void)fprintf(stderr,
		"\t-f           Filter out type 2 radials with status 28\n");
  (void)fprintf(stderr,
		"\t-s STID      Set bytes 21-24 with STID\n");
	exit(1);
}

char filnam[DCMXLN+1]="";
char *filptr=NULL;

static void cleanup()
{
char command[1024];
if(filptr != NULL) {
   sprintf(command,"mv %s %s\0",filnam, filptr);
   system(command);
   }
}

int main(int argc, char *argv[], char *envp[])
{
    char clength[4];
    char *block = (char *)malloc(8192), *oblock = (char *)malloc(262144);
    unsigned isize = 8192, osize=262144, olength;
    int length, go;
    int compress = 0;
    char *logfname = "";
    int bzip2 = 1;
    int filter = 0;
    int fd;
    char stid[5]={0};

#define NUMEXP  1
        int     nexp    = NUMEXP;
        char    *prgnam = "DCNEXR2";
	char	*defprm = " ";
	char	*defstn = " ";
	char	*dfstn2 = " ";
	int	idfadd=0;
	int	idfmax=0;
	int	ndfhr1=0;
	int	ndfhr2=0;
	int     idfwdh  = 0;

/*
**      Do not change these variables. These variables are used by all
**      decoders for getting the command line parameters.
*/
        char    parms[NUMEXP][DCMXLN], newfil[DCMXLN], curtim[DCMXLN];
        int     i, j, num, iret, ier;

	char    stntbl[DCMXLN], stntb2[DCMXLN], prmfil[DCMXLN];
        int     iadstn, maxtim, nhours, txtflg, crcflg, iwndht;

        char    errstr[DCMXLN];
	extern int optind, opterr;
	extern char *optarg;
	int ch;
	long flen;

	if(atexit(cleanup) != 0)
	   {
	   printf("could not register exit routine\n");
	   }
/*
**      Initialize the output logs, set the time out and
**      parse the command line parameters. dc_init calls in_bdta in 5.4.3+.
*/

	/*
	 * process some extra arguments "fC:" for this routine
	 */

	num = argc;
	i = 1;
	while ( i < num )
	   {
	   if ( strcmp(argv[i], "-f") == 0 )
	      {
	      num--;
	      filter = 1;
	      for ( j = i+1; j < argc; j++ )
		 argv[j-1] = argv[j];
	      continue;
	      }
	   if ( strcmp(argv[i], "-C") == 0 )
	      {
	      /* optional compression specification for future use */
	      num-=2;
	      for ( j = i+2; j < argc; j++ )
		 argv[j-2] = argv[j];
	      continue;
	      }
	   i++;
	   }

	argc = num;

        dc_init ( prgnam, argc, argv, nexp, parms, &num, &iret );

/*
**      Check for an initialization error.
**      On an error, exit gracefully.
*/
	if  ( iret == -11 ) {
	   fd = 1;
	}
        else if  ( iret < 0 ) {
            sprintf ( errstr, "Error initializing\0" );
            dc_wclg ( 0, "DCNEXR2", iret, errstr, &ier );
            dc_exit ( &iret );
        }
	else {
	    char *cpos = strrchr(parms[0],'/');

	    if ( cpos == NULL )
	        sprintf(filnam,".%s\0",parms[0]);
	    else {
		strncpy(filnam,parms[0],cpos - parms[0]+1);
		strncat(filnam,".",1);
		strcat(filnam,cpos+1);
	    }

	    cfl_inqr ( parms[0], NULL, &flen, newfil, &iret );
	    if(iret != 0)
		{
		filptr = parms[0];
		strcpy(newfil,filnam);
		sprintf ( errstr, "new output file %s\0",newfil);
                dc_wclg ( 0, "DCNEXR2", iret, errstr, &ier );
		}

	    if ((fd=mkdirs_open(newfil,O_WRONLY | O_CREAT, 0664)) == -1) {
	            sprintf ( errstr, "Cannot open %s\0", newfil);
	            iret = -10;
	            dc_wclg ( 0, "DCNEXR2", iret, errstr, &ier );
	            dc_exit( &iret );
	            }

	    lseek(fd, 0, SEEK_END);
        }

/*
**      Set the decoder parameters to the command line entries or
**      default values.
*/
        dc_gopt ( defprm, defstn, dfstn2, idfadd, idfmax, ndfhr1, ndfhr2,
                  idfwdh, prmfil, stntbl, stntb2, &iadstn, &maxtim, curtim, &nhours,
                  &txtflg, &crcflg, &iwndht, &iret );

        if(stntbl[0] != '\0')
           {
	   strncat(stid,stntbl,4);
	   sprintf ( errstr, "STID set to %s\0", stid );
	   dc_wclg ( 2, "DCNEXR2", iret, errstr, &ier );
	   }


    /*
     * set up signal handlers
     */
    go = 1;
    while (go) {

	/*bufread(0, (char *)(&length), 4, &i);*/
	bufread(0, clength, 4, &i);
	if (i != 4) {
	    if (i > 0) {
		sprintf ( errstr, "Short block length\0");
		iret = -5;
		dc_wclg ( 0, "DCNEXR2", iret, errstr, &ier );
            }
	    else
		iret = 0;
	    dc_exit(&iret);
	}

	if ( (memcmp(clength, "ARCH", 4)==0) ||
	     (memcmp(clength, "AR2V", 4)==0) ) {
	    memcpy(block, clength, 4);
	    bufread(0, block+4, 20, &i);
	    if (i != 20) {
		sprintf ( errstr, "Missing header\0");
		iret = -5;
		dc_wclg ( 0, "DCNEXR2", iret, errstr, &ier );
		dc_exit( &iret);
	    }

	    if ( stid[0] != 0 ) memcpy(block+20,stid,4);
	    lseek(fd, 0, SEEK_SET);
	    write(fd, block, 24);
	    continue;
	}

	length = 0;
	for(i=0;i<4;i++)
	   length = ( length << 8 ) + (unsigned char)clength[i];

	if(length < 0) {
	    sprintf ( errstr, "EOF %ld\0",length);
	    iret = -9;
	    dc_wclg ( 2, "DCNEXR2", iret, errstr, &ier );
	    length = -length;
	    go = 0;
	}
	if (length > isize) {
	    isize = length;
	    sprintf ( errstr, "Expanding input buffer to %d\0", isize);
	    iret = -7;
	    dc_wclg ( 2, "DCNEXR2", iret, errstr, &ier );
	    if ((block = (char *)realloc(block, isize)) == NULL) {
	        sprintf ( errstr, "Cannot allocate input buffer\0");
		iret = -4;
	        dc_wclg ( 0, "DCNEXR2", iret, errstr, &ier );
		dc_exit( &iret );
	    }
	}
        sprintf(errstr, "read block size %d\0",length);
	dc_wclg ( 4, "DCNEXR2", 0, errstr, &ier );
	bufread(0, block, length, &i);
	if (i != length) {
	    sprintf ( errstr, "Short block read\0");
	    iret = -5;
	    dc_wclg ( 0, "DCNEXR2", iret, errstr, &ier );
	    dc_exit( &iret );
	}
	if (length > 10) {
	    int error;

	tryagain:
	    olength = osize;
	    if (bzip2 == 1)
#ifdef BZ_CONFIG_ERROR
		error = BZ2_bzBuffToBuffDecompress(oblock, &olength,
		/*error = bzBuffToBuffDecompress(oblock, &olength,*/
#else
		error = bzBuffToBuffDecompress(oblock, &olength,
#endif
					       block, length, 0, 0);
	    if (error) {
		if (error == BZ_OUTBUFF_FULL) {
		    osize += 262144;
		    sprintf(errstr, "Expanding output buffer to %d\0", osize);
		    iret = -7;
	            dc_wclg ( 2, "DCNEXR2", iret, errstr, &ier );
		    if ((oblock=(char*) realloc(oblock, osize)) == NULL) {
			sprintf(errstr, "Cannot allocate output buffer\0");
			iret = -4;
	                dc_wclg ( 0, "DCNEXR2", iret, errstr, &ier );
			dc_exit(&iret);
		    }
		    goto tryagain;
		}
		sprintf(errstr, "decompress error - %d\0", error);
		iret = -5;
	        dc_wclg ( 0, "DCNEXR2", iret, errstr, &ier );
		dc_exit( &iret);
	    }
	    if (filter) {
	        int i;

		for (i=0; i < olength; i += 2432) {
		    struct packet *packet=(struct packet *) (oblock+i);

		    if (packet->type != 2 || packet->rad_status != 28)
			write(fd, oblock+i, 2432);
		}
	    }
	    else write(fd, oblock, olength);
	}
    nbull++;
    }
    close(fd);
    iret = 5;
    dc_exit(&iret);
}
