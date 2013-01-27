/*
 *   Copyright 1999, University Corporation for Atmospheric Research
 *   Do not redistribute without permission.
 *
 *   dcncprof.c
 *      Generic netCDF format COSMIC decoder. See usage information
 *   for options.
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <string.h>

#include "ulog.h"
#include <mkdirs_open.h>

#ifdef NO_ATEXIT
#include <atexit.h>
#endif

#include "cosmic.h"

#define HEX80	0x80808080
#ifdef MCIDAS
#define MISS	HEX80
#else
#define MISS	-9999L
#endif

int filebin=0;


static void usage(char *argv0)
{
(void)fprintf(stderr,
	"Usage: %s [options] output_file\t\nOptions:\n", argv0);
(void)fprintf(stderr,
	"\t-h           Print the help file, then exit the program\n") ;
(void)fprintf(stderr,
	"\t-f filename  Input NetCDF file (default is stdin)\n") ;
(void)fprintf(stderr,
	"\t-n filename  Output NetCDF file name (default not saved)\n") ;
(void)fprintf(stderr,
	"\t             [-n is ignored if -f is used]\n");
(void)fprintf(stderr,
	"\t-l filename  Log output (default uses local0, '-' to stdout\n");
(void)fprintf(stderr,
	"\t-v           Verbose logging\n");
(void)fprintf(stderr,
        "\t-x           Debug logging\n");
(void)fprintf(stderr,
        "\t-e VAR=value	Set an environmental variable in program\n");
(void)fprintf(stderr,
	"\t-b filebin   When NN template is used, filebin is minutes to put in single file\n");
exit(100);
}




/*
 * Called at exit.
 * This callback routine registered by atexit().
 */
static void cleanup()
{
uinfo("Exiting") ;
(void) closeulog();
}


typedef struct envlist{
   char *env;
   struct envlist *next;
   }envlist;


int main(int argc, char *argv[])
{

int in_file;
int out_file;
char *buf;
char *infilnam = 0;
char *outfilnam = 0;
char *logfname = 0;
char *ofil,*tempnam,*dpos;

envlist *envhead=NULL,*envobj;

extern int optind;
extern int opterr;
extern char *optarg;
int ch;
int logmask = (LOG_MASK(LOG_ERR) | LOG_MASK(LOG_NOTICE));

int rlen;
int DO_REMOVE=0;

mode_t omode=S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH;

cosmic_struct *head=NULL;
int iret;

/*
 * register exit handler
 */
if(atexit(cleanup) != 0)
   {
   serror("atexit");
   exit(1);
   }

opterr = 1;
while ((ch = getopt(argc,argv,"hvxl:f:n:e:b:")) != EOF)
   {
   switch(ch)
      {
      case 'h':
         usage(argv[0]);
         break;
      case 'f':
         infilnam = optarg;
         break;
      case 'n':
         outfilnam = optarg;
         break;
      case 'l':
         logfname = optarg;
         break;
      case 'v':
         logmask |= LOG_MASK(LOG_INFO);
         break;
      case 'x':
         logmask |= LOG_MASK(LOG_DEBUG);
         break;
      case 'e':
         envobj = (envlist *)malloc(sizeof(envlist));
         envobj->env = malloc(strlen(optarg)+1);
         strcpy(envobj->env,optarg);
         envobj->next = envhead;
         envhead = envobj;
         break;
      case 'b':
	 iret = sscanf(optarg,"%d",&filebin);
         if(iret < 1) usage(argv[0]);
	 break;
      case '?':
         usage(argv[0]);
      }
   }

switch(argc - optind)
   {
   case 1:
      ofil = argv[optind];
      break;
   default:
      usage(argv[0]);
   }

(void) setulogmask(logmask);
(void) openulog(ubasename(argv[0]), (LOG_CONS|LOG_PID), LOG_LOCAL0, logfname) ;
uinfo("Starting up");


envobj = envhead;
while(envobj != NULL)
   {
   udebug("putenv %s\0",envobj->env);
   if(putenv(envobj->env) != 0)
      uerror("Can't set environmental variable %s\0",envobj->env);
   envobj = envobj->next;
   }


/* if input from stdin, redirect to netcdf file "outfilnam" */
if(infilnam == 0)
   {
   if(outfilnam == 0) /* create a temporary file name */
      {
      tempnam = (char *)malloc(19);
      tempnam[0] = '\0';
      sprintf(tempnam,".tmp_netcdf.XXXXXX\0");
      mkstemp(tempnam);
      udebug("creating temporary netcdf file %s\0",tempnam);
      /* create temp file in directory of output  (get path to ofil)*/
      dpos = strrchr(ofil,'/');
      if(dpos == NULL)
         outfilnam = tempnam;
      else
         {
         outfilnam = (char *)malloc(strlen(ofil)+20);
         outfilnam[0] = '\0';
         strncat(outfilnam,ofil,dpos-ofil);
         strncat(outfilnam,"/",1);
         strcat(outfilnam,tempnam);
         }
      DO_REMOVE = 1;
      udebug("look %s\0",outfilnam);
      }
   in_file = STDIN_FILENO;
   out_file = mkdirs_open(outfilnam,O_WRONLY|O_CREAT|O_TRUNC,omode);
   if(out_file < 0)
      {
      uerror("could not open %s\n",outfilnam);
      exit(-1);
      }
   buf = (char *)malloc(8196);
   while((rlen = read(in_file,buf,8196)) > 0)
      {
      udebug("writing %d bytes\0",rlen);
      write(out_file,buf,rlen);
      }
   free(buf);
   close(out_file);
   infilnam = outfilnam;
   udebug("now will input netcdf from %s\0",infilnam);
   }
else
   {
   udebug("will get input from %s\0",infilnam);
   }

/* call netcdf decoder with infilnam */
head = decode_cosmic(infilnam,MISS,&iret);

/* we are done with netcdf file now, remove if necessary */
if(DO_REMOVE == 1)
  { 
  if(unlink(outfilnam) != 0) uerror("Could not delete temp file\0");
  }

if((head != NULL)&&(iret == 0))
   {
/* call package specific storage with ofil */

#ifdef GEMPAK
   write_gempak(ofil,head,logfname,&iret);
#endif
#ifdef MCIDAS
   write_mcidas(ofil,head,&iret);
#endif

   }
else
   uerror("could not decode data %d\0",iret);


exit(0);
}
