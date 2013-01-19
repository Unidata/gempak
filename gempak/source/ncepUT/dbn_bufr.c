/*
 * dbn_bufr.c
 *
 * Distributed Brokered Networking (DBNet) bufr hook
 *
 * author: Luis J. Cano
 * date:   3/1/96
 *
 *
 * $Date: 1998/03/06 12:37:08 $
 * $Id: dbn_bufr.c,v 2.2 1998/03/06 12:37:08 dbnet-bl Exp $
 * $Log: dbn_bufr.c,v $
 * Revision 2.2  1998/03/06 12:37:08  dbnet-bl
 * Set the file mode when storing bufr file. Louie.
 *
 * Revision 2.1  1996/08/07 15:57:07  dbnet-bl
 * Fixed a bug where a new file as being created each time this
 * code was called, instead of having the caching required
 * caching effect. Louie.
 *
 * Revision 2.0  1996/08/07 14:31:29  dbnet-bl
 * initial DBNet 2.0 module checkin
 *
 *
 */


#define _POSIX_SOURCE 1

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>


#ifdef UNDERSCORE
#define dbn_bufr dbn_bufr_
#endif

#define DBNROOT_ENV "DBNROOT"
#define DBNBUFRT_ENV "DBNBUFRT"
#define DBNFMODE S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH
#define FN_LEN 256
/* type includes sitecode placeholer */
#define TMP_QUE_NAME "/tmp/BUFR.0."
#define DBNBUFRPL "/bin/dbn_bufr.pl"
#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif
#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif
#define START_DBNBUFRPL 0


/*
 * function prototypes
 */
static int BufrInit(void);
static int WriteToStream(char *subtype, unsigned char *msg, size_t mlen);
static void BuildFN( char *token, unsigned *cnt, int dot);


/*
 * file scope 
 */

static char *dbn_root=NULL;
static int dbn_cache_time=0;
static char *pending_path=NULL;
static unsigned pending_prefix_len;
static int inited=0;
static size_t msg_element_sz=4;

void dbn_bufr(char *subtype, int *subtypelen,
              unsigned char *msg, unsigned long *lenptr,
              int *iret                                   );

/*
 * main function
 */
void dbn_bufr(char *subtype, int *subtypelen, 
              unsigned char *msg, unsigned long *lenptr, 
              int *iret                                   ) {

	/* ensure null terminated string(s) */
	subtype[*subtypelen]=0x0;

	/* assume an error */
	*iret=0;

	if ( !inited && !BufrInit() )  return;

	if ( !WriteToStream(subtype, msg, (size_t)(*lenptr)) ) return;

	*iret=1;	
	return;
}

/*======================================================================*/


static int BufrInit(void) {

	const char myname[]="\ndbn_bufr: BufrInit:";

	char env_vars=1;
	int dbn_root_plen;
	char *cache_time;

/*
 * validate env variable
 */ 


	dbn_root=getenv(DBNROOT_ENV);
	cache_time=getenv(DBNBUFRT_ENV);


	if (dbn_root==NULL) {
		(void)fprintf(stderr, "%s environment variable %s not set!\n", myname, DBNROOT_ENV);
		env_vars=0;	
	}
	if (cache_time==NULL) {
		(void)fprintf(stderr, "%s environment variable %s not set!\n", myname, DBNBUFRT_ENV);
		env_vars=0;
	}
	if ( !env_vars ) return 0;

	dbn_cache_time=atoi(cache_time);
/*
 * determine the pending path of bufr -> dbnet
 */

	dbn_root_plen=strlen(dbn_root)+1;   /* include null */

	if (pending_path==NULL) {
		pending_path=malloc(dbn_root_plen + strlen(TMP_QUE_NAME) + FN_LEN);
		if (pending_path==NULL) {
			(void)fprintf(stderr, "%s malloc(%i) for pending path of bufr->dbnet failed",
				myname, (int)(dbn_root_plen+strlen(TMP_QUE_NAME)+FN_LEN));
			return 0;
		}
		(void)strcpy(pending_path, dbn_root);
		(void)strcat(pending_path, TMP_QUE_NAME);
		pending_prefix_len=strlen(pending_path);
	}

/*
 * determine dbn_bufr.pl path 
 */
#if START_DBNBUFRPL

	static char *dbn_bufrpl=NULL;
	 if (dbn_bufrpl==NULL) {
		dbn_bufrpl=malloc(dbn_root_plen + strlen(DBNBUFRPL));
		if (dbn_bufrpl==NULL) {
			(void)fprintf(stderr, "%s malloc(%i) for dbn_bufr.pl path failed",
				myname, dbn_root_plen+strlen(DBNBUFRPL));
			return 0;
		}
		(void)strcpy(dbn_bufrpl, dbn_root);
		(void)strcat(dbn_bufrpl, DBNBUFRPL);
	}

#endif

/*
 * start dbn_bufr.pl 
 */
#if START_DBNBUFRPL
	pid_t cpid, gpid;

	if ((cpid=fork()) < 0) {
		perror("dbn_bufr: fork");
		(void)fprintf(stderr, "%s fork failed", myname);
		return 0;
	}
	if (cpid==0) {		/* child process */
		if ((gpid=fork()) < 0) {
			perror("dbn_bufr: child: fork");
			(void)fprintf(stderr, "%s child: fork failed", myname);
			exit(EXIT_FAILURE);
		}
		if (gpid==0) {	/* grandchild process */
			if (execlp(dbn_bufrpl, dbn_bufrpl, NULL)<0) {
				perror("dbn_bufr: grandchild: execlp");
				(void)fprintf(stderr, "%s grandchild: execlp(%s, %s) failed\n",
					myname, dbn_bufrpl, dbn_bufrpl); 
			}
			exit(EXIT_FAILURE);		/* grandchild process error */
		}
		exit(EXIT_SUCCESS);			/* child exit */
	}
	while (wait((int *)NULL) != cpid) /* wait for the child */
		;
#endif

/*
 * determine message element size
 */
	msg_element_sz=sizeof(int);


/*
 * successful init
 */ 
	inited=1;
	return 1;
}


#define TOLERANCE 1
#define MINCACHETIME TOLERANCE+2
static int WriteToStream(char *subtype, unsigned char *msg, size_t mlen) {

	const char myname[]="\ndbn_bufr: WriteToStream:";

	static unsigned fsn=0;
	static FILE *fs=NULL;
	static char token[128];
	unsigned cnt;
	
	time_t currtime=time((time_t *)NULL);
	static time_t prevtime=0;
	time_t cachetime;
	time_t offsettime;
	static time_t filetime=TOLERANCE;

	if (currtime >= (prevtime + filetime - TOLERANCE)) {
	
		/* close the current file */	
		if (fs!=NULL)  {
			(void)fclose(fs);
			fs=NULL;
		}

		cachetime=(((time_t)dbn_cache_time > MINCACHETIME) ? (time_t)dbn_cache_time : MINCACHETIME);
		offsettime=cachetime - (currtime % cachetime);
		filetime=((offsettime <= MINCACHETIME) ? (cachetime + offsettime) : offsettime);

		/* insert subtype as the site code */
		cnt=0;
		BuildFN(subtype, &cnt, 1);

		/* insert file seq number into filename */
		fsn = (fsn + 1) % 10000;
		(void)sprintf(token, "%u", fsn);
		BuildFN(token, &cnt, 1);

		/* insert pid into the filename */
		(void)sprintf(token, "%u", getpid());
		BuildFN(token, &cnt, 1);

		/* insert time into the filename */
		(void)sprintf(token, "%lu", currtime);
		BuildFN(token, &cnt, 1);

		/* insert cache time into the filename */
		(void)sprintf(token, "%lu", filetime);
		BuildFN(token, &cnt, 0);
		prevtime=currtime;

	}

	if (fs==NULL && (fs=fopen(pending_path, "a"))==NULL) {
		(void)fprintf(stderr, "%s fopen(%s) failed", myname, pending_path);
		return 0;
	}

	if (fwrite(msg, msg_element_sz, mlen, fs) != mlen) {
		(void)fprintf(stderr, "%s fwrite(%s) failed - mlen=%u", myname, pending_path, (unsigned int)mlen);
		return 0;
	}

	(void)fclose(fs);
	if (chmod(pending_path, DBNFMODE)) {
		(void)fprintf(stderr, "%s chmod(%s,%u) failed", myname, pending_path, DBNFMODE);
		return 0;
	}
	fs=NULL;
	return 1;
}



static void BuildFN( char *token, unsigned *cnt, int dot) {
	

	(void)strcat(pending_path + pending_prefix_len + *cnt, token);
	*cnt += strlen(token);
	if ( !dot )  return;
	*(pending_path + pending_prefix_len + *cnt)='.';
	*cnt = *cnt + 1;
	*(pending_path + pending_prefix_len + *cnt)=0x0;
	return;	
}
