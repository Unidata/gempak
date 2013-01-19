#ifndef		lint
static char *rcsid = "$Id: main.c,v 1.4 1992/05/04 20:18:25 steve Exp $" ;
#endif	/*	lint*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mc_area.h"

int verbose = 0 ;
int ymdh=0,root=0;
char *progname;

void
usage()
{
	fprintf(stderr,
		"Usage: %s [-v] [-w] [file ... ]\n", progname);
	exit(1);
}

int main(int ac, char *av[])
{
	extern int optind;
	extern int opterr;
	extern char *optarg;
	int cc;
	char *cp ;
	char ofname[80] ;
	char file_ymdh[10];
	struct mc_area *area ;
	extern struct mc_area *mc_rarea() ;
	extern char *mc_sensor() ;
	extern char *create_filename() ;

	opterr = 1;
	progname = av[0];

	while ((cc = getopt(ac, av, "vdt")) != EOF)
		switch(cc) {
		case 'v':
			verbose = 1 ;
			break;
		case 'd':
			ymdh = 1;
			break;
		case 't':
			ymdh = 1;
			root = 1;
			break;
		case '?':
			usage();
			break;
		}

	ac -= optind;
	av += optind;

	if(ac < 1) usage() ;

 	for ( ; ac > 0 ; ac--, av++)
	{
		if(( area = mc_rarea(*av)) == NULL)
		{
			break;
		}

		if(1)
		{
			fprintf(stderr,"%s\n", *av) ;
		}
		if(verbose)
		{
			char scratch[48] ;
			struct area_dir *dir = area->dir ;

			fprintf(stderr,"%s\n", "Directory") ;
			
			fprintf(stderr,"\tstatus    %d\n", dir->status) ; /*  1 */
			fprintf(stderr,"\ttype      %d\n", dir->type) ; /*  2 */
			fprintf(stderr,"\tsatid     %d: %s\n",
				dir->satid, mc_sensor(dir->satid) ) ; /*  3 */
			fprintf(stderr,"\tndate     %d (YYDDD)\n", dir->ndate) ; /*  4 */
			fprintf(stderr,"\tntime     %d (HHMMSS)\n", dir->ntime) ; /*  5 */
			fprintf(stderr,"\tlcor      %d\n", dir->lcor) ; /*  6 */
			fprintf(stderr,"\tecor      %d\n", dir->ecor) ; /*  7 */
			fprintf(stderr,"\tzcor      %d\n", dir->zcor) ; /*  8 */
			fprintf(stderr,"\tlsiz      %d\n", dir->lsiz) ; /*  9 */
			fprintf(stderr,"\tesiz      %d\n", dir->esiz) ; /* 10 */
			fprintf(stderr,"\tzsiz      %d\n", dir->zsiz) ; /* 11 */
			fprintf(stderr,"\tlres      %d\n", dir->lres) ; /* 12 */
			fprintf(stderr,"\teres      %d\n", dir->eres) ; /* 13 */
			fprintf(stderr,"\tbands     %d\n", dir->bands) ; /* 14 */
			fprintf(stderr,"\tyzprefix  %d\n", dir->yzprefix) ; /* 15 */
			fprintf(stderr,"\tprojnum   %d\n", dir->projnum) ; /* 16 */
			fprintf(stderr,"\tcdate     %d\n", dir->cdate) ; /* 17 */
			fprintf(stderr,"\tctime     %d\n", dir->ctime) ; /* 18 */
			fprintf(stderr,"\tfiltmap   %d\n", dir->filtmap) ; /* 19 */
			fprintf(stderr,"\timageid   %d\n", dir->imageid) ; /* 20 */
			fprintf(stderr,"\tresvid    %d %d %d %d\n",
				dir->resvid[0] , /* 21 */
				dir->resvid[1] , /* 22 */
				dir->resvid[2] , /* 23 */
				dir->resvid[3] ) ; /* 24 */
			strncpy(scratch, dir->comments, 32) ; scratch[32] = 0 ;
			fprintf(stderr,"\tcomments  %s\n", scratch) ; /* 25 - 32 */
			fprintf(stderr,"\tcalkey    %d\n", dir->calkey) ; /* 33 */
			fprintf(stderr,"\tnavkey    %d\n", dir->navkey) ; /* 34 */
			fprintf(stderr,"\tnavkey2   %d\n", dir->navkey2) ; /* 35 */
			fprintf(stderr,"\tlprefix   %d\n", dir->lprefix) ; /* 36 */
			fprintf(stderr,"\tpdl       %d %d %d %d %d %d %d %d\n",
				dir->pdl[0] , /* 37 */
				dir->pdl[1] , /* 38 */
				dir->pdl[2] , /* 39 */
				dir->pdl[3] , /* 40 */
				dir->pdl[4] , /* 41 */
				dir->pdl[5] , /* 42 */
				dir->pdl[6] , /* 43 */
				dir->pdl[7] ) ; /* 44 */
			fprintf(stderr,"\tband8     %d\n", dir->band8) ; /* 45 */
			fprintf(stderr,"\tidate     %d (YYDDD)\n", dir->idate) ; /* 46 */
			fprintf(stderr,"\titime     %d (HHMMSS)\n", dir->itime) ; /* 47 */
			fprintf(stderr,"\tstartscan %d\n", dir->startscan) ; /* 48 */
			fprintf(stderr,"\tdoclen    %d\n", dir->doclen) ; /* 49 */
			fprintf(stderr,"\tcallen    %d\n", dir->callen) ; /* 50 */
			fprintf(stderr,"\tlevlen    %d\n", dir->levlen) ; /* 51 */
			strncpy(scratch, dir->stype, 4) ; scratch[4] = 0 ;
			fprintf(stderr,"\tstype     %s\n", scratch) ; /* 52 */
			strncpy(scratch, dir->stype, 4) ; scratch[4] = 0 ;
			fprintf(stderr,"\tctype     %s\n", scratch) ; /* 53 */
			fprintf(stderr,"\treserved  %d %d %d %d %d %d %d %d %d %d %d\n",
				dir->reserved[0] , /* 54 */
				dir->reserved[1] , /* 55 */
				dir->reserved[2] , /* 56 */
				dir->reserved[3] , /* 57 */
				dir->reserved[4] , /* 58 */
				dir->reserved[5] , /* 59 */
				dir->reserved[6] , /* 60 */
				dir->reserved[7] , /* 61 */
				dir->reserved[8] , /* 62 */
				dir->reserved[9] , /* 63 */
				dir->reserved[10] ) ; /* 64 */
			strncpy(scratch, (char *)dir->reserved, 44) ; scratch[44] = 0 ;
			fprintf(stderr,"\t\t      %s\n", scratch) ;
		}

		if(verbose)
		{
			long id, idate ;
/*			id = area->nav->iddate/100000 ;
			idate = area->nav->iddate - id*100000 ; */
			id = area->nav->goes.iddate/100000 ;
			idate = area->nav->goes.iddate - id*100000 ; 
			fprintf(stderr,"%s\n", "Navigation") ;
			fprintf(stderr,
				"\tsatellite id, image date and time:\t%d\t%d\t%d (NAV)\n",
/*				id,idate,area->nav->itime) ; */
				id,idate,area->dir->itime) ; 
		}
		if(verbose)
		{
			putc('\n', stderr) ;
		}
                if(!ymdh)
		printf("%s %dkm\n", create_filename(area->dir),area->dir->lres) ;
		else
		   {
                   if(!root)
		      printf("%s\n", create_filename(area->dir));
		   else
 		      {
		      file_ymdh[0] = '\0';
                      strncat(file_ymdh,create_filename(area->dir),8);
                      printf("%s\n",file_ymdh);
                      }
		   }
	}

	exit(0);
}
