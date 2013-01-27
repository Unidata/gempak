/*
 *	Copyright 1988, University Corporation for Atmospheric Research
 *		Not for Resale. All copies to include this notice.
 */
/* $Id: mc_rarea.c,v 1.3 1991/10/29 23:58:17 steve Exp $ */

#include <stdio.h>
#include <stdlib.h>
#include "mc_area.h"


/*
 * convert from little endian to big endian four byte object
 */
static unsigned int
vhtonl(lend)
unsigned int lend ;
{
	unsigned int bend ;
	unsigned char *lp, *bp ;

	lp = ((unsigned char *)&lend) + 3 ;
	bp = (unsigned char *) &bend ;

	*bp++ = *lp-- ;
	*bp++ = *lp-- ;
	*bp++ = *lp-- ;
	*bp = *lp ;

	return(bend) ;
}


/*
 *  Get a McIdas area file by name
 *   returns dynamically allocated struct or NULL on error.
 */
struct mc_area *
mc_rarea( filename )
char *filename ;
{
	FILE *ifd ,*efopen();
	struct mc_area *gret ;
	struct area_dir *dir ;
	mc_nav *nav ;
	unsigned char *ibuf ;
	int size ;
	unsigned int *begin ; 
	unsigned int *ulp ;
	int doswap = 0 ;

	if((ifd = efopen(filename, "r")) == NULL) return NULL ;

	if((gret = (struct mc_area*)malloc(sizeof(struct mc_area))) == NULL)
	{
		perror("malloc:(struct mc_area)") ;
		fclose(ifd) ;
		return NULL ;
	}

	if((dir = (struct area_dir *)malloc(sizeof(struct area_dir))) == NULL)
	{
		perror("malloc:(struct area_dir)") ;
		free(gret) ;
		fclose(ifd) ;
		return NULL ;
	}

	if (fread(dir, sizeof(struct area_dir), 1, ifd) == 0)
	{
		perror("fread: area directory ");
		free(dir) ;
		free(gret) ;
		fclose(ifd) ;
		return(NULL) ;
	}
	gret->dir = dir ;


	if(dir->type != 4) 
	{
		if(dir->type != 67108864)
		{
			fprintf(stderr,"Unknown type %d %u\n",
				dir->type,dir->type) ;
			free(dir) ;
			free(gret) ;
			fclose(ifd) ;
			return(NULL) ;
		} else
			doswap = 1 ;

	}

	if(doswap) 
	{
	begin = (unsigned int *)dir ;
	for(ulp = begin ; ulp < &begin[AREA_COMMENTS] ; ulp++)
	*ulp = vhtonl(*ulp) ;
	for(ulp = &begin[AREA_CALKEY] ; ulp < &begin[AREA_STYPE] ; ulp++)
	*ulp = vhtonl(*ulp) ;
	}

	if((nav = (mc_nav *)malloc(sizeof(mc_nav)))
		== NULL)
	{
		perror("malloc:(struct nav)") ;
		free(dir) ;
		free(gret) ;
		fclose(ifd) ;
		return NULL ;
	}
	if (fread(nav, sizeof(mc_nav), 1, ifd) == 0)
	{
		perror("nav fread");
		free(nav) ;
		free(dir) ;
		free(gret) ;
		fclose(ifd) ;
		return(NULL) ;
	}
	gret->nav = nav ;

	if(doswap)
	{
	begin = (unsigned int *)nav ;
	for(ulp = &begin[NAV_DATA] ; ulp < &begin[NAV_RESERVED] ; ulp++)
	*ulp = vhtonl(*ulp) ;
	}

	if(dir->zsiz != 1)
	{
		fprintf(stderr,"zsiz %d != 1 not supported\n", dir->zsiz) ;
		free(nav) ;
		free(dir) ;
		free(gret) ;
		fclose(ifd) ;
		return NULL ;
	}

	size = dir->lsiz * dir->esiz ;

	if((ibuf = (unsigned char*)malloc(size)) == NULL)
	{
		perror("malloc:(image)") ;
		free(nav) ;
		free(dir) ;
		free(gret) ;
		fclose(ifd) ;
		return(NULL) ;
	}
/*	if (fread(ibuf, size, 1, ifd) == 0)
	{
		perror("image fread");
		fprintf(stderr,"size %d\n", size) ;
		return(NULL) ;
	}*/
	gret->image =  ibuf ;
	gret->private =  NULL ;
	return(gret) ;
}
