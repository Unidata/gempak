#include "shapefil.h"
#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "pgprm.h"


#define ERROR (-1)

/* 
 * Object Type codes used in main(): 
 */
#define OBJECTTYPE_POLYGON	3

/* 
 * Error codes for exit() routine: 
 */
#define	ERR_USAGE	1
#define ERR_TYPE	2
#define ERR_FORMAT	3
#define ERR_OBJECTTYPE	4
#define ERR_ALLOC	5
#define ERR_DBFCREATE	10
#define ERR_DBFADDFIELD	11
#define ERR_DBFOPEN	12
#define	ERR_DBFWRITEINTEGERATTRIBUTE	13
#define ERR_SHPOPEN	20

/* 
 * Maximum length for read strings,
 * if input lines with more characters appear,
 * errors are likely to occur 
 */
#define STR_BUFFER_SIZE		300

/*
 *  Global variables
 */
int	_found = G_FALSE;
/************************************************************************
 * tpc2shp.c                                                            *
 *                                                                      *
 * CONTENTS:                                                            *
 *      tpc2shp                                                         *
 *                                                                      *
 * Private Functions:                                                   *
 *									*
 * 	LaunchDbf	- creates and returns handle for .dbf file	*
 *	LaunchShp	- creates and returns handle for .shp file	*
 *	MakeShapeAtts	- calls DBFAddField to add atts to .dbf file    *
 * 	PutShpAtts 	- calls DBFWriteStringAttribute to write atts   *
 * 	Lin2Shp		- writes line elem data to .dbf and .shp files	*
 *	WrtShpPgn	- calls SHPWriteObject to write out a polygon	*
 *	WrtShpPrj	- writes earth model to the .prj file		*
 *                                                                      *
 ***********************************************************************/

DBFHandle LaunchDbf ( const char *fname );

SHPHandle LaunchShp ( const char *fname, int ObjectType );

static void Lin2Shp        ( VG_DBStruct *el, char *areaType, char *areaName,
                      DBFHandle dbf, SHPHandle shp, int *shp_nmbr );

static void GetTextTags    ( char *text, char *areaType, char *areaName, int *iret);

static void PutShpAtts	    ( int rec, char *areaType, char *areaName, DBFHandle hDBF );

static void MakeShapeAtts  ( DBFHandle hDBF );

static void WrtShpPgn      ( SHPHandle hSHP, int rec, int coords, 
                      double *x, double *y, int nparts, int *partstarts);

static void WrtShpPrj      ( char *filename);

int main ( int argc, char **argv )
/************************************************************************
 * bnd2shp                                                              *
 *                                                                      *
 * This programs reads VGF file containing bounds and creates		*
 * output polygon shapefile files .shp, .shx, .prj and .dbf 	  	*
 *                                                                      *
 * Usage:                                                               *
 * 	bnd2shp [-uh] -o outfile -i infile.vgf				*
 *                                                                      *
 * main ( argc, argv )                                                  *
 *                                                                      *
 * Input parameters:                                                    *
 *      argc    int             number of parameters of command line    *
 *      argv    char**          parameter array of command line         *
 *                                                                      *
 * Output files:                                                   	*
 * 	.shp 			vertices of objects			*	
 *	.shx			index data to objects in .shp file	*
 * 	.dbf			attributes of objects in xBase format	*
 *	.prj			description of the earth model used	*
 *									*
 * Return parameters:                                                   *
 *      NONE                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC 03/09   Created                                 *
 ***********************************************************************/
{
	int		ne, neIn, more, moreIn, ier; 
	int		pagflg, wrtflg, curpos, curposIn;
	VG_DBStruct	el, elIn;
	char		asfn[32], vgfn[128], pgnfile[132];
	char		ifname[128], areaType[30], areaName[30];
        char    	errgrp[8];
	long		ifilesize;
	int		c,i,npgn;
	FILE		*ifptr;
	DBFHandle pgnDBF;	/* handle for polygon dBase file */
	SHPHandle pgnSHP;	/* handle for polygon shape files .shx and .shp */
/*---------------------------------------------------------------------*/

/*
 *  First check if number of input arguments is correct.
 */
    	if ( argc == 1 )  {
        	pagflg = G_FALSE;
       	 	strcpy ( errgrp, "BND2SHP" );
        	ip_help ( errgrp, &pagflg, &ier, strlen(errgrp) );
        	exit (0);
    	}

	c=1;
	while ((i = getopt(argc, argv, "uhi:o:")) != -1) {
	   switch (i) {
             case 'u' :
	        printf("usage: bnd2shp -o outfile -i infile.vgf\n");
	        printf("reads VGF file containing line elements \n"
		"and creates output polygon shapefile\n"
		"files shp, shx and dbf \n"
		"example:\nbnd2shp -o test -i test.vgf\n"
		"creates POLYGON files test_pgn.[shp,shx,dbf]\n");
		return(0);
             case 'h' :
		printf("usage: bnd2shp [-uh] -o outfile -i infile.vgf\n");
		return(0);
             case 'o' :
		sprintf(asfn,"%s",optarg); c++;
		break;
             case 'i' :
		sprintf(vgfn,"%s",optarg); c++;
		break;
	   }
	}

	/*  
	 * if user specified both input and output we are good to go
 	 */

	wrtflg = 0;
	cvg_open ( vgfn, wrtflg, &(ifptr), &ier );

	if ( ier != 0 )  {
		printf("Error opening VGF file %s\n", vgfn );
		exit (0);
	}

	/* 
	 * Open and prepare output files 
	 */
	sprintf(pgnfile,"%s_pgn",asfn);

	cfl_inqr ( vgfn, NULL, &ifilesize, ifname, &ier );

	ne = 0;
	npgn = 0;
	more = G_TRUE;
	curpos = 0;
	ifptr = (FILE *) cfl_ropn(ifname, "", &ier);
        /*
         * Outer scan
         */
	while ( ne < MAX_EDITABLE_ELEMS && more == G_TRUE )  {
		cvg_rdrecnoc( ifname, ifptr, curpos, &el, &ier );
		if ( ier < 0 )  {
			more = G_FALSE;
		}
		else if ( el.hdr.recsz > 0 )  {
			switch (el.hdr.vg_type) {
				case LINE_ELM :
                                /*
                                 * Inner scan
                                 */
                                neIn = 0;
                                moreIn = G_TRUE;
                                curposIn = 0; 
                                while ( neIn < MAX_EDITABLE_ELEMS && moreIn == G_TRUE )  {
                                       cvg_rdrecnoc( ifname, ifptr, curposIn, &elIn, &ier );
                                       if ( ier < 0 )  {
                                           more = G_FALSE;
                                       }
                                       else if ( elIn.hdr.recsz > 0 )  {
                                           switch (elIn.hdr.vg_type) {
				               case SPTX_ELM :
                                                  if ( el.hdr.grpnum == elIn.hdr.grpnum ) {
					              /* 
                                                       * Create handles for Polygon files if at least 
                                                       * one SPTX_ELM found
					               */
                                                      _found = G_TRUE;
        				              pgnDBF = LaunchDbf(pgnfile);
        				              pgnSHP = LaunchShp(pgnfile, OBJECTTYPE_POLYGON);
					              WrtShpPrj(pgnfile);
                                                      /*
                                                       * leave the Inner and Outer scan loops
                                                       */
                                                      more = G_FALSE;
                                                      moreIn = G_FALSE;
                                                  }
					          break;
				               default:
					          break;
                                           }
                                           curposIn += elIn.hdr.recsz;
                                       }
                                       neIn++;
                                }

				default:
					break;
			}
			curpos += el.hdr.recsz;
		}
                ne++;
	}

        cvg_freeElPtr ( &el );
        cvg_freeElPtr ( &elIn );
        cvg_clos ( ifptr, &ier );

	/* 
	 * Read elements and call the specific functions 
         * to convert to shapefile format  
	 */
	cvg_open ( vgfn, wrtflg, &(ifptr), &ier );
	cfl_inqr ( vgfn, NULL, &ifilesize, ifname, &ier );

	ne = 0;
	npgn = 0;
	more = G_TRUE;
	curpos = 0;
	ifptr = (FILE *) cfl_ropn(ifname, "", &ier);
	while ( ne < MAX_EDITABLE_ELEMS && more == G_TRUE )  {
		cvg_rdrecnoc( ifname, ifptr, curpos, &el, &ier );
		if ( ier < 0 )  {
			more = G_FALSE;
		}
		else if ( el.hdr.recsz > 0 )  {
			switch (el.hdr.vg_type) {
				case LINE_ELM :
                                        /*
                                         * Inner scan
                                         */
                                        neIn = 0;
                                        moreIn = G_TRUE;
                                        curposIn = 0; 
                                        while ( neIn < MAX_EDITABLE_ELEMS && moreIn == G_TRUE )  {
                                            cvg_rdrecnoc( ifname, ifptr, curposIn, &elIn, &ier );
                                            if ( ier < 0 )  {
                                               more = G_FALSE;
                                            }
                                            else if ( elIn.hdr.recsz > 0 )  {
                                               switch (elIn.hdr.vg_type) {
				                   case SPTX_ELM :
                                                       if ( el.hdr.grpnum == elIn.hdr.grpnum ) {
					                 /* 
					                  * get the name and type of the area tag
					                  */
                                                          GetTextTags (elIn.elem.spt.text, areaType, areaName, &ier);
					                 /* 
					                  * copy the attribute data into the line dBase files
					                  * copy the spatial data into the line shapefiles
					                  */
					                  Lin2Shp(&el, areaType, areaName, pgnDBF, pgnSHP, &npgn);
                                                          /*
                                                           * leave the Inner scan loop
                                                           */
                                                          moreIn = G_FALSE;
                                                       }
					               break;
				                   default:
					               break;
                                               }
                                               curposIn += elIn.hdr.recsz;
                                            }
                                            neIn++;
                                        }
				default:
					break;
			}
			curpos += el.hdr.recsz;
		}
	}

        cvg_clos ( ifptr, &ier );

	/* 
	 * Finish by closing output shapefile files 
	 */
        if ( _found ) {
	    DBFClose( pgnDBF );
	    SHPClose( pgnSHP );
        }
	return(0);
}

/*=====================================================================*/

static void Lin2Shp ( VG_DBStruct *el, char *areaType, char *areaName, 
               DBFHandle dbf, SHPHandle shp, int *shp_nmbr )
{

int     ii, npts, nptsp1, nparts = 1, *pstr = NULL;
double  *x, *y;

/*---------------------------------------------------------------------*/                                                                                         

        npts = el->elem.lin.info.numpts;
        nptsp1 = npts + 1;
        x = (double *)malloc(nptsp1*sizeof(double));
        y = (double *)malloc(nptsp1*sizeof(double));
        pstr = malloc( sizeof(int) * (nparts));
        pstr[0] = 0;
        for ( ii = 0; ii < npts; ii++ ) {
                x[ii] = (double)el->elem.lin.latlon[ii+npts];
                y[ii] = (double)el->elem.lin.latlon[ii];
        }
        x[npts] = x[0];
        y[npts] = y[0];
        PutShpAtts(*shp_nmbr, areaType, areaName, dbf);
        WrtShpPgn(shp,*shp_nmbr,nptsp1,x,y, nparts, pstr);
        *shp_nmbr += 1;
        free(pstr);
        free(x);
        free(y);
                                                                                         
        return;
                                                                                         
}
                                                                                         
/*=====================================================================*/

static void GetTextTags (char *text, char *areaType, char *areaName, int *iret)

{

        char       **starr,subtext[60];
        int        ier, ii, maxnum=5, maxch=30, istnum;        

/*---------------------------------------------------------------------*/                                                                                         
        
        starr = (char **)malloc(maxnum * sizeof(char *)); 
        for ( ii = 0; ii < maxnum; ii++ ) {
           starr[ii] = (char *)malloc((maxch+1) * sizeof(char *));
        }

        cst_clst (text, '<', "", maxnum, maxch, starr, &istnum, &ier);
        strcpy ( subtext, starr[1]);
        cst_clst (subtext, '>', "", maxnum, maxch, starr, &istnum, &ier);
        strcpy ( areaType, starr[0]);
        strcpy ( areaName, starr[1]);

        for( ii = 0; ii < maxnum; ii++ ) {
           free( starr[ii] );
        }

        if ( starr ) free( (char **) starr ); 

        return;

}

/*=====================================================================*/

DBFHandle LaunchDbf (	const char *fname ) 

{
	DBFHandle	hDBF;
	char		dbffname[STR_BUFFER_SIZE];

/*---------------------------------------------------------------------*/                                                                                         

	sprintf(dbffname, "%s.dbf", fname);

	hDBF = DBFCreate( dbffname );
	if( hDBF == NULL ) {
		fprintf(stderr, "DBFCreate(%s) failed.\n", fname );
		exit(ERR_DBFCREATE);
	}

        if ( _found ) {
	   MakeShapeAtts(hDBF);
        }

	DBFClose( hDBF );

	hDBF = DBFOpen( dbffname, "r+b" );
	if( hDBF == NULL ) {
		fprintf(stderr, "DBFOpen(%s,\"r+b\") failed.\n", dbffname );
		exit(ERR_DBFOPEN);
	}

	return hDBF;
}

/*=====================================================================*/

SHPHandle LaunchShp(	const char *fname,
				int ObjectType ) 

{
	SHPHandle	hSHP;
	char		shpfname[STR_BUFFER_SIZE];

/*---------------------------------------------------------------------*/                                                                                         

	sprintf(shpfname, "%s.shp", fname);

	switch (ObjectType) {
		case OBJECTTYPE_POLYGON:
			hSHP = SHPCreate( shpfname, SHPT_POLYGON );
			break;
		default:
			fprintf(stderr, "internal error: "
				"unknown ObjectType=%d\n", ObjectType);
			exit(ERR_OBJECTTYPE);
	}

	if( hSHP == NULL ) {
		fprintf(stderr, "SHPOpen(%s, shape_type) failed.\n", shpfname );
		exit(ERR_SHPOPEN);
	}

	return hSHP;
}

/*=====================================================================*/

static void MakeShapeAtts (DBFHandle hDBF)
{
        DBFAddField( hDBF, "AreaType",  FTString, 20, 0 );
        DBFAddField( hDBF, "AreaName",  FTString, 20, 0 );
}

/*=====================================================================*/

static void PutShpAtts (int rec, char *areaType, char *areaName, DBFHandle hDBF)
{
	DBFWriteStringAttribute(hDBF,rec, 0, areaType );
	DBFWriteStringAttribute(hDBF,rec, 1, areaName );
        return;
}

static void WrtShpPrj (char *filename)
{
	char buffname[132];
	FILE *fp;

/*---------------------------------------------------------------------*/                                                                                         

	sprintf(buffname,"%s.prj",filename);
	fp = fopen(buffname,"w+");
	/*fprintf(fp,"some projection some radius 6371200.0\n");*/
	fprintf(fp,"GEOGCS[\"GCS_Sphere\",DATUM[\"D_Sphere\",SPHEROID[\"Sphere\",6371200.0,0.0]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]]");
	fclose(fp);
}

/*=====================================================================*/

static void WrtShpPgn(	SHPHandle hSHP,
		int rec,
		int coords,
		double * x,
		double * y,
		int nparts,
		int * partstarts) 
{
	SHPObject	*psShape;

/*---------------------------------------------------------------------*/                                                                                         


	psShape = SHPCreateObject( SHPT_POLYGON, rec, nparts, partstarts, NULL,
		coords, x, y, NULL, NULL );
	SHPWriteObject( hSHP, -1, psShape );
	SHPDestroyObject( psShape );
}
