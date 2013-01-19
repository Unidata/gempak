#include "geminc.h"
#include "gemprm.h"

#include "netcdf.h"

#ifdef UNDERSCORE
#define imrad2g	imrad2g_
#define imcset	imcset_
#define imdset	imdset_
#define im_calbar_init	im_calbar_init_
#define im_calbar	im_calbar_
#endif

void	imrad2g ( char *elevation, char *radparm, int *ier, size_t, size_t);
void	imcset ( char *cparm, char *cval, int *ier, size_t, size_t);
void	imdset ( char *cparm, int *dval, int *ier, size_t );
void	im_calbar_init (int *ier);
void	im_calbar (float *minpx, float *maxpx, float *minval, float *maxval, int *ier);

void im_rqpe ( char imgfil[], int *kx, int *ky, int *isorc, int *itype, 
		int *idate, int *itime, int *iproj, float *clat, 
		float *clon, float *xmin, float *xmax, float *ymin, 
		float *ymax, int *iret )
/************************************************************************
 * im_rqpe								*
 *									*
 * This subroutine opens a NetCDF file and reads the header information *
 * and returns it to the calling function.                              *
 *                                                                      *
 * im_rqpe ( imgfil, kx, ky, isorc, itype, idate, itime, iproj,         *
 *	  clat, clon, xmin, xmax, ymin, ymax, iret )                    *
 *                                                                      *
 * Input parameters:                                                    *
 *	imgfil[]	char		Name of image file              *
 *                                                                      *
 * Output parameters:                                                   *
 *	*kx		int		x dimension of image		*
 *     	*ky		int		y dimension of image		*
 *	*isorc		int						*
 *	*itype		int		type of channel			*
 * 	*idate		int		date				*
 *	*itime		int		time				*
 *	*iproj		int		projection type			*
 *	*clat		float		central latitude		*
 * 	*clon		float		central longitude		*
 *	*xmin		float		x - minimum for image		*
 * 	*xmax	 	float		x - maximum for image		*
 *	*ymin 		float		y - minimum for image		*
 *	*ymax		float		y - maximum for image		*
 *	*iret 		int		return value			*
 *                                                                      *
 *                                                                      *
 **									*
 * Log:									*
 ***********************************************************************/
{
	int		ier, mosaic_id, xdim_id, ydim_id ;
	int		DONE, ncid, iy, im, id, ih, in, is;
	size_t		jx, jy, attlen;
	nc_type		xtype;
	int		lenp, i, j, nvars, nelev=0, numlev;
	long		dsec;
	float		dlat, dlon, scale;
	time_t		isec;
	struct tm	*tarr;
	char		radparm[33], elevation[33], cparm[12], nname[33], lbuf[256], buf[256];
	char 		*cattval;
	float		minpx = 0., maxpx = 255.; /* eventually read these from radnetcdf.tbl file */
	float		fmin=0., fmax=255.;
	FILE 		*fp;
	static char	oname[33]="cref"; /* default parameter name to use when opening file*/
/*---------------------------------------------------------------------*/
	*iret = 0;
	*itype = -1;
/*
 *	Open the NetCDF file.
 */
	ier = nc_open ( imgfil, NC_NOWRITE, &ncid );
	if ( ier != 0 )
	   {
	   printf("unable to open file\n");
	   *iret = -1; /* not able to open file */
	   return;
	   }
/*
 *	See if this is a QPESUMS like file
 */
	imrad2g ( elevation, radparm, &ier, sizeof(elevation), sizeof(radparm));
	st_null ( radparm, radparm, &lenp, &ier, sizeof(radparm), sizeof(radparm));
	st_null ( elevation, elevation, &lenp, &ier, sizeof(elevation), sizeof(elevation));

	cst_lstr ( radparm, &lenp, &ier );
	radparm[lenp] = '\0';

	cst_uclc ( radparm, nname, &ier);

	if ( strcmp(nname,"list") != 0 )
           {
           if ( (fp = (FILE *)cfl_tbop("radnetcdf.tbl","unidata",&ier) ) != NULL )
             {
             ier = 0; DONE = 0;
             while ( ( ier == 0 ) && ( ! DONE ) )
                {
                cfl_trln(fp,255,buf,&ier);
		if ( ier != 0 ) continue;

		buf[19] = '\0';
	        cst_lstr ( buf, &lenp, &ier );
	        buf[lenp] = '\0';

		cst_uclc(buf, lbuf, &ier);
                if(strcmp(lbuf,nname) == 0)
	           {
		   strcpy(nname,buf); /* just in case NetCDF name is not lower case */
		   sscanf ( buf+20, "%d %f,%f,%f,%f", itype,&minpx,&maxpx,&fmin,&fmax);
		   DONE = 1;
                   }
		else if ( strcmp(buf,oname) == 0 ) /* save this as a failsafe, but keep looking! */
		   sscanf ( buf+20, "%d %f,%f,%f,%f", itype,&minpx,&maxpx,&fmin,&fmax);
	        }
	     cfl_clos ( fp, &ier);
             if ( *itype < 0 )
                {
	        printf("unknown parm name %s\n",nname);
	        *iret = -3;
	        return;
                }
	     if ( DONE == 0 )
		strcpy ( nname, oname );
             }
           else
              {
	      printf("unable to open radnetcdf.tbl\n");
              *iret = -2; /* not able to open product table */
	      return;
	      }
	   }

	if( ( ier = nc_inq_varid(ncid, nname, &mosaic_id)) != 0 )
	   {
	   /* try to match uiname, use lowercase only comparison */
	   if ( strcmp(nname,"list") == 0 ) printf("Searching for %s in the NetCDF file:\n",radparm);

           ier = nc_inq_nvars(ncid, &nvars);
           for ( i=0; i < nvars; i++)
	      {
	      ier = nc_inq_varname(ncid, i, buf);
	      ier = nc_inq_att(ncid, i, "uiname", &xtype, &attlen);
	      if ( ( ier == 0 ) && ( xtype == NC_CHAR ) && ( attlen > 0 ) )
		 {
		 cattval = (char *)malloc ( attlen * sizeof(char)  + 1);
		 ier = nc_get_att_text(ncid, i, "uiname", cattval);
		 for ( j = 0; j < strlen (cattval); j++) cattval[j] = tolower(cattval[j]);
		 if ( strcmp(nname,"list") == 0 ) printf("%d)%s %s\n",i,buf,cattval);
		 if ( strcmp(nname,cattval) == 0 ) 
		    {
		    mosaic_id = i;
		    strcpy ( nname, cattval);
		    free(cattval);
		    break;
		    }
		 free(cattval);
 		 }
              }
	   if ( ier != 0 )
	      {
	      /*if ( strcmp(nname,"list") != 0 ) printf("unable to locate %s in radnetcdf.tbl\n",nname);*/
	      nc_close ( ncid );
	      *iret = -4; /* does not contain expected variable */
	      return;
	      }
	   }

	/*printf("got here nname %s imdoff %d\n",nname,mosaic_id);*/

	strcpy ( cparm, "imdoff" );
 	imdset ( cparm, &mosaic_id, &ier, strlen(cparm));

	ier = nc_inq_dimid ( ncid, "Lon", &xdim_id);
	ier += nc_inq_dimid ( ncid, "Lat", &ydim_id);
        if ( ier != 0 )
           {
           nc_close ( ncid );
           *iret = -3;
	   return;
           }
/*
 *	Get the x and y dimensions of the image.
 */
	ier = nc_inq_dimlen ( ncid, ydim_id, &jy );
	ier = nc_inq_dimlen ( ncid, xdim_id, &jx );
	*kx = (int) jx;
	*ky = (int) jy;
/*
 *	Get the date/time. It is stored as the number of seconds
 *	since 00:00:00 UTC, January 1, 1970.
 */
	ier = nc_get_att_long ( ncid, NC_GLOBAL, "Time", &dsec );
	isec = (time_t) dsec;
	tarr = gmtime ( &isec );
	iy = (*tarr).tm_year + 1900;
	im = (*tarr).tm_mon + 1;
	id = (*tarr).tm_mday;
	ih = (*tarr).tm_hour;
	in = (*tarr).tm_min;
	is = (*tarr).tm_sec;
	*idate = iy * 10000 + im * 100 + id;
	*itime = ih * 10000 + in * 100 + is;

	/* for now, hardcode these */

	*isorc = 7;
/*
 *	Get the projection and central lat/lon.
 */
	*iproj = 8;
	*clat = 0.0;
	*clon = 0.0;
	/*nc_get_att_int ( ncid, NC_GLOBAL, "projIndex", iproj );
	nc_get_att_float ( ncid, NC_GLOBAL, "centralLat", clat );
	nc_get_att_float ( ncid, NC_GLOBAL, "centralLon", clon );*/

/*
 *	Get the corner lat/lons. The values are for the upper left
 *	and lower right corners.
 */
	nc_get_att_float ( ncid, NC_GLOBAL, "Latitude",   ymax );
	nc_get_att_float ( ncid, NC_GLOBAL, "Longitude",   xmin );
	nc_get_att_float ( ncid, NC_GLOBAL, "LatGridSpacing",   &dlat );
	nc_get_att_float ( ncid, NC_GLOBAL, "LonGridSpacing",   &dlon );
	*xmax = *xmin + ( (float)(jx - 1) * dlon );
	*ymin = *ymax - ( (float)(jy - 1) * dlat );
	/*printf("look %f %f %f %f   %f %f\n",
		*xmin, *xmax, *ymin, *ymax, dlat, dlon);*/

/*
 *	Close the NetCDF file.
 */

	im_calbar_init ( &ier);
	strcpy ( cparm, "imcalbar" ); i = 1;
 	imdset ( cparm, &i, &ier, strlen(cparm));
	im_calbar ( &minpx, &maxpx, &fmin, &fmax, &ier); /* note minpix should be same as in imgtype.tbl */
	strcpy ( cparm, "imndlv" ); i = (int)(maxpx - minpx);
 	imdset ( cparm, &i, &ier, strlen(cparm));

	if ( (ier = nc_get_att_float(ncid, mosaic_id, "Scale", &scale ) ) != 0 )
           scale = 1.0;

	if ( (ier = nc_inq_attlen(ncid, mosaic_id, "Units", &attlen ) ) == 0 )
           {
	   cattval = (char *)malloc ( attlen * sizeof(char)  + 1);
	   ier = nc_get_att_text(ncid, mosaic_id, "Units", cattval);
           if ( ier == 0 ) 
              {
	      cattval[attlen] = '\0';
	      cst_lstr ( cattval, &lenp, &ier );
	      cattval[lenp] = '\0';
	      /*if ( scale != 1.0 )
	         sprintf( buf, "%s*%d\0",cattval,(int)scale);
              else*/
		 strcpy(buf,cattval);

	      if (strlen(buf) > 8 ) buf[8] = '\0';

	      strcpy ( cparm, "cmbunt" );

              imcset ( cparm, buf, &ier, strlen(cparm), strlen(buf));
	      }
           free(cattval);
           }

	if ( (ier = nc_inq_dimid ( ncid, "Ht", &numlev) ) != 0 )
	   nelev = 0;
	else
	   {
	   cst_numb ( elevation, &nelev, &ier);
	   if ( ier != 0 )
	      nelev = 0;
	   else if ( ( nelev < 0 ) || ( nelev >= numlev ) ) 
	      nelev = 0;
	   }

	strcpy ( cparm, "imnchl" );
	i = mosaic_id * 100 + nelev;
 	imdset ( cparm, &i, &ier, strlen(cparm));

	strcpy ( cparm, "rmbelv" );
 	imdset ( cparm, &nelev, &ier, strlen(cparm));

 	nc_close ( ncid );
}
