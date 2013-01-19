#include "geminc.h"
#include "gemprm.h"
#include "cpgcmn.h"


void pg_print ( char *infname, int xsz, int ysz, int sample, 
		char *prt_name, int prt_size, char *outfname, int *iret )
/************************************************************************
 * pg_print								*
 *									*
 * This function creates a Postscript file for a raster product, and	*
 * sends that product to the named printer.				*
 *									*
 * pg_print ( infname, xsz, ysz, sample, prt_name, prt_size, outfname,	*
 *	      iret )							*
 *									*
 * Input parameters:							*
 *	*infname	char		Name of file with raster data	*
 *	xsz		int		Product size (pixels along X)	*
 *	ysz		int		Product size (pixels along Y)	*
 *	sample		int		Subsampling factor (1, 2, 4, 8)	*
 *	*prt_name	char		Printer name			*
 *	prt_size	int		Paper size (11, 17, etc.)	*
 *	*outfname	char		Name of Postscript file		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 6/96	Created					*
 * E. Wehner/EAi	11/96	Use cfl_inqr vice stat calls		*
 * M. Linda/GSC		10/97	Corrected the prologue format		*
 * T. Piper/SAIC	 1/02	Fixed memory leak; freed bufin		*
 * S. Jacobs/NCEP	 2/02	Fixed coord translation			*
 ***********************************************************************/
{
    char prolog[2048], bufout[120];
    char fname[120];
    FILE *wfp;    /* writing file pointer */
    FILE *rfp;    /* reading file pointer */
    char pprnam[20];
    float scale_fact;   /* calculated scaling factor */
    float scalex;
    float scaley;
    float prtx;
    float prty;
    int lenp;
    int line_ct = 0;
    int i,j,k;
    int b_in;
    long flen = 0;

    char tbuff[40];
    char *bufin;  /* buffer to read data into */

    *iret = 0;
    /* build a postscript file for the printer
     * by opening for write an output, creating a prolog,
     * then opeing the input raster, and converting inbound
     * data to hex ascii representations.
     */
    sprintf(outfname,"%s.ps", infname);

    if (prt_size == 11)
        strcpy(pprnam, "/Letter");
    else
        strcpy(pprnam, "/Tabloid");

    wfp = cfl_wopn(outfname, iret);
    if (wfp == NULL)
    {
        printf("Error opening %s for write \n", outfname);
        return;
    }

/*
 *      Construct the file prolog.
 */
    strcpy ( prolog, "%!PS-Adobe-3.0\n");
    strcat ( prolog, "%%Title:\n");
    strcat ( prolog, "%%Creator: GEMPAK\n");
    strcat ( prolog, "%%EndComments\n");
    strcat ( prolog, "%%Pages:(atend)\n");

    strcat ( prolog, " /inch {72 mul} def\n");

    strcat ( prolog, " /papersizedict 10 dict def\n" );
    strcat ( prolog, " /papersize {\n" );
    strcat ( prolog, "    papersizedict begin\n" );
    strcat ( prolog, "        /Letter {lettertray letter} def\n" );
    strcat ( prolog, "        /Tabloid {11x17tray 11x17} def\n" );
    strcat ( prolog, "        /unknown {unknown} def\n" );
    strcat ( prolog, "    papersizedict dup papername get\n" );
    strcat ( prolog, "    end\n" );
    strcat ( prolog, "    /pagedicttop countdictstack 1 add def\n" );
    strcat ( prolog, "    statusdict begin stopped end\n" );
    strcat ( prolog, "    countdictstack -1 pagedicttop {pop end} for\n" );
    strcat ( prolog, "    } def\n" );

    sprintf ( bufout, " /papername {%s} def\n", pprnam );
    strcat ( prolog, bufout );

    strcat ( prolog, "%%EndProlog\n" );
    strcat ( prolog, "%%BeginSetup\n" );
    strcat ( prolog, " papersize\n" );
    strcat ( prolog, "%%EndSetup\n" );

    strcat (prolog, "/concatprocs \n");
    strcat (prolog, "	{ /proc2 exch cvlit def \n");
    strcat (prolog, "     /proc1 exch cvlit def \n");
    strcat (prolog, "     /newproc proc1 length proc2 length add array def \n");
    strcat (prolog, "     newproc 0 proc1 putinterval \n");
    strcat (prolog, "     newproc proc1 length proc2 putinterval \n");
    strcat (prolog, "     newproc cvx \n");
    strcat (prolog, "   } def \n");
    strcat (prolog, "/picstr 2 string def \n");

    strcat (prolog, "\n/imagefax \n");
    strcat (prolog, "   {\n");

    sprintf(bufout, "%i %i 1 [%i 0 0 -%i 0 %i ]\n", xsz, ysz, xsz,
                                                   ysz, ysz);
    strcat (prolog, bufout);

    strcat( prolog, "      { currentfile picstr readhexstring pop }\n");
    strcat (prolog,"       image\n");
    strcat (prolog, "    } def \n");


    strcat ( prolog, " save\n");
/*
 *      Put transformation info in the PS header as well.
 */
    strcat ( prolog, " 1 setlinecap 1 setlinejoin newpath\n");
    strcat ( prolog, "%%Page:   1  ?\n\0");

    strcat ( prolog, "gsave\n");

    if ((float)prt_size <= 11.5F)
    {
        prtx = 8.0F;
        prty = 11.0F;
    }
    else
    {
        prtx = 11.0F;
        prty = 17.0F;
    }
    /* if input figure is in landscape mode */
    if  ( xsz > ysz )
    {
        scalex = (float)prty/(float)(xsz/100);
        scaley = (float)prtx/(float)(ysz/100);
        if ((scalex >0.0F) && (scaley>0.0F) )
        {
            /* use the smaller of the two scale factors */
            if (scalex > scaley)
                scale_fact = scaley;
            else
                scale_fact = scalex;
        }

        printf("scale_factor is %f and prt_size is %i \n", scale_fact,
                                 prt_size);

        sprintf ( bufout,
       "%5.2f inch .25 inch translate 90 rotate %2.2f inch %2.2f inch scale\n",
			((double)(ysz/100)*.9),
                        ((float)(xsz/100)*scale_fact), ((float)(ysz/100) * scale_fact) );
    }
    else    /* if portrait mode */
    {
        scalex = (float)prtx/(float)(xsz/100);
        scaley = (float)prty/(float)(ysz/100);
        if ((scalex >0.0F) && (scaley>0.0F) )
        {
            /* use the smaller of the two scale factors */
            if (scalex > scaley)
                scale_fact = scaley;
            else
                scale_fact = scalex;
        }

        sprintf ( bufout,
         " .25 inch .25 inch translate %2.2f inch %2.2f inch  scale\n",
                                (float)(xsz/100)*scale_fact,
                                (float)(ysz/100)*scale_fact );
    }



    strcat (prolog, bufout);

    strcat ( prolog, "  { 1 exch sub } currenttransfer concatprocs\n");
    strcat ( prolog, "    settransfer\n");
    strcat ( prolog, "  imagefax\n");

/*
 *      Write prolog commands to file.
 */
    lenp = (int)strlen ( prolog );
    cfl_writ ( wfp, lenp, (unsigned char *)prolog, iret );


    /* read the image data from the file */
/*
 * First, get the size of the raster file to be read, then allocate
 * a buffer large enough to hold it.
 */
    cfl_inqr(infname, getenv("FAX_TEMP"), &flen, fname, iret);
    if (*iret < 0)
    {
        return;
    }

    bufin = (char *)malloc((size_t)flen);

    rfp = cfl_ropn(infname, NULL, iret);
    if (!rfp)
    {
        return;
    }


/*
 * Read the file data in...
 */
    cfl_read(rfp, (int)flen, (unsigned char *)bufin, &b_in, iret);


    printf("Read %i bytes from %s \n", b_in, infname);




    /* write the image data out to the file */
    line_ct = 0;    /* counts the number of character on a line */
    bufout[0] = '\0';


    i = 0;
    for(j=0;j<xsz/8;j++)
    {
        for (k = 0; k< ysz;k++)
        {

            if (i< b_in)
            {
                /* if even numbered byte, dont add that extra space... */
                if ((i%2) == 0)
                {
                    sprintf(tbuff, "%2.2x", (bufin[i] & 0xff ));
                }
                else
                {
                    sprintf(tbuff, "%2.2x ", ( bufin[i] & 0xff ));
                }
                strcat(bufout, tbuff);

                line_ct++;
                if ( line_ct > 15 )
                {
                    strcat(bufout, "\n");
                    cfl_writ ( wfp, (int)strlen(bufout), (unsigned char*)bufout, iret );
                    line_ct = 0;
                    bufout[0] = 0;
                }
            }
            else    /* nulls at bottom of file */
            {
                /* if even numbered byte, dont add that extra space... */
                if ((i%2) == 0)
                {
                    strcpy(tbuff, "00");
                }
                else
                {
                    strcpy(tbuff, "00 ");
                }
                strcat(bufout, tbuff);

                line_ct++;
                if ( line_ct > 15)
                {
                    strcat(bufout, "\n");
                    cfl_writ ( wfp, (int)strlen(bufout), (unsigned char*)bufout, iret );
                    line_ct = 0;
                    bufout[0] = 0;
                }
            }
            i++;
        }
    }
    free(bufin);
    strcat(bufout, "\n");
    cfl_writ ( wfp, (int)strlen(bufout), (unsigned char*)bufout, iret );

    strcpy(prolog,
      "gsave showpage grestore \n restore \n%%Trailer\n%%Pages: 1\n%%EOF\n\0");
    cfl_writ(wfp, (int)strlen(prolog), (unsigned char*)prolog, iret);

    cfl_clos (wfp, iret);
    cfl_clos (rfp, iret);

}
