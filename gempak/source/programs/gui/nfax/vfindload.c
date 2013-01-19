#include "geminc.h"
#include "gemprm.h"
#include "cpgcmn.h"


extern Widget cad_form;
extern Widget toplevel;
extern Widget busy;
extern Pixmap pmap;
extern int printing;       /* whether we are printing or not */
extern int samp_factor;


int vfindload ( int node, PrdRec *pr )
/************************************************************************
 * vfindload								*
 *									*
 * This function finds and loads the requested bitmap file into either 	*
 * a draw area or to a printable postscript file.			*
 *									*
 * int vfindload  ( node, pr)						*
 *									*
 * Input parameters:							*
 *  	node	int	Node from which load is to be performed 	*
 * 	*pr 	PrdRec	Product record for this request			*
 *									*
 * Output parameters:							*
 * vfindload	int							*
 **									*
 * Log:									*
 *  E. Wehner/EAi	6/96		Created				*
 *  E. Wehner/EAi	1/97	Swap wheel and subset on pgsixread	*
 *  E. Wehner/EAi	3/97	Change pg_sixread -> cpg_sixrd		*
 *  E. Wehner/EAi	5/97	Switched file names.			*
 *  E. Wehner/EAi	5/97	Correctly interpret x and y sizes	*
 *  R. Tian/SAIC        05/02   Renamed pgcmn.h to be cpgcmn.h          *
 ***********************************************************************/
{
    int istat;
    int mirror;
    int flip;
    char fnm[120];
    char prt_name[20];
    char new_name[133];
    Cardinal bpscan;
    Cardinal numscans;


/*  set the bitsperscan and the numberofscanlines */
        if (pr->ysize > 1728)
        {  /* flip, no questions asked */
            pr->rotate = 90.0F;
            numscans = pr->ysize;
            bpscan = pr->xsize;
        }
        else
        {
            if  ((pr->rotate > 89.0F) && (pr->rotate < 91.0F))
            {
                pr->rotate = 90.0F;
                numscans = pr->ysize;
                bpscan = pr->xsize;
            }
            else
            {
                bpscan = pr->ysize;
                numscans = pr->xsize;
            }
        }


/* 
 * 	Loads are performed based on the node item that was checked
 *	on the wheel selection dialog box.  A fax product is sent to
 * 	a variety of places on its way to a consumer.  It can exist
 *	as an uncompressed raster entity, a compressed raster entity
 *	on the local machine, as an entity at a fax consumers location,
 * 	or as a packed 6 bit entity at the fax plot server.  Each one
 *      of these posibilities is addressed as a node, and the following
 *	case switch handles the processing which is unique to each node.
 */

    switch(node)
    {

      case 1:     /* load file from local node . It is uncompressed */

        sprintf(fnm, "%s/%s.ras", getenv("FAX_TEMP"), pr->pwheel);
        mirror = 1;
        flip  = 1;

 
        if (printing)
        {
            pg_print(fnm, (int)pr->xsize, (int)pr->ysize, 
                        samp_factor, prt_name, pr->pagesz, new_name, &istat);
            if (istat != 0)
                    return -1;
        }
        else
        {
            if (vfiledisp(fnm, bpscan, numscans, 
			samp_factor,mirror, flip,&pmap)  != 0)
                return -1;
        }
        break;
      case 2:     /* load file from 6 bit on local node */
        mirror = 1;
        flip = 1;

/* change subset to wheel EJW */
        sprintf(pr->pfname, "%s/%s.6bt", getenv("FAX_TEMP"), pr->pwheel);

/* change subset to wheel EJW */
        cpg_sixrd(pr->pfname, pr->psubset, pr->pdesc, (int*)&pr->xsize,
		 (int*)&pr->ysize, &pr->bitpix, &istat);
        if (istat != 0)
        {
            return -1;
        }

        vtitle(cad_form, pr->pdesc);

        sprintf(fnm, "%s/%s.ras", getenv("FAX_TEMP"), pr->psubset);
/* no error....display the file */
        if (printing)
        {
            pg_print(fnm, (int)pr->xsize, (int)pr->ysize, 
                        samp_factor, prt_name, pr->pagesz, new_name, &istat);
            if (istat != 0)
                    return -1;
        }
        else
        {
            if (vfiledisp(fnm, pr->xsize, pr->ysize, 
                                samp_factor,mirror, flip, &pmap) 
                               != 0)
                return -1;
	    }
        break;
      case 3:     /* load 6bit file from OSO node */
        break;
      case 4:     /* load 6 bit file from plot server node */

/* now, request ftp transfer of the file */
        pg_getfile(pr->pfname);

        XtManageChild(busy);
        XmUpdateDisplay(toplevel);


        cpg_sixrd(pr->pfname, pr->pwheel, pr->pdesc, 
		(int*)&pr->xsize, (int*)&pr->ysize,
       			  &pr->bitpix, &istat);

        XmUpdateDisplay(toplevel);
        if (istat != 0)
        {
            return -1;
        }
        sprintf(fnm, "%s/%s.ras", getenv("FAX_TEMP"), pr->pwheel);
/* no error....display the file */
        if (printing)
        {
            pg_print(fnm, (int)pr->xsize, (int)pr->ysize, 
                        samp_factor, prt_name, pr->pagesz, new_name, &istat);
            if (istat != 0)
                    return -1;
        }
        else
        {
            mirror =0;
            flip =0;

            vtitle(cad_form, pr->pdesc);

            XmUpdateDisplay(toplevel);
            if (vfiledisp(fnm, pr->xsize, pr->ysize, 
                           samp_factor,mirror, flip, &pmap) 
                               != 0)
             return -1;
        }

        XtUnmanageChild(busy);

        break;
      default:
        break;
    } 
    return 0;
}
