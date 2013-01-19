#include "geminc.h"
#include "gemprm.h"
#include "cpgcmn.h"

/* #define DEBUG 1 */
#define FAXPROD_DELIM ":"
#define NUM_DELIMS 10
#define MAX_DESCRSZ 100

void pg_prse ( char *prod_str, PrdRec *pr, int *iret )
/************************************************************************
 * pg_prse								*
 *									*
 * This function parses a product file string into a product generation	*
 * record containing information for a specific product.		*
 *									*
 * pg_prse ( prod_str, pr, iret )					*
 *									*
 * Input parameters:							*
 * *prod_str	char 	String from product table			*
 *									*
 * Output parameters:							*
 *	*pr	PrdRec	Record of product info that contains:		*
 *		   psubset - product subset				*
 *		   ptype - string containing the type of product (AFOS, *
 *					FAX, etc)			*
 *		   rle - type of run length encoding to perform		*
 *		   xsize - size of product in number of x pixels	*
 *		   ysize - size of product in number of y pixels	*
 *		   rotate - rotation angle				*
 *		   bitpix - bits per pixel				*
 *		   origx - origin in x units				*
 *		   origy - origin in y units				*
 *		   sszx - size of original product in x coords.		*
 *		   sszy	- size of original product in y coords.		*
 *									*
 *	*iret	int	Return code					*
 **									*
 * Log:									*
 * E.Wehner/EAi		 7/96	Created					*
 * E. Wehner/EAi	 1/97	Swap wheel and subset			*
 * T. Piper/GSC		10/98	Prolog update				*
 * R. Tian/SAIC         05/02   Renamed pgcmn.h to be cpgcmn.h          *
 ***********************************************************************/
{

    char *p;
    int delim_ct = 0;
    size_t i = 0;

    *iret=G_NORMAL;


/*  First, count the number of delimeters.  If it is wrong
 *  report the error, and return...
 */    
    i = 0;
    while (i<strlen(prod_str) )
    {
        if ( prod_str[i] == ':')
            delim_ct++;
        i++;
    }


    /* see if AFOS, then do afos parsing...if fax, do fax parsing */
    /* throw away 1st token.... */
    p = strtok(prod_str, FAXPROD_DELIM);
    strncpy(pr->pwheel, p, 20);

    p = strtok(NULL, FAXPROD_DELIM);
    if (p)
        strncpy(pr->ptype, p, 20);
    else
        strcpy(pr->ptype, "FAX\0");

    if (strcmp(pr->ptype, "AFOS") == 0)
    {
        /*
         *  Now, go token by token through the product line, and if there
         *  is a valid value in the product line, copy it to the appropriate
         *  variable.  If there is not, set a default variable.  The defaults
         *  for AFOS are  NA projection, MAD 32512, tau 0, NMC, GPH covering
         *  the entire window area from the original display.
         */

        p = strtok(NULL, FAXPROD_DELIM);  /* tau */
        if ((p) && (p[0] != '~'))
	    pr->tau = atoi(p);
	else
	    pr->tau = 0;

	p = strtok(NULL, FAXPROD_DELIM);	/* projection key */
        if ((p) && (p[0] != '~'))
	    strncpy(pr->prjkey, p, MAX_NAMESZ-1);
	else
	    strcpy(pr->prjkey, "na");		/* default = north am. */

	p = strtok(NULL, FAXPROD_DELIM);	/* message address */
        if ((p) && (p[0] != '~'))
	    pr->msgadr = atoi(p);
	else
	    pr->msgadr = 32512;		/* default = 32512 */

	p = strtok(NULL, FAXPROD_DELIM);	/* message type */
        if ((p) && (p[0] != '~'))
	    pr->msgtyp = atoi(p);
	else
	    pr->msgtyp = 48;			/* default = 48 */

	p = strtok(NULL, FAXPROD_DELIM);	/* geography scale */
        if ((p) && (p[0] != '~'))
	    pr->geoscl = atoi(p);
	else
	    pr->geoscl = 2000;			/* default = 2000 */

	p = strtok(NULL, FAXPROD_DELIM);  /* for x size, doesnt matter */
        p = strtok(NULL, FAXPROD_DELIM);  /* for y size doesnt matter */
	pr->xsize = 2048;
        pr->ysize = 1536;

	/* Don't tokenize references, but if did, it would go here */

        p = strtok(NULL, FAXPROD_DELIM);	/* offset in X direction */
        if ((p) && (p[0] != '~'))
            pr->origx = atoi(p);
        else
            pr->origx = 0;

        p = strtok(NULL, FAXPROD_DELIM);	/* offset in Y direction */
        if ((p) && (p[0] != '~'))
            pr->origy = atoi(p);
        else
            pr->origy = 0;

	p = strtok(NULL, FAXPROD_DELIM);	/* ccc, should be "NMC" */
        if ((p) && (p[0] != '~'))
	    strncpy(pr->ccc, p, MAX_NAMESZ-1);
	else
	    strcpy(pr->ccc, "NMC");		/* default = NMC */

	p = strtok(NULL, FAXPROD_DELIM);	/* nnn, should be "NMC" */
        if ((p) && (p[0] != '~'))
	    strncpy(pr->nnn, p, MAX_NAMESZ-1);
	else
	    strcpy(pr->nnn, "GPH");		/* default = NMC */

	p = strtok(NULL, FAXPROD_DELIM);	/* UTF Header? */
        if ((p) && (p[0] != '~'))
	    strncpy(pr->utfhed, p, MAX_NAMESZ-1);
	else
	    pr->utfhed[0] = '\0';

	p = strtok(NULL, FAXPROD_DELIM);	/* projection indicator */
        if ((p) && (p[0] != '~'))
	    pr->prjind = atoi(p);
	else
	    pr->prjind = 1;			/* default = 1 */

	p = strtok(NULL, FAXPROD_DELIM);	/* font size */
        if ((p) && (p[0] != '~'))
	    pr->fontsz = atoi(p);
	else
	    pr->fontsz = 3;			/* default = 3 */

        p = strtok(NULL, FAXPROD_DELIM);
        if ((p) && (p[0] != '~'))
            pr->sszx = (float)atof(p);
        else
            pr->sszx = -1.0F;

        p = strtok(NULL, FAXPROD_DELIM);
        if ((p) && (p[0] != '~'))
            pr->sszy = (float)atof(p);
        else
            pr->sszy = -1.0F;

#ifdef DEBUG
	printf("Pil->%s xsize->%f ysize->%f origx->%f origy->%f \n", 
		pr->pwheel, pr->xsize, pr->ysize, pr->origx, pr->origy);
	printf("tau->%i prjkey->%s msgaddr->%i msgtyp->%i geoscl->%i %s%s \n",
		pr->tau, pr->prjkey, pr->msgadr, pr->msgtyp, pr->geoscl,
		pr->ccc, pr->nnn);
	
#endif
    }
    else    /* FAX or other product */
    {


        /*
         *  Now, go token by token through the product line, and if there
         *  is a valid value in the product line, copy it to the appropriate
         *  variable.  If there is not, set a default variable.  The defaults
         *  are "FAX NMC-6bit product with no rotation, 1728 pix**2, covering
         *  the entire window area from the original display.
         */

	p = strtok(NULL, FAXPROD_DELIM);
	if ((p) && (p[0] != '~'))
	    strncpy(pr->psubset, p, MAX_DESCRSZ);
	else
	    strcpy(pr->psubset, "0167\0");

        p = strtok(NULL, FAXPROD_DELIM);
        if ((p) && (p[0] != '~'))
            strncpy(pr->pdesc, p, MAX_DESCRSZ);
        else
            strcpy(pr->pdesc, "NAWIPS TEST MAP\0");


        p  = strtok(NULL, FAXPROD_DELIM);
        if ((p) && (p[0] != '~'))
            strncpy(pr->rle, p, 20);
        else
            strcpy(pr->rle, "NMC6\0");

        p = strtok(NULL, FAXPROD_DELIM);
        if ((p) && (p[0] != '~'))
            pr->ysize = (Cardinal)atoi(p);
        else
            pr->ysize = 1728;

        p = strtok(NULL, FAXPROD_DELIM);
        if ((p) && (p[0] != '~'))
            pr->xsize = (Cardinal)atoi(p);
        else
        	pr->xsize = 1728;


        p = strtok(NULL, FAXPROD_DELIM);
        if ((p) && (p[0] != '~'))
            pr->rotate = (float)atof(p);
        else
            pr->rotate = 0.0F;

        p = strtok(NULL, FAXPROD_DELIM);
        if ((p) && (p[0] != '~'))
            pr->bitpix = atoi(p);
        else
            pr->bitpix = 1;

        p = strtok(NULL, FAXPROD_DELIM);
        if ((p) && (p[0] != '~'))
            pr->origx = atoi(p);
        else
            pr->origx = 0;

        p = strtok(NULL, FAXPROD_DELIM);
        if ((p) && (p[0] != '~'))
            pr->origy = atoi(p);
        else
            pr->origy = 0;

        p = strtok(NULL, FAXPROD_DELIM);
        if ((p) && (p[0] != '~'))
            pr->sszx = (float)atof(p);
        else
            pr->sszx = -1.0F;

        p = strtok(NULL, FAXPROD_DELIM);
        if ((p) && (p[0] != '~'))
            pr->sszy = (float)atof(p);
        else
            pr->sszy = -1.0F;

#ifdef DEBUG
        printf("ptype->%s rle->%s xsz->%i %i \n", pr->ptype,pr->rle, 
				pr->xsize,pr->ysize);
        printf("rotate->%f bitpix->%i origx->%i origy->%i sszx->%f sszy->%f \n",
              pr->rotate, pr->bitpix, pr->origx, pr->origy, pr->sszx,pr->sszy);
#endif
    }

    return;
}
