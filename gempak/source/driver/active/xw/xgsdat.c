#include "xwcmn.h"

int xgsdat ( register Display *dpy, char *block_name, 
			unsigned char *data, unsigned int *nbyte )
/************************************************************************
 * xgsdat                                                   		*
 *                                                                      *
 * This function gets the shared data (a raw 8-bit data block)    	*
 *  among the applications through the window property.  The parameter  *
 *  block_name uniquely identifies this data block.      		*
 *                                                                      *
 * int xgsdat(*dpy, block_name, data, nbyte)                		*
 *                                                                      *
 * Input parameters:                                                    *
 *  *dpy	register Display  specifies a connection to an X server.*
 *  *block_name char              identifies the data block.            *
 *                                                                      *
 * Output parameters:                                                   *
 *  *data       unsigned char     data array.                           *
 *                                                                      *
 * Input/Output parameters:                                             *
 *  *nbyte      unsigned int      number of bytes of data.              *
 *                                                                      *
 * Return parameters:                                                   *
 * xgsdat	int         G_NORMAL     = normal return.               *
 *          G_NSRDAT     = no data stored in the atom.       		*
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       	02/95                                           *
 * E. Safford/GSC	02/01	Remove shared_data mem leak (awc fix)	*
 ***********************************************************************/
{
int		bytes, format;
Atom            block_atom, blocktype_atom, type;
char            blocktype_name[ATOM_NAME];
unsigned long   ii, nitems, left;
unsigned char   *shared_data;
/*---------------------------------------------------------------------*/

	bytes = *nbyte;

        /* The shared data array will be obtained as
         *  a window property via an atom interned by the block_name.
         *  An auxiliary atom specifying the data type is also interned
         *  internally using the name obtained by concatenating
         *  "_TYPE" to the block_name. 
	 */
        sprintf(blocktype_name, "%s_TYPE", block_name);
        block_atom = XInternAtom(dpy, block_name, True);
        blocktype_atom = XInternAtom(dpy, blocktype_name, True);

	/*
	 * If the atoms don't exist, no share data were created
	 */
        if ( (block_atom == (unsigned long)NULL) ||
                        (blocktype_atom == (unsigned long)NULL) ) {
                return (G_NSRDAT);
	}

        if ( XGetWindowProperty( (XtPointer)dpy, DefaultRootWindow( (XtPointer)dpy ),
                block_atom, 0,
                bytes, FALSE,
                blocktype_atom,
                &type, &format, &nitems, &left,
                (unsigned char **)(&shared_data) ) != Success ) {
           free( shared_data);
           return (G_NSRDAT);
        }
        else {
           for (ii=0; ii<nitems; ii++)
                data[ii] = shared_data[ii];
	   *nbyte = nitems;
	}

        free( shared_data );

        return (G_NORMAL);

}
