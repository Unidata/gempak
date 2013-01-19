#include "xwcmn.h"

/************************************************************************
 * xcsdat                                                   		*
 *                                                                      *
 * This function creates/changes shared data (a raw 8-bit data block)   *
 *  among the applications through the window property.  The parameter  *
 *  block_name uniquely identifies this data block.      		*
 *                                                                      *
 * int xcsdat(*dpy, block_name, data, nbyte)                		*
 *                                                                      *
 * Input parameters:                                                    *
 *  dpy             (Display *)     specifies a connection to           *
 *                                      an X server.                    *
 *  block_name      (char *)        identifies the data block.          *
 *  data            (char *)        data array .                        *
 *  nbyte           (unsigned  int) number of bytes of data.            *
 *                                                                      *
 * Return parameters:                                                   *
 *          G_NORMAL = normal return.                                   *
 *									*
 *          G_BADATOM  = error in interning an atom.   			*
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI       01/95                                               *
 ***********************************************************************/

int xcsdat ( register Display *dpy, char *block_name, char *data, 
						unsigned int nbyte )
{
Atom block_atom, blocktype_atom;
char blocktype_name[ATOM_NAME];

        /* The integer data array will be saved as a window
         *  property via an atom created by the using block_name.
         *  An auxiliary atom specifying the data type is also created
         *  internally using the name obtained by concatenating
         *  "_TYPE" to the block_name. 
	 */
	sprintf(blocktype_name, "%s_TYPE", block_name);
        block_atom = XInternAtom(dpy, block_name, False);
        blocktype_atom = XInternAtom(dpy, blocktype_name, False);

        if( (block_atom == (unsigned long)NULL) ||
                (blocktype_atom == (unsigned long)NULL) )
                return(G_BADATOM);


        XChangeProperty( (XtPointer)dpy, DefaultRootWindow( (XtPointer)dpy ),
                        block_atom, blocktype_atom,
                        8, PropModeReplace,
                        (unsigned char *)data, nbyte);

        return(G_NORMAL);

}
