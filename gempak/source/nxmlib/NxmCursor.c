#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "NxmInit.h"


#define CURSORTYPE_TBL 	"cursortyp.tbl"
#define CURSORREF_TBL   "cursorref.tbl"
#define CURSORSYMB_TBL  "cursorsymb.tbl"

#define ICON_DIR        "$NAWIPS/icons/cursor"
#define MAX_CURSOR_REF  10
#define IMPVAL          -99


typedef struct {
    char    X_name[25];      /* X standard cursor font symbol name*/
    int     id;              /* X standard cursor font symbol id */
}cursorsymb_t;     /* data structure for standard cursor symbol */

typedef struct {
    int            nsymb;     /* total # of cursor symbols */
    cursorsymb_t*  cursymbs;  /* array of cursor symbols */
}cursymbTbl_t;  /* data structure for standard cursor symbol table */


static curtypTbl_t     _curtypTbl;  
static currefTbl_t     _currefTbl;
static cursymbTbl_t    _cursymbTbl;

static Cursor	       _cursorRef[MAX_CURSOR_REF];
static Drawable        _bxmWin;
static Screen	       *_bxmScreen;


/*
 *  Private functions
 */
void NxmCursor_rdTbls ( void );


/************************************************************************
 * NxmCursor.c								*
 * 									*
 * This module contains functions that related to the cursor use in     *
 * N_AWIPS.                                                             *
 * 									*
 * CONTENTS:								*
 *									*
 * NxmCursor_createCursor()  create the cursor for a reference index    *
 * NxmCursor_setCursor()     set the current cursor reference           *
 *                                                                      *
 * NxmCursor_getTypTbl()     query the cursor type table info.          *
 * NxmCursor_getRefTbl()     query the cursor ref. table info.          *
 *                                                                      *
 * NxmCursor_rdTbls()        read cursor type&reference tables          *
 ***********************************************************************/

/*=====================================================================*/

void NxmCursor_setCursor ( Widget parent, int ref )
/************************************************************************
 * NxmCursor_setCursor							*
 *									*
 * This function changes the cursor for the widget.			*
 *									*
 * void NxmCursor_setCursor(parent, ref)			        *
 *									*
 * Input parameters:							*
 *	parent	      Widget	   ID of parent widget			*
 *	ref           int	   reference # of cursor		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		05/94						*
 * S. Law/GSC		09/99	check if cursor has been created	*
 * H. Zeng/EAI          04/00   changed name&parameters                 *
 ***********************************************************************/
{
    int			ii;
    static Boolean	first = TRUE;
/*---------------------------------------------------------------------*/

    if (!NXMisInitialized) {
	NxmInitialize (parent);
    }

    if ( _bxmWin == (Drawable)NULL ) {
	_bxmScreen   = XtScreen(parent);
        _bxmWin = RootWindowOfScreen(_bxmScreen);
    }

    if (first) {

/*
 * Read cursor type and cursor reference tables
 */
        NxmCursor_rdTbls();

/*
 * Initialize the cursors
 */
	for (ii = 0; ii < _currefTbl.nref; ii++) {
            _cursorRef[ii] = (Cursor)0;
	    NxmCursor_createCursor(ii);
	}

	first = FALSE;
    }

    XDefineCursor (NXMdisplay, XtWindow (parent), _cursorRef[ref]);

}

/*=====================================================================*/

void NxmCursor_rdTbls ( void )
/************************************************************************
 * NxmCursor_rdTbls                                             	*
 *                                                                      *
 * This function reads the cursor type table and cursor reference table *
 *                                                                      *
 * void NxmCursor_rdTbls()                                            	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          04/00     initial coding                        *
 ***********************************************************************/
{
int  ii, jj, nr = 0, ignore, iret;
char buffer[256];
FILE *fp;

/*---------------------------------------------------------------------*/
/*
 * Read cursorsymb.tbl
 */
	_cursymbTbl.nsymb = 0;

        fp = cfl_tbop( CURSORSYMB_TBL, "config", &iret );
	if ( iret == 0 ) { 
	    cfl_tbnr( fp, &nr, &iret);
	}
	    
	if ( nr == 0 ) {
	    fclose(fp);
        }
        else {
           _cursymbTbl.nsymb = nr;
           _cursymbTbl.cursymbs = (cursorsymb_t*)
                           malloc((size_t)_cursymbTbl.nsymb*sizeof(cursorsymb_t));

           ii = 0;
           while (ii < _cursymbTbl.nsymb) {
               cfl_trln(fp, 256, buffer, &iret);

               if ( iret == 0 ) {
                   sscanf(buffer, "%s %d", _cursymbTbl.cursymbs[ii].X_name, 
				  &(_cursymbTbl.cursymbs[ii].id) );
         
	       }

               ii++;
	   }      

           fclose(fp);
	}   

/*
 * Read cursortyp.tbl
 */
	_curtypTbl.ntyp = 0;

        fp = cfl_tbop( CURSORTYPE_TBL, "config", &iret );
	if ( iret == 0 ) { 
	    cfl_tbnr( fp, &nr, &iret);
	}
	    
	if ( nr == 0 ) {
	    fclose(fp);
        }
        else {
           _curtypTbl.ntyp = nr;
           _curtypTbl.curtyps = (cursortyp_t*)
                             malloc((size_t)_curtypTbl.ntyp*sizeof(cursortyp_t));

           ii = 0;
           while (ii <  _curtypTbl.ntyp) {
               cfl_trln(fp, 256, buffer, &iret);

               if ( iret == 0 ) {
                   sscanf(buffer, "%s %s", 
                          _curtypTbl.curtyps[ii].ext_name, 
			  _curtypTbl.curtyps[ii].int_name  );
                   
/*
 * By default id value is -1 which means it is a pixmap cursor.
 */
                   _curtypTbl.curtyps[ii].id = -1;

                   if( strstr(_curtypTbl.curtyps[ii].int_name, 
                              "XC_") != NULL ) {

                     for(jj=0; jj<_cursymbTbl.nsymb; jj++) {
                        if(strcmp(_curtypTbl.curtyps[ii].int_name,
                           _cursymbTbl.cursymbs[jj].X_name)==0 ) {

                           _curtypTbl.curtyps[ii].id = 
                                       _cursymbTbl.cursymbs[jj].id;
                            break; 
                        }

                     } /* the end of for() */

		   } /* the end of if(strstr...) */

	       }

               ii++;
	   }      

           fclose(fp);
	}   

/*
 * Read cursorref.tbl
 */
	_currefTbl.nref = 0;

        fp = cfl_tbop( CURSORREF_TBL, "config", &iret );
	if ( iret == 0 ) { 
	    cfl_tbnr( fp, &nr, &iret);
	}
	    
	if ( nr == 0 ) {
	    fclose(fp);
        }
        else {
           _currefTbl.nref = nr;
           _currefTbl.currefs = (cursorref_t*)
                             malloc((size_t)_currefTbl.nref*sizeof(cursorref_t));

           ii = 0;
           while (ii < _currefTbl.nref) {
               cfl_trln(fp, 256, buffer, &iret);

               if ( iret == 0 ) {
                   sscanf(buffer, "%d %s %s %s", &ignore, 
                                  _currefTbl.currefs[ii].ref_name, 
				  _currefTbl.currefs[ii].typ_name,
                                  _currefTbl.currefs[ii].color    );
         
	       }

               ii++;
	   }      

           fclose(fp);
	}   

        free(_cursymbTbl.cursymbs);

}

/*=====================================================================*/

void NxmCursor_createCursor ( int ref )
/************************************************************************
 * NxmCursor_createCursor						*
 *									*
 * This function creates the cursor for a specific reference index	*
 *									*
 * void NxmCursor_createCursor(ref)			                *
 *									*
 * Input parameters:							*
 *	ref           int	   reference # of cursor		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          04/00   initial coding                          *
 * H. Zeng/EAI          05/00   added incorrect info. handling          *
 ***********************************************************************/
{
    int		   ii, x_hot, y_hot;
    int            cursor_id = IMPVAL, iret1, iret2;
    long int       no_use;
    unsigned int   width, height;       
    Pixmap         cursor_pixmap, mask_pixmap;
    XColor         fgcolr, bgcolr, ignore;
    char           cursor_name[20], pix_name[25], cursor_file[128]; 
    char           cursor_mask[128], cursor_color[10];
    char           bg_color[] = "black", *ptr;
    Status         status;
/*---------------------------------------------------------------------*/
/*
 * Find cursor type and cursor color according to the reference #.
 */
    strcpy(cursor_name, _currefTbl.currefs[ref].typ_name);
    strcpy(cursor_color, _currefTbl.currefs[ref].color);

    for(ii = 0; ii < _curtypTbl.ntyp; ii++) {
       if( strcmp(cursor_name,_curtypTbl.curtyps[ii].ext_name)==0 ) {
         strcpy(pix_name, _curtypTbl.curtyps[ii].int_name);
         cursor_id = _curtypTbl.curtyps[ii].id;
         break; 
       }
    }

/*
 * For garbage cursor_name
 */
    if(cursor_id == IMPVAL) {
      switch(ref) {
         case 0:
            cursor_id = 132;
            break;
         case 1:
            cursor_id = 34;
            break;
         case 2:
            cursor_id = 0;
            break;
         default:
            cursor_id = 132;
            break;
      }

    }

/*
 * Get foreground color and background color.
 */
    fgcolr.flags = DoRed | DoBlue | DoGreen;
    status = XAllocNamedColor (NXMdisplay, NXMcmap, cursor_color, 
                               &fgcolr, &ignore);

/*
 * For garbage cursor_color
 */
    if(status == 0) {
       XAllocNamedColor (NXMdisplay, NXMcmap, "white", 
                         &fgcolr, &ignore);
    }

    bgcolr.flags = DoRed | DoBlue | DoGreen;
    XAllocNamedColor (NXMdisplay, NXMcmap, bg_color, 
                      &bgcolr, &ignore);


/*
 * According to whether cursor_id is -1 or not, create 
 * pixmap cursor or standard cursor.
 */
    if( cursor_id != -1 ) {

/*
 * create standard cursor
 */
      if( _cursorRef[ref] != (Cursor)0 ) {
         XFreeCursor(NXMdisplay, _cursorRef[ref]);
      }

      _cursorRef[ref] = XCreateFontCursor(NXMdisplay, (Cardinal)cursor_id);

      XRecolorCursor (NXMdisplay, _cursorRef[ref], &fgcolr, &bgcolr);

    }
    else {

      strcat(pix_name, ".xbm");
      cfl_inqr (pix_name, ICON_DIR, &no_use, cursor_file, &iret1 ); 
      ptr = strstr(pix_name, ".xbm");
      sprintf (ptr, "_mask.xbm");
      cfl_inqr (pix_name, ICON_DIR, &no_use, cursor_mask, &iret2 ); 

      if( (iret1 != 0) || (iret2 != 0) ) {
  
/*
 * File not found !
 */
         fprintf(stderr, "\nPixmap file not found!\n");
         fprintf(stderr, "Please check the cursor type table.\n\n");
         switch(ref) {
	    case 0:
              cfl_inqr ("lg_arrow.xbm", ICON_DIR, &no_use, 
                        cursor_file, &iret1 );
              cfl_inqr ("lg_arrow_mask.xbm", ICON_DIR, &no_use, 
                        cursor_mask, &iret2 );
              break;
	    case 1:  
              cfl_inqr ("lg_cross.xbm", ICON_DIR, &no_use, 
                        cursor_file, &iret1 );
              cfl_inqr ("lg_cross_mask.xbm", ICON_DIR, &no_use, 
                        cursor_mask, &iret2 );
              break;
	    case 2:
              cfl_inqr ("lg_x.xbm", ICON_DIR, &no_use, 
                        cursor_file, &iret1 );
              cfl_inqr ("lg_x_mask.xbm", ICON_DIR, &no_use, 
                        cursor_mask, &iret2 );
              break;
	    default:
              cfl_inqr ("lg_arrow.xbm", ICON_DIR, &no_use, 
                        cursor_file, &iret1 );
              cfl_inqr ("lg_arrow_mask.xbm", ICON_DIR, &no_use, 
                        cursor_mask, &iret2 );
              break;

         }

      }  

      XReadBitmapFile(NXMdisplay, _bxmWin, cursor_mask,	
	              &width, &height, &mask_pixmap, 
		      &x_hot, &y_hot );

      XReadBitmapFile(NXMdisplay, _bxmWin, cursor_file,	
	              &width, &height, &cursor_pixmap, 
		      &x_hot, &y_hot );

      if( _cursorRef[ref] != (Cursor)0 ) {
         XFreeCursor(NXMdisplay, _cursorRef[ref]);
      }

      _cursorRef[ref] = XCreatePixmapCursor(NXMdisplay, cursor_pixmap, 
                                            mask_pixmap, &fgcolr, &bgcolr, 
                                            (Cardinal)x_hot, (Cardinal)y_hot );

      XFreePixmap(NXMdisplay, mask_pixmap);
      XFreePixmap(NXMdisplay, cursor_pixmap);

    }/* the end of else part */

}

/*=====================================================================*/

curtypTbl_t *NxmCursor_getTypTbl ( void )
/************************************************************************
 * NxmCursor_getTypTbl							*
 *									*
 * This function queries the current current cursor type table          *
 * information  	                                                *
 * 									*
 *									*
 * curtypTbl_t*   NxmCursor_getTypTbl()					*
 *									*
 * Input parameters:							*
 *	        	NONE				                *
 *									*
 * Output parameters:							*
 * NxmCursor_getTypTbl     curtypTbl_t*	  cursor type table		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		05/00	initial coding				*
 ***********************************************************************/
{
    return (&_curtypTbl);
}

/*=====================================================================*/

currefTbl_t *NxmCursor_getRefTbl ( void )
/************************************************************************
 * NxmCursor_getRefTbl							*
 *									*
 * This function queries the current current cursor ref. table          *
 * information  	                                                *
 * 									*
 *									*
 * currefTbl_t*   NxmCursor_getRefTbl()					*
 *									*
 * Input parameters:							*
 *	        	NONE				                *
 *									*
 * Output parameters:							*
 * NxmCursor_getRefTbl     currefTbl_t*	  cursor ref. table		*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI		05/00	initial coding				*
 ***********************************************************************/
{
    return (&_currefTbl);
}
