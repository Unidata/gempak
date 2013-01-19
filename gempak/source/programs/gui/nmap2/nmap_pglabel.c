#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"
#include "pgprm.h"
#include "hints.h"
#include "NxmTxt.h"


static int	_oldClass;
static int	_oldObject;
static Boolean	_labelPending = False;
static int	_labelType = 1;  /* use Text as default label*/


/************************************************************************
 * nmap_pglabel.c							*
 *									*
 * This module contains label related functions				*
 *									*
 * CONTENTS:								*
 *  pglabel_txtpopup()   pop up a text window for label			*
 *  pglabel_popupOldWin  pop up the old window after adding a label	*
 *  pglabel_saveOldInfor save old infomation before adding a label	*
 *  pglabel_setLabFlag   set the label flag	                    	*
 *  pglabel_getLabFlag   get the label flag	                    	*
 *  pglabel_ifpopup()	 check if pop up old window			*
 *  pglabel_setLabelPending () Set label pending			*
 *  pglabel_getLabelPending () Get label pending			*
 *  pglabel_symbpopup()   pop up a symbol window for label		*
 *									*
 ***********************************************************************/

/*=====================================================================*/

void pglabel_txtpopup ( void )
/************************************************************************
 * pglabel_txtpopup							*
 *									*
 * This function popup a text editor for label.				*
 *									*
 * pglabel_txtpopup()							*
 *									*
 * Input parameters:                                                    *
 *  			NONE						*
 *                                                                      *
 * Output parameters:                                                   *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 *									*
 * W. Li/EAI		02/99						*
 * G. Krueger/EAI	05/99	Added circle draw function		*
 * W. Li/EAI		06/99	Modified for ghostting box setting	*
 * H. Zeng/EAI          04/00   modified for new grptyp.tbl             *
 * E. Safford/GSC	05/00   don't reset label text if label is null *
 * H. Zeng/EAI          05/00   fixed a bug                             *
 * H. Zeng/EAI          06/00   added label color flag check            *
 * H. Zeng/EAI          06/00   added parameter to pggst_clearGhost()   *
 * H. Zeng/EAI          06/00   increased the label size                *
 * M. Li/GSC		12/00	added pggrpw_getGrpTypColr		*
 * M. Li/GSC		01/01	line color = label color		*
 * M. Li/GSC		01/01	reset label color = line color		*
 * H. Zeng/EAI          03/01   preset text attrib. for line            *
 * J. Wu/SAIC		08/01	add braces for union initilization 	*
 * M. Li/SAIC		04/02	Removed FUNC_LABEL			*
 * J. Wu/SAIC		08/02	pop up symbol window for label	 	*
 * J. Wu/SAIC		08/02	remove symbol popup call	 	*
 * E. Safford/SAIC	07/04	add preset text attribs for symbol	*
 * E. Safford/SAIC	09/21	corrected case mapping, added 216, 226	*
************************************************************************/
{
char	    label[256], info[GRP_MXINFO];
char        *ptr, *ptr1, *ptr2;
int         ignore, ifont, ihwsw, itxwid, ibrdr, irrotn, ijust, iret;
float       siztxt;
Boolean     label_color = FALSE;
txt_attrib  color = {TRUE, TRUE, TRUE, {32} };
txt_attrib  line_color = {TRUE, TRUE, TRUE, {0} };
txt_attrib  line_size  = {TRUE, TRUE, TRUE, {0} };
txt_attrib  line_font  = {TRUE, TRUE, TRUE, {0} };
txt_attrib  line_style = {TRUE, TRUE, TRUE, {0} };
txt_attrib  line_box   = {TRUE, TRUE, TRUE, {0} };
txt_attrib  line_rotn  = {TRUE, TRUE, TRUE, {0} };
txt_attrib  line_align = {TRUE, TRUE, TRUE, {0} };
/*--------------------------------------------------------------------*/

    pgpalw_unmanageObjPal();
    
    _labelType = 1;  /* set label type to text */
            
    /*
     *  Pop up the text window when the user wants to label a line
     *  with text. 
     */        
    switch(_oldClass){

	case CLASS_LINES:
	    pgline_getLabValue(label);
            label_color = pgline_getLabColorFlag();
            if( label_color ) {
              pgline_getColor( &(color.value._i) );
            }
	    pgline_popdown();
	   break;
	case CLASS_FRONTS:
	    pgfrtw_getLabValue(label);
            label_color = pgfrtw_getLabColorFlag();
            if( label_color ) {
              color.value._i = pgfrtw_getFrtColor();
            }
	    pgfrtw_popdown();
	   break;

	case CLASS_SYMBOLS:
	    pgsymb_getLabValue(label);
            label_color = pgsymb_getLabColorFlag();
            if( label_color ) {
              pgsymb_getColor( &(color.value._i) );
            }
	    pgsymb_popdown();
	   break;

	case CLASS_CIRCLE:
	    pgcirc_getLabValue(label);
            label_color = pgcirc_getLabColorFlag();
            if( label_color ) {
              pgcirc_getAttr( &(color.value._i), &ignore );
            }
	    pgcirc_popdown();
	   break;

	default:
	   break;
    }

    pgpalw_dsplyObjPal (CLASS_TEXT);
    pgpalw_setCurBtns (FUNC_SELECT, CLASS_TEXT, OBJ_TEXTGEN);

    pgnew_setArmDynamic();
    mcanvw_setDynActFlag(FALSE);

    pgtxt_popup ();

    /*
     * Preset text attributes for line according to _lineAttrStr.
     */
    if( _oldClass == CLASS_LINES || _oldClass == CLASS_SYMBOLS ) {

	if( _oldClass == CLASS_LINES ) {
            pgline_getAttrStr( info );
        }
	else {
	    pgsymb_getAttrStr( info );
        }

	if( info[0] != '\0' ) {

            ptr = strtok(info, "|");
            ptr = strtok(NULL, "|");

            ptr2 = strstr(ptr, "/");
            ptr2 = ptr2 + 1;
            in_txtn(ptr2, &ifont, &ihwsw, &siztxt, &itxwid, &ibrdr, 
                          &irrotn,&ijust, &iret, strlen(ptr2) );
            line_style.value._i = ifont / 10;
            if (ihwsw == 1) {
	       /*
                * For software fonts:
                */
               line_font.value._i = (ifont % 10) + 2;

            }
            else if(ihwsw == 2) {
	       /*
                * For hardware fonts:
                */
               line_font.value._i = (ifont % 10) - 1;

            }
            ctb_fszfnd(siztxt, &(line_size.value._i), &iret);

	    /* 
	     *  This switch maps ibrdr values to special text types as 
	     *  described in gtxsy.f.
             */
            switch( ibrdr ) {
	        case 111:
                   line_box.value._i = 0;	/* plain text */
                   break;
	        case 222:
                   line_box.value._i = 1;	/* low pressure box */
                   break;
	        case 223:
                   line_box.value._i = 2;	/* high pressure box */
                   break;
	        case 211:
                   line_box.value._i = 3;	/* pressure, bounded, unfilled */
                   break;
	        case 221:
                   line_box.value._i = 4;	/* pressure, bounded, filled */
                   break;
	        case 121:
                   line_box.value._i = 5;	/* pressure, unbounded, filled */
                   break;
	        case 215:
                   line_box.value._i = 10;	/* underline */
                   break;
	        case 225:
                   line_box.value._i = 11;	/* underline, filled */
                   break;
 		case 216:
		   line_box.value._i = 13;	/* overline */
		   break;
                case 226:
		   line_box.value._i = 14;	/* overline, filled */
		   break;
	         default:
                   line_box.value._i = 0;
                   break;
               
            }
            line_rotn.value._i = (irrotn - 1) * 1000;
            line_align.value._i = ijust - 2;

            /*
             * Obtain color info.
             */
            ptr1 = strtok(ptr, "/");
            sscanf(ptr1, "%d", &(line_color.value._i));

            /*
             * Update text attribute window.
             */
            pgtxt_initBox   (line_box);
            pgtxt_initColor (line_color);
            pgtxt_initSize  (line_size);
            pgtxt_initFonts (line_font);
            pgtxt_initStyle (line_style);
            pgtxt_initRotn  (line_rotn);
            pgtxt_initAlign (line_align);

        }
        pgline_setAttrStr("\0");

    }

    /*
     * Set label color according to the label color toggle.
     */
    if ( label_color ) { 
        pgtxt_initColor ( color );
    }

    pgtxt_setGhostFlag (TRUE, pggst_setText);  /* set the function */
    pggst_clearGhost(TRUE);
    pgtxt_setGhostFlag (TRUE, pggst_setText);  /* using the function */

    if (strlen(label) <= (size_t)0) {
       pgtxt_getTextValue(FALSE, label);
    }

    pgtxt_setLabelValue (label);

}

/*=====================================================================*/

void pglabel_popupOldWin ( void )
/************************************************************************
 * pglabel_popupOldWin							*
 *									*
 * This function popup the old window after a label is added.		*
 *									*
 * pglabel_popupOldWin()						*
 *									*
 * Input parameters:                                                    *
 *  			NONE						*
 *                                                                      *
 * Output parameters:                                                   *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * W. Li/EAI		02/99						*
 * S. Law/GSC		03/99	removed call to pgline_updateChar	*
 * G. Krueger/EAI	05/99	Added circle draw function		*
 * H. Zeng/EAI          07/00   added a new para. to pg****_popup() for *
 *                              front, line, symbol and circle          *
 * E. Safford/GSC	01/01	add param to pgline_popup		*
 * E. Safford/GSC	03/01	add param to pgline_popup		*
 * J. Wu/SAIC		10/01	add param to pgline_popup		*
 * J. Wu/SAIC		07/02	pop down the symbol window if exists	*
 ***********************************************************************/
{
int	elmid, gemtyp, subtyp;
/*--------------------------------------------------------------------*/

    pgpalw_unmanageObjPal();

    pgtxt_popdown();
    pgtxt_setGhostFlag (FALSE, pggst_setText);
    
    if ( pgsymb_isUp() )  pgsymb_popdown();
    
    pgpalw_dsplyObjPal ((char)_oldClass);
    pgpalw_setCurBtns( 0, _oldClass, _oldObject);

    switch (_oldClass) {

	case CLASS_LINES:

	    pgobj_getId (_oldClass, _oldObject, &elmid, &gemtyp, &subtyp);

	    if ( elmid == LINE_ELM ) {
		pgline_popup(gemtyp, elmid, TRUE, FALSE, FALSE, FALSE,
                             NULL, NULL, 0, FALSE );
	    }
	    else {
		if ( gemtyp == 24 || gemtyp == 25 ) { /* kink line */
		    pgline_popup(gemtyp, elmid, TRUE, TRUE, TRUE, FALSE,
                             NULL, NULL, 0, FALSE );
		}
		else {
		    pgline_popup(gemtyp, elmid, TRUE, TRUE, FALSE, FALSE,
                             NULL, NULL, 0, FALSE );		
		}		
	    }

	    pgnew_setArmDynamic();

	    break;

	case CLASS_FRONTS:

            pgobj_getId (_oldClass, _oldObject, &elmid, &gemtyp, &subtyp);
	    pgfrtw_popup(gemtyp, TRUE, TRUE, FALSE, FALSE, NULL, FALSE);
	    pgnew_setArmDynamic();

	    break;

	case CLASS_SYMBOLS:

	    pgobj_getId (_oldClass, _oldObject, &elmid, &gemtyp, &subtyp);
	    pgsymb_popup (subtyp, elmid, FALSE, NULL, NULL, FALSE);
	    pgnew_setArmDynamic();
   	    mcanvw_setDynActFlag(FALSE);

	    break;

	case CLASS_CIRCLE:

	    pgobj_getId (_oldClass, _oldObject, &elmid, &gemtyp, &subtyp);
	    pgcirc_popup (gemtyp, elmid, TRUE, FALSE, NULL, NULL);
	    pgnew_setArmDynamic();
   	    mcanvw_setDynActFlag(FALSE);

	    break;

	default:

	    pgpalw_unmanageObjPal();
	    pgpalw_setCurBtns( FUNC_SELECT, CLASS_ANY, -1);

	    break;
    }

}

/*=====================================================================*/

Boolean pglabel_ifpopup ( void )
/************************************************************************
 * pglabel_ifpopup							*
 *									*
 * To check if pop up the old window after a label is added.		*
 *									*
 * Boolean pglabel_ifpopup()						*
 *									*
 * Input parameters:                                                    *
 *  			NONE						*
 *                                                                      *
 * Output parameters:                                                   *
 *  			NONE						*
 *                                                                      *
 * Return parameters:							*
 *	pglabel_ifpopup		Boolean		Window should pop up	*
 *									*
 **									*
 * Log:									*
 *									*
 * W. Li/EAI		02/99						*
 * G. Krueger/EAI	05/99	Added circle draw function		*
 ***********************************************************************/
{
   if (pgfrtw_getLabFlag() ||
       pgline_getLabFlag() ||      
       pgsymb_getLabFlag() ||
       pgcirc_getLabFlag())

	    return TRUE;
   else
	    return FALSE;
}


/*=====================================================================*/

void pglabel_setLabFlag ( Boolean lab_flag )
/************************************************************************
 * pglabel_setLabFlag							*
 *									*
 * This function sets label flag.					*
 *									*
 * pglabel_setLabFlag(lab_flag)						*
 *									*
 * Input parameters:                                                    *
 *  lab_flag	 	Boolean label flag				*
 *                                                                      *
 * Output parameters:                                                   *
 *			NONE						*
 **									*
 * Log:									*
 *									*
 * W. Li/EAI		02/99						*
 * G. Krueger/EAI	05/99	Added circle draw function		*
 ***********************************************************************/
{
    pgline_setLabFlag(lab_flag);
    pgfrtw_setLabFlag(lab_flag);
    pgsymb_setLabFlag(lab_flag);
    pgcirc_setLabFlag(lab_flag);
}

/*=====================================================================*/

void pglabel_saveOldInfor ( int class, int object )
/************************************************************************
 * pglabel_saveOldInfor							*
 *									*
 * To save the old information before selecting it to add a label	*
 *									*
 * pglabel_saveOldInfor(class, object)					*
 *									*
 * Input parameters:                                                    *
 *  class	int	class  ID.					*
 *  object	int	object ID.					*
 *                                                                      *
 * Output parameters:                                                   *
 *			NONE						*
 **									*
 * Log:									*
 *									*
 * W. Li/EAI		02/99						*
 * G. Krueger/EAI	05/99	Cleanup prologue			*
 ***********************************************************************/
{
    _oldClass   = class;
    _oldObject  = object;
}


/*=====================================================================*/

Boolean pglabel_getLabFlag ( void )
/************************************************************************
 * pglabel_getLabFlag							*
 *									*
 * This function gets LabelFlag. 					*
 *									*
 * Boolean pglabel_getLabFlag()						*
 *									*
 * Output parameters:							*
 *			NONE						*
 * 									*
 * Return value:							*
 *	pglabel_getLabFlag	Boolean		Add labels mode		*
 *									*
 **									*
 * Log:									*
 *									*
 * W. Li/EAI		02/99						*
 * G. Krueger/EAI	05/99	Added circle draw function		*
 ***********************************************************************/
{     
     if (pgpalw_getCurClassId() == CLASS_LINES) {
	 return(pgline_getLabFlag());
     }
     else if (pgpalw_getCurClassId() == CLASS_FRONTS) {
	 return(pgfrtw_getLabFlag());
     }
     else if (pgpalw_getCurClassId() == CLASS_SYMBOLS) {
	 return(pgsymb_getLabFlag());
     }
     else if (pgpalw_getCurClassId() == CLASS_CIRCLE) {
	 return(pgcirc_getLabFlag());
     }
     else{
	 return FALSE;
     }
}

/*=====================================================================*/

void pglabel_setLabelPending ( Boolean flag )
/************************************************************************
 * pglabel_setLabelPending                                              *
 *                                                                      *
 * This function sets Label Pending.                                    *
 *                                                                      *
 * void pglabel_setLabelPending ( flag )             			* 
 *                                                                      *
 * Input parameters:                                                    *
 *  flag	Boolean   	label pending flag			*
 *									*	
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *                                                                      *
 * M. Li/SAIC		04/02						*
 ***********************************************************************/
{

    _labelPending = flag;

}

/*=====================================================================*/

Boolean pglabel_getLabelPending ( void )
/************************************************************************
 * pglabel_getLabelPending                                              *
 *                                                                      *
 * This function gets Label Pending.                                    *
 *                                                                      *
 * Boolean pglabel_getLabelPending ()                                	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return value:                                                        *
 *      pglabel_getLabelPending Boolean	label pending  	       		*
 **                                                                     *
 * Log:                                                                 *
 *                                                                      *
 * M. Li/SAIC           04/02                                           *
 ***********************************************************************/
{
    return _labelPending; 
}

/*=====================================================================*/

void pglabel_symbpopup ( void )
/************************************************************************
 * pglabel_symbpopup							*
 *									*
 * This function popup a symbol window for label.			*
 *									*
 * pglabel_symbpopup()							*
 *									*
 * Input parameters:                                                    *
 *  			NONE						*
 *                                                                      *
 * Output parameters:                                                   *
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC		07/02	initial coding			 	*
 * J. Wu/SAIC		08/02	reset label type & adjust attributes	*
 ***********************************************************************/
{
char        *ptr, *ptr1, info[GRP_MXINFO];
int         sym_cls, sym_id, elmid, gemtyp, subtyp;
int         color, width, user_color, user_width;
float	    size, user_size;		
Boolean     label_color = FALSE;
/*--------------------------------------------------------------------*/

    pgpalw_unmanageObjPal (); 

    pgline_getSymbInfo ( &sym_cls, &sym_id );
    label_color = pgline_getLabColorFlag ();
    
    pgline_popdown();
    
    pgpalw_dsplyObjPal ( (char)sym_cls ); /* SYMBOLS, COMBSYM, and MARKER */
    pgpalw_setCurBtns ( 0, sym_cls, sym_id );

    pgnew_setArmDynamic ();
    mcanvw_setDynActFlag ( FALSE );

    pgobj_getId ( sym_cls, sym_id, &elmid, &gemtyp, &subtyp );
    pgsymb_popup ( subtyp, elmid, FALSE, NULL, NULL, TRUE );


    /*
     *  Do not label a symbol again. Also reset the label pending
     *  status to true and label type to symbol since this symbol
     *  is now used to label a line.
     */
    pgsymb_unmanageLabForm ();
    pglabel_setLabelPending ( True );     
    _labelType = 2;   

    /*
     * Preset symbol attributes according to _lineAttrStr.
     */
    if ( _oldClass == CLASS_LINES ) {

        /*
	 *  Retrieve current symbol attribute settings. 
	 */
	pgsymb_getColor( &color );
        pgsymb_getSize( &size );
        pgsymb_getWidth( &width );
        
        
	/*
	 *  Load setttings in grptyp.tbl, if presented. 
	 */
	pgline_getAttrStr ( info );

	if ( info[0] != '\0' ) {
	    ptr = strtok ( info, "|" );
            ptr = strtok ( NULL, "|" );
            
            ptr1 = strtok ( ptr, "/" );
	    if ( ptr1 ) {
		sscanf ( ptr1, "%d", &user_color );
                if ( user_color >= 1 && user_color <= 32 ) {
		    color = user_color;		
		}
		
		ptr1 = strtok ( NULL, "/" );
                if ( ptr1 ) {
		    sscanf ( ptr1, "%d", &user_width );            
	            if ( user_width >= 1 && user_width <= 10 ) {
		        width = user_width;
		    }
		    
		    ptr1 = strtok ( NULL, "/" );
                    if ( ptr1 )  { 
		        sscanf ( ptr1, "%f", &user_size );
	                if ( user_size >= 1.0F && user_size <= 10.0F ) {
			    size = user_size;
			}
                    }
		}
	    }
	}
	    
        /*
         *  Use the line color if required.
         */
        if ( label_color ) {
            pgline_getColor ( &color );
        }
    
	pgsymb_setAttr ( color, size, width );     	
	
	pgline_setAttrStr ( "\0" );

    } 
}

/*=====================================================================*/
int pglabel_getLabType ( void )

/************************************************************************
 * pglabel_getLabelType                                              	*
 *                                                                      *
 * This function gets the label type - either text or symbol.         	*
 *                                                                      *
 * int pglabel_getLabType ()                                		*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return value:                                                        *
 *      pglabel_getLabType	 int	label type  	       		*
 *                                      1 - text                  	*
 *                                      2 - symbol                  	*
 **                                                                  	*
 * Log:                                                                 *
 *                                                                      *
 * J. Wu/SAIC           08/02       initial coding			*
 ***********************************************************************/
{
    return _labelType; 
}

/*=====================================================================*/

void pglabel_setLabType ( int lab_type )

/************************************************************************
 * pglabel_setLabelType                                              	*
 *                                                                      *
 * This function sets the label type - text or symbol.                  *
 *                                                                      *
 * void pglabel_setLabType ( lab_type )                                	*
 *                                                                      *
 * Input parameters:                                                    *
 *	lab_type	int		Label type to be set		*
 *                                      1 - text                  	*
 *                                      2 - symbol                  	*
 *                                                                      *
 * Output parameters:                                                   *
 *	NONE                                            		*
 * Return value:                                                        *
 *	NONE                                            		*
 **                                                                  	*
 * Log:                                                                 *
 *                                                                      *
 * J. Wu/SAIC           08/02       initial coding                    	*
 ***********************************************************************/
{
    if ( lab_type == 1 || lab_type == 2 )  _labelType = lab_type; 
}

/*=====================================================================*/

int pglabel_getOldObject ( void )

/************************************************************************
 * pglabel_getOldObject                                              	*
 *                                                                      *
 * This function gets the obj. type saved before the text window popup	*
 *                                                                      *
 * int pglabel_getOldObject ( void )                      		*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return value:                                                        *
 *      pglabel_getOldObject	 int	Object type  	       		*
 **                                                                  	*
 * Log:                                                                 *
 *                                                                      *
 * J. Wu/SAIC           08/02       initial coding			*
 ***********************************************************************/
{
    return _oldObject; 
}

/*=====================================================================*/
