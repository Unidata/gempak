#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"

/************************************************************************
 * nmap_pgobj.c                                                         *
 *                                                                      *
 * This module containts a function which defines objects element ID, 	*
 * gempak type, and sub type in product generation.			*
 *                                                                      *
 * CONTENTS:                                                            *
 *									*
 *      pgobj_getId()      Get element ID, gempak type, and sub type.	*
 ***********************************************************************/

/*=====================================================================*/

void pgobj_getId ( int class_id, int obj_id, int *elm_id, 
					int *gem_typ, int *sub_typ )
/************************************************************************
 * pgobj_getId								*
 *                                                                      *
 * This function returns elemaent ID, gempak type, and sub type to 	*
 * calling function according to class ID and object ID.		*
 *                                                                      *
 * void pgobj_getId ( class_id, obj_id, elm_id, gem_typ, sub_typ )	*
 *                                                                      *
 * Input parameters:                                                    *
 *       class_id       int     Class ID.                               *
 *       obj_id         int     Object ID.                              *
 *                                                                      *
 * Output parameters:                                                   *
 *      *elm_id        int     Return element ID.			*
 *      *gem_typ       int     Return gempak type.			*
 *      *sub_typ       int     Return sub type.				*
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI            02/98                                           *
 * W. Li/EAI		03/98	Add new symbols				*
 * W. Li/EAI		04/98	Add OBJ_SPTEXTUD			*
 * C. Lin/EAI		04/98	Add combo symbols			*
 * W. Li/EAI		04/98	Add darr and hash in winds		*
 * S. Law/GSC		05/98	Added changes from drwids.h		*
 * C. Lin/EAI		06/98	Add more dash lines:DSLN2-DSLN9		*
 * G. Krueger/EAI	08/98	Add STMCNTR, TRPDPRSN, and TRPCYCLN	*
 * F. J. Yen/NCEP	09/98	Added more special lines:  SPLN20-SPLN21*
 * E. Safford/GSC	10/98	Mod COMSYM, remove getCombId		*
 * G. Krueger/EAI	 1/99	Add OBJ_XCROSS and OBJ_LOWX		*
 * S. Jacobs/NCEP	 2/99	Added OBJ_SQUALL			*
 * S. Jacobs/NCEP	 2/99	Changed names for/Added new combo syms	*
 * S. Jacobs/NCEP	 3/99	Added special line: SPLN22		*
 * W. Li/EAI		05/99	Added CLASS_WATCHES			*
 * G. Krueger/EAI       05/99   Added circle draw function              *
 * W. Li/EAI		05/99	Added CLASS_TRACKS			*
 * S. Law/GSC		07/99	Added CLASS_SIGMETS			*
 * G. Krueger/EAI	08/99	N & SH trop storm specials		*
 * T. Piper/GSC		12/99	Added OBJ_HAZE				*
 * S. Law/GSC		02/00	Added CCF				*
 * S. Jacobs/NCEP	 3/01	Added OBJ_TRPTFNT			*
 * S. Jacobs/NCEP	 9/01	Added OBJ_NUCLEAR			*
 * J. Wu/SAIC		10/01	add OBJ_SPLN23 - double line		*
 * M. Li/SAIC		10/01	Added MARKER				*
 * J. Wu/SAIC		10/01	add OBJ_KINKLN1, 2 - kink arrow lines	*
 * M. Li/SAIC		10/01	Added OBJ_TEXTICNG			*
 * J. Wu/SAIC		11/02	add CLASS_LIST				*
 * J. Wu/SAIC		02/03	add OBJ_TEXTMCLOUD - midlevel cloud	*
 * m/gamazaychikov/SAIC 04/03   added special symbols 42 thru 49        *
 * m/gamazaychikov/SAIC 04/03   added combo symbol number 28		*
 * H. Zeng/XTRIA	07/03	added volcano and ash cloud		*
 * J. Wu/SAIC		11/03	add CLASS_MET/OBJ_JET			*
 * A. Hardy/NCEP	12/03   added special symbol 50			*
 * J. Wu/SAIC		02/04	add OBJ_AIRMET under CLASS_MET		*
 * J. Wu/SAIC		03/04	add OBJ_NCONSIG under CLASS_MET		*
 * A. Hardy/NCEP	03/04   added OBJ_LISTWBCMZ			*
 * J. Wu/SAIC		05/04	add OBJ_GFA				*
 * J. Wu/SAIC		09/04	remove OBJ_AIRMET & OBJ_NCONSIG		*
 * S. Gilbert/NCEP	06/05	add OBJ_SPLN26 - ZZZZZZ line		*
 * B. Yin/SAIC		11/05	remove subtype for GFA			*
 * E. Safford/SAIC	03/07	add OBJ_GFA_P				*
 * S. Jacobs/NCEP	04/08	Added OBJ_DSHLN10			*
 * S. Jacobs/NCEP	 5/09	Added OBJ_SPSYM51 thru OBJ_SPSYM56	*
 ***********************************************************************/
{

    *elm_id  = 0;
    *gem_typ = 0;
    *sub_typ = 0;

    switch (class_id) {

      /*=========== CLASS_WATCHES ==============*/	

      case CLASS_WATCHES:
	switch (obj_id) {
	  case OBJ_WBCOUNTY:
	    *elm_id = WBOX_ELM;
	    *sub_typ = 1;
	    break;
	  case OBJ_WBPARALL:
	    *elm_id = WBOX_ELM;
	    *sub_typ = 2;
	    break;
	}
	break;
      /*============ CLASS_SYMBOLS ==============*/

      case CLASS_SYMBOLS:
	switch (obj_id) {

	  /*  cloud type symbols */
	  case OBJ_CLOUD01:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 1;
	    break;
	  case OBJ_CLOUD02:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 2;
	    break;
	  case OBJ_CLOUD03:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 3;
	    break;
	  case OBJ_CLOUD04:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 4;
	    break;
	  case OBJ_CLOUD05:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 5;
	    break;
	  case OBJ_CLOUD06:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 6;
	    break;
	  case OBJ_CLOUD07:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 7;
	    break;
	  case OBJ_CLOUD08:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 8;
	    break;
	  case OBJ_CLOUD09:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 9;
	    break;
	  case OBJ_CLOUD11:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 11;
	    break;
	  case OBJ_CLOUD12:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 12;
	    break;
	  case OBJ_CLOUD13:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 13;
	    break;
	  case OBJ_CLOUD14:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 14;
	    break;
	  case OBJ_CLOUD15:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 15;
	    break;
	  case OBJ_CLOUD16:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 16;
	    break;
	  case OBJ_CLOUD17:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 17;
	    break;
	  case OBJ_CLOUD18:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 18;
	    break;
	  case OBJ_CLOUD19:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 19;
	    break;
	  case OBJ_CLOUD21:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 21;
	    break;
	  case OBJ_CLOUD22:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 22;
	    break;
	  case OBJ_CLOUD23:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 23;
	    break;
	  case OBJ_CLOUD24:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 24;
	    break;
	  case OBJ_CLOUD25:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 25;
	    break;
	  case OBJ_CLOUD26:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 26;
	    break;
	  case OBJ_CLOUD27:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 27;
	    break;
	  case OBJ_CLOUD28:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 28;
	    break;
	  case OBJ_CLOUD29:
	    *elm_id  = CTSYM_ELM;
	    *sub_typ = 29;
	    break;

	  /*  past weather symbols */
	  case OBJ_PSTWX03:
	    *elm_id  = PWSYM_ELM;
	    *sub_typ = 3;
	    break;
	  case OBJ_PSTWX04:
	    *elm_id  = PWSYM_ELM;
	    *sub_typ = 4;
	    break;
	  case OBJ_PSTWX05:
	    *elm_id  = PWSYM_ELM;
	    *sub_typ = 5;
	    break;
	  case OBJ_PSTWX06:
	    *elm_id  = PWSYM_ELM;
	    *sub_typ = 6;
	    break;
	  case OBJ_PSTWX07:
	    *elm_id  = PWSYM_ELM;
	    *sub_typ = 7;
	    break;
	  case OBJ_PSTWX08:
	    *elm_id  = PWSYM_ELM;
	    *sub_typ = 8;
	    break;
	  case OBJ_PSTWX09:
	    *elm_id  = PWSYM_ELM;
	    *sub_typ = 9;
	    break;

	  /*  pressure tendency symbols */
	  case OBJ_PTEND00:
	    *elm_id  = PTSYM_ELM;
	    *sub_typ = 999;
	    break;
	  case OBJ_PTEND01:
	    *elm_id  = PTSYM_ELM;
	    *sub_typ = 1999;
	    break;
	  case OBJ_PTEND02:
	    *elm_id  = PTSYM_ELM;
	    *sub_typ = 2999;
	    break;
	  case OBJ_PTEND03:
	    *elm_id  = PTSYM_ELM;
	    *sub_typ = 3999;
	    break;
	  case OBJ_PTEND04:
	    *elm_id  = PTSYM_ELM;
	    *sub_typ = 4999;
	    break;
	  case OBJ_PTEND05:
	    *elm_id  = PTSYM_ELM;
	    *sub_typ = 5999;
	    break;
	  case OBJ_PTEND06:
	    *elm_id  = PTSYM_ELM;
	    *sub_typ = 6999;
	    break;
	  case OBJ_PTEND07:
	    *elm_id  = PTSYM_ELM;
	    *sub_typ = 7999;
	    break;
	  case OBJ_PTEND08:
	    *elm_id  = PTSYM_ELM;
	    *sub_typ = 8999;
	    break;

	  /*  sky cover symbols */
	  case OBJ_SKY00:
	    *elm_id  = SKSYM_ELM;
	    *sub_typ = 0;
	    break;
	  case OBJ_SKY01:
	    *elm_id  = SKSYM_ELM;
	    *sub_typ = 1;
	    break;
	  case OBJ_SKY02:
	    *elm_id  = SKSYM_ELM;
	    *sub_typ = 2;
	    break;
	  case OBJ_SKY03:
	    *elm_id  = SKSYM_ELM;
	    *sub_typ = 3;
	    break;
	  case OBJ_SKY04:
	    *elm_id  = SKSYM_ELM;
	    *sub_typ = 4;
	    break;
	  case OBJ_SKY05:
	    *elm_id  = SKSYM_ELM;
	    *sub_typ = 5;
	    break;
	  case OBJ_SKY06:
	    *elm_id  = SKSYM_ELM;
	    *sub_typ = 6;
	    break;
	  case OBJ_SKY07:
	    *elm_id  = SKSYM_ELM;
	    *sub_typ = 7;
	    break;
	  case OBJ_SKY08:
	    *elm_id  = SKSYM_ELM;
	    *sub_typ = 8;
	    break;
	  case OBJ_SKY09:
	    *elm_id  = SKSYM_ELM;
	    *sub_typ = 9;
	    break;
	  case OBJ_SKY10:
	    *elm_id  = SKSYM_ELM;
	    *sub_typ = 10;
	    break;

	  /*  icing symbols */
	  case OBJ_ICE00:
	    *elm_id  = ICSYM_ELM;
	    *sub_typ = 0;
	    break;
	  case OBJ_ICE01:
	    *elm_id  = ICSYM_ELM;
	    *sub_typ = 1;
	    break;
	  case OBJ_ICE02:
	    *elm_id  = ICSYM_ELM;
	    *sub_typ = 2;
	    break;
	  case OBJ_ICE03:
	    *elm_id  = ICSYM_ELM;
	    *sub_typ = 3;
	    break;
	  case OBJ_ICE04:
	    *elm_id  = ICSYM_ELM;
	    *sub_typ = 4;
	    break;
	  case OBJ_ICE05:
	    *elm_id  = ICSYM_ELM;
	    *sub_typ = 5;
	    break;
	  case OBJ_ICE06:
	    *elm_id  = ICSYM_ELM;
	    *sub_typ = 6;
	    break;
	  case OBJ_ICE07:
	    *elm_id  = ICSYM_ELM;
	    *sub_typ = 7;
	    break;
	  case OBJ_ICE08:
	    *elm_id  = ICSYM_ELM;
	    *sub_typ = 8;
	    break;
	  case OBJ_ICE09:
	    *elm_id  = ICSYM_ELM;
	    *sub_typ = 9;
	    break;
	  case OBJ_ICE10:
	    *elm_id  = ICSYM_ELM;
	    *sub_typ = 10;
	    break;

	  /* special symbols */
	  case OBJ_SPSYM00:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 0;
	    break;
	  case OBJ_SPSYM01:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 1;
	    break;
	  case OBJ_SPSYM02:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 2;
	    break;
	  case OBJ_SPSYM03:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 3;
	    break;
	  case OBJ_SPSYM04:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 4;
	    break;
	  case OBJ_SPSYM05:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 5;
	    break;
	  case OBJ_SPSYM06:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 6;
	    break;
	  case OBJ_SPSYM07:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 7;
	    break;
	  case OBJ_SPSYM08:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 8;
	    break;
	  case OBJ_SPSYM09:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 9;
	    break;
	  case OBJ_SPSYM10:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 10;
	    break;
	  case OBJ_SPSYM11:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 11;
	    break;
	  case OBJ_SPSYM12:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 12;
	    break;
	  case OBJ_SPSYM13:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 13;
	    break;
	  case OBJ_SPSYM14:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 14;
	    break;
	  case OBJ_SPSYM15:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 15;
	    break;
	  case OBJ_SPSYM16:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 16;
	    break;
	  case OBJ_SPSYM17:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 17;
	    break;
	  case OBJ_SPSYM18:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 18;
	    break;
	  case OBJ_SPSYM19:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 19;
	    break;
	  case OBJ_SPSYM20:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 20;
	    break;
	  case OBJ_SPSYM21:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 21;
	    break;
	  case OBJ_SPSYM22:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 22;
	    break;
	  case OBJ_SPSYM23:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 23;
	    break;
	  case OBJ_SPSYM24:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 24;
	    break;
	  case OBJ_SPSYM25:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 25;
	    break;
	  case OBJ_SPSYM26:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 26;
	    break;
	  case OBJ_SPSYM27:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 27;
	    break;
	  case OBJ_SPSYM28:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 28;
	    break;
	  case OBJ_SPSYM29:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 29;
	    break;
	  case OBJ_SPSYM30:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 30;
	    break;
	  case OBJ_SPSYM31:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 31;
	    break;
	  case OBJ_SPSYM32:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 32;
	    break;
	  case OBJ_SPSYM33:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 33;
	    break;
	  case OBJ_SPSYM34:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 34;
	    break;
	  case OBJ_SPSYM35:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 35;
	    break;
	  case OBJ_SPSYM36:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 36;
	    break;
	  case OBJ_SPSYM37:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 37;
	    break;
	  case OBJ_SPSYM38:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 38;
	    break;
	  case OBJ_SPSYM39:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 39;
	    break;
	  case OBJ_SPSYM40:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 40;
	    break;
	  case OBJ_SPSYM41:
	    *elm_id  = SPSYM_ELM;
	    *sub_typ = 41;
	    break;
          case OBJ_SPSYM42:
            *elm_id  = SPSYM_ELM;
            *sub_typ = 42;
            break;
          case OBJ_SPSYM43:
            *elm_id  = SPSYM_ELM;
            *sub_typ = 43;
            break;
          case OBJ_SPSYM44:
            *elm_id  = SPSYM_ELM;
            *sub_typ = 44;
            break;
          case OBJ_SPSYM45:
            *elm_id  = SPSYM_ELM;
            *sub_typ = 45;
            break;
          case OBJ_SPSYM46:
            *elm_id  = SPSYM_ELM;
            *sub_typ = 46;
            break;
          case OBJ_SPSYM47:
            *elm_id  = SPSYM_ELM;
            *sub_typ = 47;
            break;
          case OBJ_SPSYM48:
            *elm_id  = SPSYM_ELM;
            *sub_typ = 48;
            break;
          case OBJ_SPSYM49:
            *elm_id  = SPSYM_ELM;
            *sub_typ = 49;
            break;
          case OBJ_SPSYM50:
            *elm_id  = SPSYM_ELM;
            *sub_typ = 50;
            break;
          case OBJ_SPSYM51:
            *elm_id  = SPSYM_ELM;
            *sub_typ = 51;
            break;
          case OBJ_SPSYM52:
            *elm_id  = SPSYM_ELM;
            *sub_typ = 52;
            break;
          case OBJ_SPSYM53:
            *elm_id  = SPSYM_ELM;
            *sub_typ = 53;
            break;
          case OBJ_SPSYM54:
            *elm_id  = SPSYM_ELM;
            *sub_typ = 54;
            break;
          case OBJ_SPSYM55:
            *elm_id  = SPSYM_ELM;
            *sub_typ = 55;
            break;
          case OBJ_SPSYM56:
            *elm_id  = SPSYM_ELM;
            *sub_typ = 56;
            break;

	  /* turbulence symbols */
	  case OBJ_TURB00:
	    *elm_id  = TBSYM_ELM;
	    *sub_typ = 0;
	    break;
	  case OBJ_TURB01:
	    *elm_id  = TBSYM_ELM;
	    *sub_typ = 1;
	    break;
	  case OBJ_TURB02:
	    *elm_id  = TBSYM_ELM;
	    *sub_typ = 2;
	    break;
	  case OBJ_TURB03:
	    *elm_id  = TBSYM_ELM;
	    *sub_typ = 3;
	    break;
	  case OBJ_TURB04:
	    *elm_id  = TBSYM_ELM;
	    *sub_typ = 4;
	    break;
	  case OBJ_TURB05:
	    *elm_id  = TBSYM_ELM;
	    *sub_typ = 5;
	    break;
	  case OBJ_TURB06:
	    *elm_id  = TBSYM_ELM;
	    *sub_typ = 6;
	    break;
	  case OBJ_TURB07:
	    *elm_id  = TBSYM_ELM;
	    *sub_typ = 7;
	    break;
	  case OBJ_TURB08:
	    *elm_id  = TBSYM_ELM;
	    *sub_typ = 8;
	    break;
	
	  /* combination weather symbols */

	  /* weather symbols */
 	  case  OBJ_WXSYM000:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 0;
	    break;
 	  case  OBJ_WXSYM001:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 1;
	    break;
 	  case  OBJ_WXSYM002:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 2;
	    break;
 	  case  OBJ_WXSYM003:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 3;
	    break;
 	  case  OBJ_WXSYM004:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 4;
	    break;
 	  case  OBJ_WXSYM005:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 5;
	    break;
 	  case  OBJ_WXSYM006:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 6;
	    break;
 	  case  OBJ_WXSYM007:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 7;
	    break;
 	  case  OBJ_WXSYM008:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 8;
	    break;
 	  case  OBJ_WXSYM009:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 9;
	    break;
	  case OBJ_WXSYM010:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 10;
	    break;
	  case OBJ_WXSYM011:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 11;
	    break;
	  case OBJ_WXSYM012:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 12;
	    break;
	  case OBJ_WXSYM013:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 13;
	    break;
	  case OBJ_WXSYM014:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 14;
	    break;
	  case OBJ_WXSYM015:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 15;
	    break;
	  case OBJ_WXSYM016:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 16;
	    break;
	  case OBJ_WXSYM017:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 17;
	    break;
	  case OBJ_WXSYM018:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 18;
	    break;
	  case OBJ_WXSYM019:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 19;
	    break;
	  case OBJ_WXSYM020:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 20;
	    break;
	  case OBJ_WXSYM021:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 21;
	    break;
	  case OBJ_WXSYM022:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 22;
	    break;
	  case OBJ_WXSYM023:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 23;
	    break;
	  case OBJ_WXSYM024:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 24;
	    break;
	  case OBJ_WXSYM025:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 25;
	    break;
	  case OBJ_WXSYM026:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 26;
	    break;
	  case OBJ_WXSYM027:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 27;
	    break;
	  case OBJ_WXSYM028:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 28;
	    break;
	  case OBJ_WXSYM029:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 29;
	    break;
	  case OBJ_WXSYM030:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 30;
	    break;
	  case OBJ_WXSYM031:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 31;
	    break;
	  case OBJ_WXSYM032:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 32;
	    break;
	  case OBJ_WXSYM033:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 33;
	    break;
	  case OBJ_WXSYM034:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 34;
	    break;
	  case OBJ_WXSYM035:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 35;
	    break;
	  case OBJ_WXSYM036:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 36;
	    break;
	  case OBJ_WXSYM037:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 37;
	    break;
	  case OBJ_WXSYM038:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 38;
	    break;
	  case OBJ_WXSYM039:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 39;
	    break;
	  case OBJ_WXSYM040:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 40;
	    break;
	  case OBJ_WXSYM041:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 41;
	    break;
	  case OBJ_WXSYM042:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 42;
	    break;
	  case OBJ_WXSYM043:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 43;
	    break;
	  case OBJ_WXSYM044:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 44;
	    break;
	  case OBJ_WXSYM045:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 45;
	    break;
	  case OBJ_WXSYM046:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 46;
	    break;
	  case OBJ_WXSYM047:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 47;
	    break;
	  case OBJ_WXSYM048:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 48;
	    break;
	  case OBJ_WXSYM049:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 49;
	    break;
	  case OBJ_WXSYM050:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 50;
	    break;
	  case OBJ_WXSYM051:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 51;
	    break;
	  case OBJ_WXSYM052:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 52;
	    break;
	  case OBJ_WXSYM053:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 53;
	    break;
	  case OBJ_WXSYM054:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 54;
	    break;
	  case OBJ_WXSYM055:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 55;
	    break;
	  case OBJ_WXSYM056:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 56;
	    break;
	  case OBJ_WXSYM057:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 57;
	    break;
	  case OBJ_WXSYM058:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 58;
	    break;
	  case OBJ_WXSYM059:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 59;
	    break;
	  case OBJ_WXSYM060:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 60;
	    break;
	  case OBJ_WXSYM061:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 61;
	    break;
	  case OBJ_WXSYM062:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 62;
	    break;
	  case OBJ_WXSYM063:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 63;
	    break;
	  case OBJ_WXSYM064:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 64;
	    break;
	  case OBJ_WXSYM065:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 65;
	    break;
	  case OBJ_WXSYM066:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 66;
	    break;	 
	  case OBJ_WXSYM067:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 67;
	    break;
	  case OBJ_WXSYM068:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 68;
	    break;
	  case OBJ_WXSYM069:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 69;
	    break;
	  case OBJ_WXSYM070:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 70;
	    break;
	  case OBJ_WXSYM071:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 71;
	    break;
	  case OBJ_WXSYM072:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 72;
	    break;
	  case OBJ_WXSYM073:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 73;
	    break;
	  case OBJ_WXSYM074:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 74;
	    break;
	  case OBJ_WXSYM075:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 75;
	    break;
	  case OBJ_WXSYM076:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 76;
	    break;	 
	  case OBJ_WXSYM077:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 77;
	    break;
	  case OBJ_WXSYM078:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 78;
	    break;
	  case OBJ_WXSYM079:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 79;
	    break;
	  case OBJ_WXSYM080:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 80;
	    break;
	  case OBJ_WXSYM081:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 81;
	    break;
	  case OBJ_WXSYM082:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 82;
	    break;
	  case OBJ_WXSYM083:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 83;
	    break;
	  case OBJ_WXSYM084:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 84;
	    break;
	  case  OBJ_WXSYM085:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 85;
	    break;
	  case OBJ_WXSYM086:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 86;
	    break;	 
	  case OBJ_WXSYM087:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 87;
	    break;
	  case OBJ_WXSYM088:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 88;
	    break;
	  case OBJ_WXSYM089:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 89;
	    break;
	  case OBJ_WXSYM090:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 90;
	    break;
	  case OBJ_WXSYM091:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 91;
	    break;
	  case OBJ_WXSYM092:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 92;
	    break;
	  case OBJ_WXSYM093:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 93;
	    break;
	  case OBJ_WXSYM094:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 94;
	    break;
	  case  OBJ_WXSYM095:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 95;
	    break;
	  case OBJ_WXSYM096:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 96;
	    break;	 
	  case OBJ_WXSYM097:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 97;
	    break;
	  case OBJ_WXSYM098:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 98;
	    break;
	  case OBJ_WXSYM099:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 99;
	    break;
	  case OBJ_WXSYM103:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 103;
	    break;
	  case OBJ_WXSYM104:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 104;
	    break;
	  case OBJ_WXSYM105:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 105;
	    break;
	  case OBJ_WXSYM107:
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 107;
	    break;
	  case OBJ_WXSYM201:	
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 201;
	    break;
	  case OBJ_WXSYM202:	
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 202;
	    break;
	  case OBJ_WXSYM203:	
	    *elm_id  = WXSYM_ELM;
	    *sub_typ = 203;
	    break;
	}
	break;	    

      /*=========== CLASS_WINDS ==============*/	

      case CLASS_WINDS:
	switch (obj_id) {
	  case OBJ_WINDARRW:
	    *elm_id = ARROW_ELM;
	    break;
	  case OBJ_WINDBARB:
	    *elm_id = BARB_ELM;
	    break;
	  case OBJ_WINDDARR:
	    *elm_id = DARR_ELM;
	    break;
	  case OBJ_WINDHASH:
	    *elm_id = HASH_ELM;
	    break;
	}
	break;

      /*=========== CLASS_FRONTS ==============*/

      case CLASS_FRONTS:

	*elm_id = FRONT_ELM;

	switch (obj_id) {
	  case OBJ_COLDFNT:
	    *gem_typ = 400;
	    *sub_typ = 400;
	    break;
	  case OBJ_WKCOLDFNT:
	    *gem_typ = 400;
	    *sub_typ = 405;
	    break;
	  case OBJ_DIFCOLDFNT:
	    *gem_typ = 400;
	    *sub_typ = 408;
	    break;
	  case OBJ_WARMFNT:
	    *gem_typ = 200;
	    *sub_typ = 200;
	    break;
	  case OBJ_WKWARMFNT:
	    *gem_typ = 200;
	    *sub_typ = 205;
	    break;
	  case OBJ_DIFWARMFNT:
	    *gem_typ = 200;
	    *sub_typ = 208;
	    break;
	  case OBJ_STATFNT:
	    *gem_typ = 0;
	    *sub_typ = 0;
	    break;
	  case OBJ_WKSTATFNT:
	    *gem_typ = 0;
	    *sub_typ = 5;
	    break;
	  case OBJ_DIFSTATFNT:
	    *gem_typ = 0;
	    *sub_typ = 8;
	    break;
	  case OBJ_OCCLFNT:
	    *gem_typ = 600;
	    *sub_typ = 600;
	    break;
	  case OBJ_WKOCCLFNT:
	    *gem_typ = 600;
	    *sub_typ = 605;
	    break;
	  case OBJ_DIFOCCLFNT:
	    *gem_typ = 600;
	    *sub_typ = 608;
	    break;
	  case OBJ_DRYFNT:
	    *gem_typ = 700;
	    *sub_typ = 700;
	    break;
	  case OBJ_TROFFNT:
	    *gem_typ = 800;
	    *sub_typ = 800;
	    break;
	  case OBJ_TRPTFNT:
	    *gem_typ = 800;
	    *sub_typ = 809;
	    break;
	  case OBJ_SQUALL:
	    *gem_typ = 900;
	    *sub_typ = 900;
	    break;
	}	
	break;

      /*============ CLASS_LINES ===============*/

      case CLASS_LINES:
	*elm_id  = SPLN_ELM;
	switch (obj_id) {

	  case OBJ_CNTR:
	    *elm_id  = LINE_ELM;
	    *gem_typ = 1;
	    break;
	  case OBJ_DSHLN2:
	    *elm_id  = LINE_ELM;
	    *gem_typ = 2;
	    break;
	  case OBJ_DSHLN3:
	    *elm_id  = LINE_ELM;
	    *gem_typ = 3;
	    break;
	  case OBJ_DSHLN4:
	    *elm_id  = LINE_ELM;
	    *gem_typ = 4;
	    break;
	  case OBJ_DSHLINE:
	  case OBJ_DSHLN5:
	    *elm_id  = LINE_ELM;
	    *gem_typ = 5;
	    break;
	  case OBJ_DSHLN6:
	    *elm_id  = LINE_ELM;
	    *gem_typ = 6;
	    break;
	  case OBJ_DSHLN7:
	    *elm_id  = LINE_ELM;
	    *gem_typ = 7;
	    break;
	  case OBJ_DSHLN8:
	    *elm_id  = LINE_ELM;
	    *gem_typ = 8;
	    break;
	  case OBJ_DSHLN9:
	    *elm_id  = LINE_ELM;
	    *gem_typ = 9;
	    break;
	  case OBJ_DSHLN10:
	    *elm_id  = LINE_ELM;
	    *gem_typ = 10;
	    break;
	  case OBJ_SPLN1:
	    *gem_typ = 1;
	    break;
	  case OBJ_SPLN2:
	    *gem_typ = 2;
	    break;
	  case OBJ_SPLN3:
	    *gem_typ = 3;
	    break;
	  case OBJ_SPLN4:
	    *gem_typ = 4;
	    break;
	  case OBJ_SPLN5:
	    *gem_typ = 5;
	    break;
	  case OBJ_SPLN6:
	    *gem_typ = 6;
	    break;
	  case OBJ_SPLN7:
	    *gem_typ = 7;
	    break;
	  case OBJ_SPLN8:
	    *gem_typ = 8;
	    break;
	  case OBJ_SPLN9:
	    *gem_typ = 9;
	    break;
	  case OBJ_SPLN10:
	    *gem_typ = 10;
	    break;
	  case OBJ_SPLN11:
	    *gem_typ = 11;
	    break;
	  case OBJ_SPLN12:
	    *gem_typ = 12;
	    break;
	  case OBJ_SPLN13:
	    *gem_typ = 13;
	    break;
	  case OBJ_SPLN14:
	    *gem_typ = 14;
	    break;
	  case OBJ_SPLN15:
	    *gem_typ = 15;
	    break;
	  case OBJ_SPLN16:
	    *gem_typ = 16;
	    break;
	  case OBJ_SPLN17:
	    *gem_typ = 17;
	    break;
	  case OBJ_SPLN18:
	    *gem_typ = 18;
	    break;
	  case OBJ_SPLN19:
	    *gem_typ = 19;
	    break;
	  case OBJ_SPLN20:
	    *gem_typ = 20;
	    break;
	  case OBJ_SPLN21:
	    *gem_typ = 21;
	    break;
	  case OBJ_SPLN22:
	    *gem_typ = 22;
	    break;
	  case OBJ_SPLN23:
	    *gem_typ = 23;
	    break;
	  case OBJ_KINKLN1:
	    *gem_typ = 24;
	    break;
	  case OBJ_KINKLN2:
	    *gem_typ = 25;
	    break;
	  case OBJ_SPLN26:
	    *gem_typ = 26;
	    break;
	}
	break;

      /*============ CLASS_TEXT ===============*/

      case CLASS_TEXT:
	*elm_id  = SPTX_ELM;
	switch (obj_id) {
	  case OBJ_TEXTGEN:
	    *sub_typ = 0;
	    break;
	  case OBJ_TEXTFZL:
	    *sub_typ = 6;
	    break;
	  case OBJ_TEXTTURB:
	    *sub_typ = 7;
	    break;
	  case OBJ_TEXTCLD:
	    *sub_typ = 8;
	    break;
	  case OBJ_TEXTICNG:
            *sub_typ = 12;
            break;
	  case OBJ_TEXTMCLOUD:
            *sub_typ = 15;
            break;
	  default:
	    *sub_typ = -99;
	    break;
	}
	break;


      /*============ CLASS_COMSYM ===============*/

      case CLASS_COMSYM:
	*elm_id  = CMBSY_ELM;
	*gem_typ = 0;

	switch (obj_id) {
	  case OBJ_CSYMB01:
	    *sub_typ = 1;
	    break;

	  case OBJ_CSYMB02:
	    *sub_typ = 2;
	    break;

	  case OBJ_CSYMB03:
	    *sub_typ = 3;
	    break;

	  case OBJ_CSYMB04:
	    *sub_typ = 4;
	    break;

	  case OBJ_CSYMB05:
	    *sub_typ = 5;
	    break;

	  case OBJ_CSYMB06:
	    *sub_typ = 6;
	    break;

	  case OBJ_CSYMB07:
	    *sub_typ = 7;
	    break;

	  case OBJ_CSYMB08:
	    *sub_typ = 8;
	    break;

	  case OBJ_CSYMB09:
	    *sub_typ = 9;
	    break;

	  case OBJ_CSYMB10:
	    *sub_typ = 10;
	    break;

	  case OBJ_CSYMB11:
	    *sub_typ = 11;
	    break;

	  case OBJ_CSYMB12:
	    *sub_typ = 12;
	    break;

	  case OBJ_CSYMB13:
	    *sub_typ = 13;
	    break;

	  case OBJ_CSYMB14:
	    *sub_typ = 14;
	    break;

	  case OBJ_CSYMB15:
	    *sub_typ = 15;
	    break;

	  case OBJ_CSYMB16:
	    *sub_typ = 16;
	    break;

	  case OBJ_CSYMB17:
	    *sub_typ = 17;
	    break;

	  case OBJ_CSYMB18:
	    *sub_typ = 18;
	    break;

	  case OBJ_CSYMB19:
	    *sub_typ = 19;
	    break;

	  case OBJ_CSYMB20:
	    *sub_typ = 20;
	    break;

	  case OBJ_CSYMB21:
	    *sub_typ = 21;
	    break;

	  case OBJ_CSYMB22:
	    *sub_typ = 22;
	    break;

	  case OBJ_CSYMB23:
	    *sub_typ = 23;
	    break;

	  case OBJ_CSYMB24:
	    *sub_typ = 24;
	    break;

	  case OBJ_CSYMB25:
	    *sub_typ = 25;
	    break;

	  case OBJ_CSYMB26:
	    *sub_typ = 26;
	    break;

	  case OBJ_CSYMB27:
	    *sub_typ = 27;
	    break;

          case OBJ_CSYMB28:
            *sub_typ = 28;
            break;

	  default:
	    *sub_typ = -99;
	    break;
	}
	break;

      /*============ CLASS_MARKER ===============*/

      case CLASS_MARKER:
        *elm_id  = MARK_ELM;
        *gem_typ = 0;

        switch (obj_id) {
          case OBJ_MARK1:  
            *sub_typ = 1;
            break;

          case OBJ_MARK2:  
            *sub_typ = 2;
            break;

          case OBJ_MARK3:    
            *sub_typ = 3;
            break;

          case OBJ_MARK4:    
            *sub_typ = 4;
            break;

          case OBJ_MARK5:    
            *sub_typ = 5;
            break;

          case OBJ_MARK6:    
            *sub_typ = 6;
            break;

          case OBJ_MARK7:    
            *sub_typ = 7;
            break;

          case OBJ_MARK8:    
            *sub_typ = 8;
            break;

          case OBJ_MARK9:    
            *sub_typ = 9;
            break;

          case OBJ_MARK10:    
            *sub_typ = 10;
            break;

          case OBJ_MARK11:
            *sub_typ = 11;
            break;

          case OBJ_MARK12:
            *sub_typ = 12;
            break;

          case OBJ_MARK13:    
            *sub_typ = 13;
            break;

          case OBJ_MARK14:    
            *sub_typ = 14;
            break;

          case OBJ_MARK15:    
            *sub_typ = 15;
            break;

          case OBJ_MARK16:    
            *sub_typ = 16;
            break;

          case OBJ_MARK17:    
            *sub_typ = 17;
            break;

          case OBJ_MARK18:    
            *sub_typ = 18;
            break;

          case OBJ_MARK19:    
            *sub_typ = 19;
            break;

          case OBJ_MARK20:   
            *sub_typ = 20;
            break;

          case OBJ_MARK21:   
            *sub_typ = 21;
            break;

          case OBJ_MARK22:   
            *sub_typ = 22;
            break;

          default:
            *elm_id = -99;
            break;
        }
        break;

      /*============ CLASS_TRACKS ===============*/ 

      case CLASS_TRACKS:
	*elm_id = TRKSTORM_ELM;
	break;

      /*============ CLASS_SIGMETS ===============*/ 

      case CLASS_SIGMETS:
	switch (obj_id) {
	  case OBJ_SIGINTL:
	    *elm_id = SIGINTL_ELM;
	    break;

	  case OBJ_SIGNCON:
	    *elm_id = SIGNCON_ELM;
	    break;

	  case OBJ_SIGCONV:
	    *elm_id = SIGCONV_ELM;
	    break;

	  case OBJ_SIGOUTL:
	    *elm_id = SIGOUTL_ELM;
	    break;

	  case OBJ_SIGAIRM:
	    *elm_id = SIGAIRM_ELM;
	    break;

	  case OBJ_SIGCCF:
	    *elm_id = SIGCCF_ELM;
	    break;

	  case OBJ_SIGVOL:
	    *elm_id = VOLC_ELM;
	    break;

	  case OBJ_SIGVAC:
	    *elm_id = ASHCLD_ELM;
	    break;

	  default:
	    *elm_id = -99;
	    break;
	}
	break;

	/*============ CLASS_CIRCLE ===============*/

	case CLASS_CIRCLE:
	    *elm_id = CIRCLE_ELM;
	  break;

      /*============ CLASS_LIST ===============*/

      case CLASS_LIST:
        *elm_id  = LIST_ELM;
        *gem_typ = 0;

        switch ( obj_id ) {
          case OBJ_LISTCOUNTY:  
            *sub_typ = 1;
            break;

          case OBJ_LISTZONE:  
            *sub_typ = 2;
            break;

          case OBJ_LISTWFO:    
            *sub_typ = 3;
            break;

          case OBJ_LISTSTATE:    
            *sub_typ = 4;
            break;
	  
          case OBJ_LISTWBCMZ:    
            *sub_typ = 5;
            break;
	  
	  default:
	    *elm_id = -99;
	    break;
        }
	break;
	
	/*============ CLASS_MET ===============*/

	case CLASS_MET:

          switch ( obj_id ) {
            
	    case OBJ_JET:  
	      *elm_id = JET_ELM;
              break;

	    case OBJ_GFA:  
	      *elm_id = GFA_ELM;
              break;

	    case OBJ_GFA_P:  
	      *elm_id = GFA_ELM;
              break;

	    default:
	      *elm_id = -99;
	      break;
          }
	  
	  break;

      default:
	break;
    } 
}

/*=====================================================================*/
