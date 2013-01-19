#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"

static float   _dcX[MAXPTS], _dcY[MAXPTS];	/* device coordinates */
static int     _dcN;				/* number of vertices */

static int	_elOffset;			/* offset in vgf file */
static int	_nearPt;			/* nearest vert to pt */
static int	_maxAllowPts = (MAXPTS - 1);


/************************************************************************
 * nmap_pgactv.c							*
 *                                                                      *
 * This module maintains the "last selected" element in the VGF file.   *
 * Note that it tracks only the last selected element and not all       *
 * all selected elments.  The information that is pertinent is the 	*
 * index to the element in the VGF file, and the vertex that		*
 * was closest to the mouse at the point of selection (_nearPt), and	*
 * the device coordinates of the points of the selected element.        *
 *									*
 * This file is the only place that the device coordinates should be    *
 * set or modified.  DO NOT manipulate these coordinate arrays          *
 * directly!								*
 *                                                                      *
 * CONTENTS:                                                            *
 * pgactv_setElmLoc()	set the vgf file location of the element	*
 * pgactv_setNearPt()   determine the closest vertex of the elem to a pt*
 * pgactv_addClosedPt() add one more pt for handling closed lines  	*
 * pgactv_addPts()	add one or more points to element		*
 * pgactv_modPt()       modify one vertex (during select/drag)		*
 * pgactv_clearActv()   reset active records                      	*
 * pgactv_deletePt()	removes one vertex                      	*
 * pgactv_deleteAll()	removes all vertices                      	*
 * 									*
 * pgactv_getElmLoc()   return the vgf file location of the element	*
 * pgactv_getNearPt()   return the nearest vertex of the element	*
 * pgactv_getDevPts()   return the number of and ptrs to the dev coords *
 ***********************************************************************/

/*=====================================================================*/

void pgactv_setActvElm ( VG_DBStruct *el, int el_offset )
/************************************************************************
 * pgactv_setActvElm							*
 *                                                                      *
 * This fucntion stores the offset to the selected element and sets the *
 * device coordinate arrays.						*
 *									*
 * void	pgactv_setActvElm ( el, el_offset )				*
 *									*
 * Input parameters:                                                    *
 *	*el		VG_DBStruct	selected element		*
 *	el_offset	int		Offset to selected element	*
 *                                                                      *
 * Output parameters:                                                   *
 *	None								*
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi	 9/97	Created.				*
 * C. Lin/EAi		10/97	rename from drw_hotset, clean up	*
 * E. Safford/GSC	 5/98	rename from pghot_setElmLoc		*
 * S. Law/GSC		09/99	added _maxAllowPts			*
 * J. Wu/SAIC		11/02	set _maxAllowPts for CLASS_LIST		*
 ***********************************************************************/
{
  int	iret;
/*---------------------------------------------------------------------*/

  _elOffset = el_offset;
  cvg_todev (el, &_dcN, _dcX, _dcY, &iret);

  _maxAllowPts = (pgpalw_getCurClassId () == CLASS_SIGMETS) ?
    (MAX_SIGMET - 1) : (MAXPTS - 1);
  
  if ( pgpalw_getCurClassId () == CLASS_LIST ) {
      _maxAllowPts = (MAXLISTITEMS - 1);
  }

}

/*=====================================================================*/

void pgactv_setNearPt ( int nearest_pt )
/************************************************************************
 * pgactv_setNearPt							*
 *                                                                      *
 * This fucntion stores the index to the nearest vertex of the the elem *
 *									*
 * void	pgactv_setNearPt(nearest_pt)					*
 *									*
 * Input parameters:                                                    *
 *	nearest_pt		int	Offset to selected element	*
 *                                                                      *
 * Output parameters:                                                   *
 *	None								*
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         9/97    Created.                               *
 * C. Lin/EAi           10/97    rename from drw_hotsetnear, clean up   *
 * E. Safford/GSC	 5/98	 moved from nmap_pghot to nmap_pgactv   *
 ***********************************************************************/
{
	_nearPt = nearest_pt;
}

/*=====================================================================*/

void pgactv_addClosedPt ( void )
/************************************************************************
 * pgactv_addClosedPt                                                   *
 *                                                                      *
 * This function adds the first point to the end of the coordinate      *
 * array.  This is necessary for processing closed figures.             *
 *                                                                      *
 * void  pgactv_addClosedPt ()                                          *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *      None                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC        5/98    initial coding                         *
 ***********************************************************************/
{
    _dcX[_dcN] = _dcX[0];
    _dcY[_dcN] = _dcY[0];
    _dcN++;
}

/*=====================================================================*/

void pgactv_addPts ( float add_x[], float add_y[], int add_n, 
						int index, int *iret )
/************************************************************************
 * pgactv_addPts							*
 *									*
 * This function adds one or more points to the list of device		*
 * coordinates.								*
 *									*
 * void  pgactv_addPts (add_x, add_y, add_n, index, iret)		*
 *									*
 * Input parameters:							*
 *	add_x[]		float	array of X coordinates to be added	*
 *	add_y[]		float	array of Y coordinates to be added	*
 *	add_n		int	number of points to be added		*
 *	index		int	index of insert location		*
 *									*
 * Output parameters:							*
 *	*iret		int	 0 = sucessful				*
 *				-1 = addtion exceeds _maxAllowPts	*
 *				-2 = index out of bounds		*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		09/98	initial coding				*
 * S. Law/GSC		09/98	added second flip to retain direction	*
 * S. Law/GSC		02/99	added ability to add mid-aray		*
 * S. Law/GSC		09/99	MAXPTS -> _maxAllowPts			*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    if ((_dcN + add_n) > _maxAllowPts) {
	*iret = -1;
    }
    else if (0 > index || index > _dcN) {
	*iret = -2;
    }
    else {
	*iret =  0;

	/*
	 * transpose coordinate array, so that you can tack on back end
	 */
	if (index == 0) {
	    pgutls_fReverseArray (_dcN, _dcX, _dcY);

	    for (ii = 0; ii < add_n; ii++) {
		_dcX[_dcN] = add_x[ii];
		_dcY[_dcN] = add_y[ii];
		_dcN++;
	    }

	    /*
	     * transpose coordinate array back to retain direction
	     */
	    pgutls_fReverseArray (_dcN, _dcX, _dcY);
	}
	else if (index == _dcN) {
	    for (ii = 0; ii < add_n; ii++) {
		_dcX[_dcN] = add_x[ii];
		_dcY[_dcN] = add_y[ii];
		_dcN++;
	    }
	}
	else {
	    for (ii = (_dcN - 1); ii >= index; ii--) {
		_dcX[ii + add_n] = _dcX[ii];
		_dcY[ii + add_n] = _dcY[ii];
	    }

	    for (ii = 0; ii < add_n; ii++) {
		_dcX[ii + index] = add_x[ii];
		_dcY[ii + index] = add_y[ii];
		_dcN++;
	    }
	}
    }
}


/*=====================================================================*/

void pgactv_modPt ( int pt, float new_x, float new_y )
/************************************************************************
 * pgactv_modPt	                                                        *
 *                                                                      *
 * This function changes the value of one device coordinate pair.  It   *
 * is used for dragging a single vertex of an element.                  *
 *                                                                      *
 * void pgactv_modPt ( pt, new_x, new_y)                                *
 *                                                                      *
 * Input parameters:                                                    *
 *  pt		int		index to vertex				*
 *  new_x  	float		new x coordinate (device coordinate)	*
 *  new_y  	float		new y coordinate (device coordinate)	*
 *                                                                      *
 * Output parameters:                                                   *
 *      None                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC        5/98    initial coding                         *
 ***********************************************************************/
{
    _dcX[pt] = new_x;
    _dcY[pt] = new_y;
}

/*=====================================================================*/

void pgactv_clearActv ( void )
/************************************************************************
 * pgactv_clearActv                                                     *
 *                                                                      *
 * This function clears the active element settings.                    *
 *                                                                      *
 * void  pgactv_clearActv (  )                                          *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *      None                                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC        5/98    initial coding                         *
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/

    _elOffset = -1;

    for (ii = 0; ii < _dcN; ii++) {
	_dcX[ii] = 0.0F;
	_dcY[ii] = 0.0F;
    }

    _dcN = 0; 
}


/*=====================================================================*/

void pgactv_deletePt ( int delpt )
/************************************************************************
 * pgactv_deletePt							*
 *									*
 * This function deletes a point from the list of device coordinates.	*
 *									*
 * void  pgactv_deletePt (delpt)					*
 *									*
 * Input parameters:							*
 *	delpt		int	point to be deleted			*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		09/98	initial coding				*
 * S. Law/GSC		09/99	made more robust			*
 ***********************************************************************/
{
    int		ii;
/*---------------------------------------------------------------------*/

    if (_dcN < 2) {
	_dcN = 0;
    }
    else {
	if (delpt != (_dcN - 1)) {
	    for (ii = delpt + 1; ii < _dcN; ii++) {
		_dcX[ii - 1] = _dcX[ii];
		_dcY[ii - 1] = _dcY[ii];
	    }
	}

	_dcN--;
    }
}


/*=====================================================================*/

void pgactv_deleteAll ( void )
/************************************************************************
 * pgactv_deleteAll							*
 *									*
 * This function deletes all points from the list of device coordinates.*
 *									*
 * void  pgactv_deleteAll ()						*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		09/99	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _dcN = 0;
}


/*=====================================================================*/

int pgactv_getElmLoc ( void )
/************************************************************************
 * pgactv_getElmLoc							*
 *                                                                      *
 * This function returns the offset in bytes to the currently		*
 *  selected element in the VGF file.					*
 *                                                                      *
 * int	pgactv_getElmLoc()						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *	pgactv_getElmLoc	int	element offset			*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         9/97    Created.                               *
 * C. Lin/EAi           10/97    rename from drw_hotget, clean up       *
 * E. Safford/GSC	 5/98	 moved from nmap_pghot to nmap_pgactv   *
 ***********************************************************************/
{
    return _elOffset;
}

/*=====================================================================*/

int pgactv_getNearPt ( void )
/************************************************************************
 * pgactv_getNearPt							*
 *                                                                      *
 * This function returns the index to the nearest vertex of the elem.   *
 *                                                                      *
 * int	pgactv_getNearPt()						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *	pgactv_getNearPt	int	element index			*
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         9/97    Created.                               *
 * C. Lin/EAi           10/97    rename from drw_hotgetnear, clean up   *
 * E. Safford/GSC	 5/98	 moved from nmap_pghot to nmap_pgactv   *
 ***********************************************************************/
{
    return _nearPt;
}

/*=====================================================================*/

void pgactv_getDevPts ( int *np, float **dx, float **dy )
/************************************************************************
 * pgactv_getDevPts                                                     *
 *                                                                      *
 * This function returns the number of vertices and pointers to the     *
 * device coordinates for the device coordinate arrays.  It is vital    *
 * these arrays are not manipulated by any procedures outside of this   *
 * file.								*
 *                                                                      *
 * void  pgactv_getDevPts( np, dx, dy )                                 *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *	*np		int	Number of coordinates			*
 * 	**dx		float	X coordinates				*
 * 	**dy		float	Y coordinates				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/EAi         9/97    Created.                               *
 * C. Lin/EAi           10/97    rename from drw_hotgetnear, clean up   *
 * E. Safford/GSC        5/98    moved from nmap_pghot to nmap_pgactv   *
 ***********************************************************************/
{
    *np  = _dcN;
    *dx  = _dcX;
    *dy  = _dcY;
}

/*=====================================================================*/
