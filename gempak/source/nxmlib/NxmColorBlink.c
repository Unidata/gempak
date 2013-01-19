#include "geminc.h"
#include "gemprm.h"
#include "xwcmn.h"
#include "Nxm.h"
#include "NxmInit.h"
#include "NxmColorEdit.h"


#define BLINK_OFF 0
#define BLINK_ON  1

int	LOCK = 0;
int	blink[256];

typedef struct  {
	XColor	on_color;
	XColor  off_color;
	int	color_index;
	XtIntervalId timer_id;
} _blinkStr;
	
_blinkStr _blinkData;

/*
 *  Private functions
 */
XtTimerCallbackProc NxmBlinkTimeout ( XtIntervalId  *id );

/************************************************************************
 * NxmColorBlink.c							*
 *									*
 * CONTENTS:								*
 ***********************************************************************/

void NxmColorBlinkSet ( int color_index, int type )
/************************************************************************
 * NxmColorBlinkSet							*
 *									*
 * NxmColorBlinkSet ( color_index, type )				*
 *									*
 * Input parameters:							*
 *	color_index	int						*
 *	type		int						*
 **									*
 * T. Piper/SAIC	07/04	Replaced ratio with COLR_SCAL		*
 ***********************************************************************/
{
XColor xcolor[2];
int	icolr, iret, red, green, blue;
/*---------------------------------------------------------------------*/

	switch(type) {

	    case 0:
		if ( LOCK ) return;
		LOCK = 1;

		blink[color_index] = BLINK_ON;

		xcolor[0].pixel = NXMcolrEditPixels[color_index];
		xcolor[0].flags = DoRed | DoBlue | DoGreen;
		xcolor[1].pixel = NXMcolrEditPixels[0];
		xcolor[1].flags = DoRed | DoBlue | DoGreen;
		XQueryColors(gemdisplay, gemmap, xcolor, 2 );

		_blinkData.on_color = xcolor[0];
		_blinkData.off_color = xcolor[1];
		(_blinkData.off_color).pixel = (_blinkData.on_color).pixel;
		_blinkData.color_index = color_index;

      		_blinkData.timer_id = XtAppAddTimeOut(NXMapp, 1L, 
		(XtTimerCallbackProc)NxmBlinkTimeout, (XtPointer)NULL );
		break;

	    case 1:
		if ( _blinkData.timer_id != (XtIntervalId)0) 
		XtRemoveTimeOut( _blinkData.timer_id );

		_blinkData.timer_id = 0;
		
		icolr = _blinkData.color_index;
		red   = _blinkData.on_color.red/COLR_SCAL;
		green = _blinkData.on_color.green/COLR_SCAL;
		blue  = _blinkData.on_color.blue/COLR_SCAL;
		gscrgb( &icolr, &red, &green, &blue, &iret );

		LOCK = 0;
		break;
	}
	
}

/*=====================================================================*/
/* ARGSUSED */
XtTimerCallbackProc NxmBlinkTimeout ( XtIntervalId *id ) 
/************************************************************************
 * NxmBlinkTimeout                                            		*
 *                                                                      *
 * This function is the color blinking routine.				*
 *                                                                      *
 * XtTimerCallbackProc NxmBlinkTimeout(id) 				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*id		XtIntervalId	blink interval			*
 *									*
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *	NxmBlinkTimeout XtTimerCallbackProc                             *
 **                                                                     *
 * Log:                                                                 *
 * ?/?		 	??/??	initial conding				*
 * E. Safford/GSC  	04/99	fix irix6 compiler warning add header   *
 * T. Piper/SAIC	07/04	Replaced ratio with COLR_SCAL 		*
 ***********************************************************************/
{
int		icolr, iret, red, green, blue;
unsigned long   wait;
/*---------------------------------------------------------------------*/

	icolr = _blinkData.color_index;

	wait = 100;

	if ( blink[icolr] == BLINK_ON) {
		
		red   = _blinkData.on_color.red/COLR_SCAL;
		green = _blinkData.on_color.green/COLR_SCAL;
		blue  = _blinkData.on_color.blue/COLR_SCAL;
		gscrgb( &icolr, &red, &green, &blue, &iret );

		blink[icolr] = BLINK_OFF;
	}
	else {
		red   = _blinkData.off_color.red/COLR_SCAL;
		green = _blinkData.off_color.green/COLR_SCAL;
		blue  = _blinkData.off_color.blue/COLR_SCAL;
		gscrgb( &icolr, &red, &green, &blue, &iret );
		blink[icolr] = BLINK_ON;
	}

      	_blinkData.timer_id = XtAppAddTimeOut(NXMapp, wait, 
		(XtTimerCallbackProc)NxmBlinkTimeout, (XtPointer)NULL);

	return (XtTimerCallbackProc)NULL;
}
