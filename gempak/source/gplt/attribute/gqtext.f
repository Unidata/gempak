	SUBROUTINE GQTEXT ( itxfn, itxhw, sztext, itxwid,
     +			    ibrdr, irrotn, ijust, iret )
C************************************************************************
C* GQTEXT 								*
C* 									*
C* This subroutine returns the text attributes including the font 	*
C* number, the text software/hardware flag, the text size and width,	*
C* the text border and blank fill flag, the text relative rotation	*
C* flag, and the text justification.					*
C*									*
C* The border and blank fill flag is a three digit number, ABC, with	*
C* the following definitions:						*
C*									*
C*	A	Border		1 = no,  2 = yes			*
C*	B	Blank fill	1 = no,  2 = yes			*
C*	C	Border type	1 = box, 2 = low, 3= high		*
C*									*
C* Low and high border types refer to the low and high outlines for use	*
C* on aviation forecast products.					*
C*									*
C* GQTEXT  ( ITXFN, ITXHW, SZTEXT, ITXWID, IBRDR, IRROTN, IJUST, IRET )	*
C*									*
C* Output parameters:							*
C*	ITXFN		INTEGER		Font number			*
C*	ITXHW		INTEGER		Software/hardware flag		*
C*					  1 = software 			*
C*					  2 = hardware 			*
C*	SZTEXT		REAL		Text size multiplier		*
C*	ITXWID		INTEGER		Text line width 		*
C*	IBRDR		INTEGER		Text border/blank fill flag	*
C*	IRROTN		INTEGER		Text north-relative rot flag	*
C*					  1 = screen			*
C*					  2 = north			*
C*	IJUST		INTEGER		Text justification		*
C*					  1 = left			*
C*					  2 = center			*
C*					  3 = right			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* S. Schotz/GSC	 1/90	Added text width			*
C* S. Jacobs/NCEP	 9/97	Added border, rotation and just flags	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
C* 	If device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    itxfn  = 0
	    itxhw  = 0
	    sztext = 0	    	    
	    itxwid = 0
	    ibrdr  = 0
	    irrotn = 0
	    ijust  = 0
	    iret   = NDVICE
	  ELSE
C	
C*	    Retrieve values from /DEVSET/.
C
	    itxfn  = ltxfn
	    itxhw  = ltxhw
	    sztext = stxsz
	    itxwid = ltxwid
	    ibrdr  = lbrdr
	    irrotn = lrrotn
	    ijust  = ljust
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
