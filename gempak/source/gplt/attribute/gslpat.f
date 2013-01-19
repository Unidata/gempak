	SUBROUTINE GSLPAT  ( ilpat, iret )
C************************************************************************
C* GSLPAT								*
C* 									*
C* This subroutine sets the current line pattern to the given 		*
C* pattern.  The current line type number will be set to zero.  	*
C* The values specified in the line pattern designate the length 	*
C* of alternating on and off segments.  For example, a long-dash 	*
C* short-dash line pattern could be specified by LPAT values of:	*
C*									*
C*		10 5 5 5 0 0 0 0					*
C*									*
C* This line pattern will display a line for 10 units, space for 5 	*
C* units, a line for 5 units, and space for 5 units and repeat this	*
C* pattern.  Dots are specified using negative numbers.  The		*
C* absolute value of the number corresponds to the space in which	*
C* the dot will be centered.						*
C*									*
C* GSLPAT  ( ILPAT, IRET )						*
C*									*
C* Input parameters:							*
C*	ILPAT (8)	INTEGER		Line pattern values		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* S. Schotz/GSC	 2/90	Flush line before setting		*
C* S. Schotz/GSC	 8/90	Add lltyp to DSLPAT parameter list	*
C* M. Linda/GSC		 6/96	Removed FLLINE				*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'DEVSET.CMN'
	INCLUDE 	'DEVCHR.CMN'
C*
	INTEGER 	 ilpat (*)
C------------------------------------------------------------------------
C* 	If device has not been set, return an error.
C
	IF  ( ddev .eq. ' ' )  THEN
	    iret = NDVICE
	  ELSE
C
C*	    Send values to device.
C
	    CALL DSLPAT  ( ilpat, lltyp, iret )
	END IF
C*
	RETURN
	END
