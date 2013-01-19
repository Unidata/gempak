	SUBROUTINE LC_UARE ( area, newfil, iflno, arecur, stn, iret )
C************************************************************************
C* LC_UARE								*
C*									*
C* This subroutine updates and processes the user input for AREA.	*
C* It calls LC_SARE only if the area name has changed or a new file	*
C* has been opened.  This subroutine is useful if AREA is to be		*
C* defined repeatedly.  ARECUR is the current active area whose		*
C* value is set in this subroutine and which should not be changed	*
C* in the application program.						*
C*									*
C* LC_UARE ( AREA, NEWFIL, IFLNO, ARECUR, STN, IRET )			*
C*									*
C* Input parameters:							*
C*	AREA		CHAR*		Input for area			*
C*	NEWFIL		LOGICAL		New file flag			*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Input and output parameters:						*
C*	ARECUR		CHAR*		Current area name		*
C*									*
C* Output parameters:							*
C*	STN		CHAR*		Station at center of area	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid area name		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* K. Tyle/GSC		 7/96	Added check for AREA = GAREA		*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stn, area, arecur
	LOGICAL		newfil
C------------------------------------------------------------------------
	iret = 0
C
C*	Check if the area has not changed or is equal to GAREA.
C
	CALL ST_LCUC ( area, area, ier )
	IF (( area .ne. arecur ) .or. newfil .or. ( area .eq. ' ' )
     +	   .or. ( area .eq. 'GAREA' )) THEN
C
C*	    Set up the search area.
C
	    CALL LC_SARE  ( area, iflno, stn, iret )
C
C*	    Return errors.
C
	    IF  ( iret .ne. 0 )  THEN
	        arecur = ' '
	      ELSE
		arecur = area
	    END IF
	END IF
C*
	RETURN
	END
