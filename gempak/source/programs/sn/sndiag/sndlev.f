	SUBROUTINE SNDLEV  ( levels, vcoord, ivert, nlev, rlevel, 
     +			     levtyp, voutc, lvert, iret )
C************************************************************************
C* SNDLEV								*
C*									*
C* This subroutine returns a list of levels or the limits of a range.	*
C* The vertical coordinate type is also converted into a integer code.	*
C*									*
C* SNDLEV  ( LEVELS, VCOORD, IVERT, NLEV, RLEVEL, LEVTYP, VOUTC,	*
C*	     LVERT, IRET )						*
C*									*
C* Input parameters:                                                    *
C*	LEVELS		CHAR*		User input for LEVELS		*
C*	VCOORD		CHAR*		User input for VCOORD		*
C*	IVERT		INTEGER		Dataset coordinate system	*
C*									*
C* Output parameters:							*
C*	NLEV		INTEGER		Number of levels		*
C*	RLEVEL (NLEV)	REAL		List of levels			*
C*	LEVTYP		INTEGER		Type of level specification	*
C*					  1 = list of levels		*
C*					  2 = range without increment	*
C*	VOUTC		CHAR*		Output vertical coordinate      *
C*	LVERT		INTEGER		Output vertical coordinate      *
C*	IRET		INTEGER		Return code                     *
C*					  0 = normal return		*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92		Copied from SNLLEV		*
C* J. Whistler/SSAI	 4/93		Cleaned up 			*
C* Chiz/Unidata		05/96           Fixed LV_INPT call for 5.2.1    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	levels, vcoord, voutc
	REAL		rlevel (*)
	LOGICAL		mandlev
C------------------------------------------------------------------------
	iret = 0
C
C*	Change vcoord from DSET to actual vertical coordinate.
C
	CALL ST_LCUC  ( vcoord, voutc, ier )
	IF  ( voutc .eq. 'DSET' )  CALL LV_CCRD ( ivert, voutc, ier )
C
C*	Get levels.
C
	mandlev = .false.
	CALL LV_INPT  ( levels, LLMXLV, voutc, nlev, rlevel, levtyp, 
     +			voutc,  lvert, mandlev , iret )
	IF  ( iret .ne. 0 )  THEN
	    IF  ( levels .ne. ' ' )  THEN
                IF  ( iret .eq. -2 ) THEN
                    CALL ER_WMSG  ( 'LV', iret, 'MAN', ier )
                ELSE
		    CALL ER_WMSG  ( 'LV', iret, levels, ier )
                END IF
	    ELSE
                iret = 0          
	        nlev = 0
            END IF
	END IF
C*
	RETURN
	END
