	SUBROUTINE GUTOL  ( np, xm, ym, xl, yl, iret )
C************************************************************************
C* GUTOL								*
C*									*
C* This subroutine converts non-rotated linear intermediate coordinates *
C* into linear intermediate coordinates.				*
C*									*
C* GUTOL  ( NP, XM, YM, XL, YL, IRET )					*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C*	XM (NP)		REAL		X coordinates			*
C*	YM (NP)		REAL		Y coordinates			*
C*									*
C* Output parameters:							*
C*	XL (NP)		REAL		X coordinates			*
C*	YL (NP)		REAL		Y coordinates			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Gilbert/NCEP	 8/06	Modified from GWTOL                     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	REAL		xl (*), yl (*), xm (*), ym (*)
C-------------------------------------------------------------------------
        iret = NORMAL
C*	Select code for current mode.
C
	IF  ( igmode .eq. 1 )  THEN
C*
	    IF  ( mclass .eq. MCCYL ) THEN
C
C*		Rotate U coordinates.
C
		IF ( rotaml ) CALL GROTTL ( 'M', +1, np, xm, ym, ier ) 
C*
	    END IF
C
C*	    Check for graph mode.
C
	ELSE IF  ( igmode .ne. 2 )  THEN
            iret = NIMODE
	END IF
C*
        DO I = 1,np
            xl(i) = xm(i)
            yl(i) = ym(i)
        ENDDO
C*
	RETURN
	END
