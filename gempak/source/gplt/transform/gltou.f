	SUBROUTINE GLTOU  ( np, xl, yl, xm, ym, iret )
C************************************************************************
C* GLTOU								*
C* 									*
C* This subroutine converts points in rotated linear intermediate       *
C* coordinates into non-rotated linear intermediate coordinates.  	*
C* 									*
C* GLTOU  ( NP, XL, YL, XM, YM, IRET )					*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C* 	XL (NP)		REAL		X coordinates 			*
C* 	YL (NP)		REAL		Y coordinates 			*
C* 									*
C* Output parameters: 							*
C* 	XM (NP)		REAL		X coordinates			*
C* 	YM (NP)		REAL		Y coordinates			*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Gilbert/NCEP	 8/06	Modified from GLTOW                     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	REAL		xl (*), yl (*), xm (*), ym (*)
C--------------------------------------------------------------------------
C*	Select code for current mode.
C
        iret = NORMAL
C
C*	    Map mode.
C
	IF  ( igmode .eq. 1 )  THEN
	    IF  ( .not. mset )  THEN
		iret = NIPROJ
C*
	    ELSE IF  ( mclass .eq. MCCYL )  THEN
C
C*		Unrotate L coordinates.
C
		IF ( rotaml ) CALL GROTTL ( 'M', -1, np, xl, yl, ier )
C*
	    END IF
C
	ELSE IF ( igmode .ne. 2 ) THEN
	    iret = NIMODE
	END IF

        DO I = 1, np
            xm(i) = xl(i)
            ym(i) = yl(i)
        ENDDO
C*
	RETURN
	END
