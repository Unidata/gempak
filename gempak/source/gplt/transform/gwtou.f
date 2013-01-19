	SUBROUTINE GWTOU  ( np, xm, ym, xl, yl, iret )
C************************************************************************
C* GWTOU								*
C*									*
C* This subroutine converts points in graph coordinates or rotated	*
C* lat/lon map coordinates into non-rotated linear intermediate         *
C* coordinates.								*
C*									*
C* GWTOU  ( NP, XM, YM, XL, YL, IRET )					*
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
C* S. Gilbert/NCEP	 8/06	Modified from GWTOL			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	REAL		xl (*), yl (*), xm (*), ym (*)
C-------------------------------------------------------------------------
C*	Select code for current mode.
C
	IF  ( igmode .eq. 1 )  THEN
C*
	    IF  ( mclass .eq. MCCYL ) THEN
C*
		IF ( mtmtyp .eq. 1 .or. mtmtyp .eq. 3 ) THEN
		    xang2 = anglr2
		ELSE
		    xang2 = 0.0
		END IF
		CALL GCYLML ( mproj, msppj, np, xm, ym, anglr1, xang2, 
     +			      anglr3, sclmcd, mrvrxy, xl, yl, iret )
C*
	      ELSE IF  ( mclass .eq. MCAZM ) THEN
		IF ( mtmtyp .eq. 1 ) THEN
		    xang2 = anglr2 + HALFPI
		ELSE
		    xang2 = 0.0
		END IF
		CALL GAZMML ( mproj, msppj, np, xm, ym, anglr1, xang2, 
     +			      anglr3, azmsav, xl, yl, iret )
C*
	      ELSE IF  ( mclass .eq. MCCON ) THEN
		IF ( mtmtyp .eq. 1 ) THEN
		    xang2 = anglr2
		ELSE
		    xang2 = -HALFPI
		END IF
		CALL GCONML ( mproj, msppj, np, xm, ym, anglr1, xang2, 
     +			      anglr3, concon, xl, yl, iret )
C*
	      ELSE IF  ( mclass .eq. MCMER ) THEN 
		CALL GMERML ( mproj, np, xm, ym, anglr1, xl, yl, iret)
C*
	      ELSE IF  ( mclass .eq. MCGOES ) THEN
		CALL GOESML ( mproj, np, xm, ym, xl, yl, iret )
C*
	      ELSE
	    	iret = NIPROJ
	    END IF
C
C*	    Check for graph mode.
C
	  ELSE IF  ( igmode .eq. 2 ) THEN
C
C*	    Transform the graph coordinates to intermediate coordinates.
C
	    CALL GGRFML ( jxtyp, jytyp, np, xm, ym, amlx1, amlx0, 
     +			  amly1, amly0, xl, yl, iret )
	  ELSE
	    iret = NIMODE
	END IF
C*
	RETURN
	END
