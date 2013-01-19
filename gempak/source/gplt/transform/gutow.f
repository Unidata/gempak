	SUBROUTINE GUTOW  ( np, xl, yl, xm, ym, iret )
C************************************************************************
C* GUTOW								*
C* 									*
C* This subroutine converts points in non-rotated linear intermediate   *
C* coordinates into graph coordinates or rotated map coordinates.	*
C* 									*
C* GUTOW  ( NP, XL, YL, XM, YM, IRET )					*
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
	IF  ( igmode .eq. 2 )  THEN
C
C*	  Check that a graph has been defined.
C
	    IF ( .not. gset )  THEN
		iret = NIPROJ
	      ELSE
		iret = NORMAL
		CALL GGRFLM ( jxtyp, jytyp, np, xl, yl, amlx1, amlx0, 
     +			      amly1, amly0, xm, ym, iret )
	    END IF
C
C*	    Map mode.
C
	  ELSE IF  ( igmode .eq. 1 )  THEN
	    IF  ( .not. mset )  THEN
		iret = NIPROJ
C*
	      ELSE IF  ( mclass .eq. MCAZM )  THEN
		IF ( mtmtyp .eq. 1 ) THEN
		    xang2 = anglr2 + HALFPI
		ELSE
		    xang2 = 0.0
		END IF
		CALL GAZMLM ( mproj, msppj, np, xl, yl, anglr1, xang2,
     +			      anglr3, azmsav, xm, ym, iret )
C*
	      ELSE IF  ( mclass .eq. MCCON )  THEN
		IF ( mtmtyp .eq. 1 ) THEN
		    xang2 = anglr2
		ELSE
		    xang2 = -HALFPI
		END IF
		CALL GCONLM ( mproj, msppj, np, xl, yl, anglr1, xang2,
     +			      anglr3, concon, xm, ym, iret )
C*
	      ELSE IF  ( mclass .eq. MCCYL )  THEN
		IF ( mtmtyp .eq. 1 .or. mtmtyp .eq. 3 ) THEN
		    xang2 = anglr2
		ELSE
		    xang2 = 0.0
		END IF
C
		CALL GCYLLM ( mproj, msppj, np, xl, yl, anglr1, xang2,
     +			      anglr3, sclmcd, mrvrxy, xm, ym, iret )
C*
	      ELSE IF  ( mclass .eq. MCMER )  THEN 
		CALL GMERLM  ( mproj, np, xl, yl, anglr1, xm, ym, iret )
C*
	      ELSE IF  ( mclass .eq. MCGOES )  THEN
		CALL GOESLM  ( mproj, np, xl, yl, xm, ym, iret )
	    END IF
	  ELSE
	    iret = NIMODE
	END IF
C*
	RETURN
	END
