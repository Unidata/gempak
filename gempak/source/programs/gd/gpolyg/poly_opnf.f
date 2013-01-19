	SUBROUTINE POLY_OPNF ( nop, lunp, outfil, iret )
C************************************************************************
C* POLY_OPNF  								*
C*									*
C* This subroutine opens a Common Alerting Protocol (CAP) file.	 The	*
C* naming format: YYMMDDHH_cc_nn.xml, where cc is the center name,	*
C* nn nth polygon.							*
C*									*
C* POLY_OPNF ( NOP, LUNP, OUTFIL, IRET )				*
C*									*
C* Input parameters:							*
C*	NOP		INTEGER		Nth polygon			*
C*									*
C* Output parameters:							*
C*	LUNP		INTEGER		LUN for CAP message file	*
C*	OUTFIL		INTEGER		Cap output file name		*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*					-1 = cannot open file		*
C*									*
C**									*
C* Log:									*
C* T. Lee/SAIC		 2/08						*
C************************************************************************
	INCLUDE		'gpolyg.cmn'
	CHARACTER	outfil*(*)
	CHARACTER	cap*3, xml*4, cnop*3
	DATA		cap /'CAP'/, xml/'.xml'/
C-----------------------------------------------------------------------
	iret = 0
C
	CALL ST_LCUC ( cap, cap, ier )
	CALL ST_INCH ( nop, cnop, ier )
	CALL ST_LSTR ( cnop, nc, ier )
	CALL ST_LSTR ( gdattm, ngd, ier )
C
	IF ( idxzon .eq. 1 )  THEN
	    outfil =  gdattm (:ngd) // '_pac_' // cnop ( : nc ) // xml 
	  ELSE IF ( idxzon .eq. 2 )  THEN
	    outfil =  gdattm (:ngd) // '_atl_' // cnop ( : nc ) // xml 
	  ELSE IF ( idxzon .eq. 3 )  THEN
	    outfil =  gdattm (:ngd) // '_tpc_' // cnop ( : nc ) // xml 
	END IF
	CALL FL_SWOP ( outfil, lunp, ier )
C
	IF  ( ier .lt. 0 )  THEN
	    iret = -4
	    RETURN
	END IF
C*
	RETURN
	END
