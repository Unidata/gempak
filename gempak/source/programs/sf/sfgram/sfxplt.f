	SUBROUTINE SFXPLT  ( iside, parms, prmtyp, icolor, data, cdata,
     +			     ntime, iptprm, mkcolr, xmndst, xval, iret )
C************************************************************************
C* SFXPLT								*
C*									*
C* This subroutine plots the parameters on one side of a single trace.	*
C*									*
C* SFXPLT  ( ISIDE, PARMS, PRMTYP, ICOLOR, DATA, CDATA, NTIME, IPTPRM,	*	
C*           MKCOLR, XMNDST, XVAL, IRET )				*
C*									*	
C* Input parameters:							*
C*	ISIDE		INTEGER		Side for title			*
C*					  1 = left			*
C*					  2 = right			*
C*	PARMS  (4)	CHAR*		Parameter names			*
C*	PRMTYP (4)	CHAR*		Parameter types			*
C*	ICOLOR (4)	INTEGER		Colors				*
C*	DATA  (NTIME,*)	REAL		Data				*
C*	CDATA (NTIME,*)	CHAR*		Character data			*
C*	NTIME		INTEGER		Number of times			*
C*	IPTPRM (4)	INTEGER		Pointers to data		*
C*	MKCOLR		INTEGER		Marker color			*
C*									*
C* Output parameters:							*
C*	XMNDST		REAL						*
C*	XVAL(*)		REAL						*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/90						*
C* K. Brill/NMC		12/91	Added plot type P (sky & wind)		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	parms (*), prmtyp (*), cdata ( NTIME, * )
	REAL		data ( NTIME, * ), xval (*)
	INTEGER		iptprm (*), icolor (*)
C*
C--------------------------------------------------------------------------
	iret = 0
C
C*	Loop through parameters on this side.
C
	DO  i = 1, 4
C*
	    ii = iptprm (i)
C
C*	    Check for different types of parameters.
C
	    IF  ( prmtyp (i) .eq. 'W' )  THEN
C
C*		Plot winds.
C
		CALL SFXWND ( i, parms (i), icolor (i),
     +			      data (1,ii), ntime, xval, ier )
C
C*		Plot symbols.
C
	      ELSE IF  ( prmtyp (i) .eq. 'S' ) THEN
		CALL SFXSYM ( i, parms (i), icolor (i),
     +			      data (1,ii), ntime, xval, ier )
C
C*		Plot gusts.
C
	      ELSE IF  ( prmtyp (i) .eq. 'G' )  THEN
		CALL SFXGST  ( parms (i), icolor (i), data (1,ii),
     +			       ntime, xval, ier )
C
C*		Plot real valued data.
C
	      ELSE IF  ( prmtyp (i) .eq. 'R' )  THEN
		CALL SFXREL  ( parms (i), icolor (i), data (1,ii), 
     +			       ntime, mkcolr, xmndst, xval, ier )
C
C*		Plot character data.
C
	      ELSE IF  ( prmtyp (i) .eq. 'C' )  THEN
		CALL SFXCHR ( i, parms (i), icolor (i),
     +     		      cdata (1,ii), ntime, xval, ier )
C
C*		Plot sky coverage combined with wind.
C
	      ELSE IF  ( prmtyp (i) .eq. 'P' )  THEN
		CALL SFXSKY ( i, parms (i), icolor (i),
     +			      data (1,ii), ntime, xval, ier )
	    END IF
	END DO
C*
	RETURN
	END
