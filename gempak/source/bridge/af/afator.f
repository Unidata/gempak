	SUBROUTINE AF_ATOR  ( awptbl, iret )
C************************************************************************
C* AF_ATOR								*
C*									*
C* This subroutine opens and reads the AIREP waypoint table.		*
C*									*
C* AF_ATOR ( AWPTBL, IRET )						*
C*									*
C* Input parameters:							*
C*	AWPTBL		CHAR*		AIREP waypoint table 		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return		*
C*					 -1 = error opening or reading	*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	awptbl
C*
	CHARACTER	stnnam*32, tbchrs*20, stat*2, coun*2
C-----------------------------------------------------------------------
	iret = -1
C
C*	Open the AIREP waypoint table file.
C
	CALL FL_TBOP  ( awptbl, 'stns', iunatb, iertop )
	IF  ( iertop .ne. 0 )  THEN
	    CALL DC_WLOG  ( 0, 'FL', iertop, awptbl, ierwlg )
	    RETURN
	END IF
C
C*	Read in the AIREP waypoint table file.
C
	ii = 1
	ierrst = 0
C
	DO WHILE  ( ( ii .le. LLSTF2 ) .and. ( ierrst .eq. 0 ) )
	    CALL TB_RSTN  ( iunatb, adwypt (ii), stnnam, istnm,
     +			    stat, coun, adlat (ii), adlon (ii),
     +			    selv, ispri, tbchrs, ierrst )
C
C*	    Decode adiccn (ii), adicpv (ii), and adnswp (ii) 
C*	    from tbchrs.
C
	    READ  ( UNIT = tbchrs, FMT = '(A, A, I2)', IOSTAT = ierred )
     +		adiccn (ii), adicpv (ii), adnswp (ii)
	    IF  ( ierred .ne. 0 )  THEN
		adiccn (ii) = '  '
		adicpv (ii) = '  '
		adnswp (ii) = IMISSD
	    END IF
	    ii = ii + 1
	END DO
C
	IF  ( ierrst .eq. -1 )  THEN
	    iret = 0
	    nade = ii - 1
	END IF
C
C*	Close the AIREP waypoint table file.
C
	CALL FL_CLOS  ( iunatb, iercls )
C*
	RETURN
	END
