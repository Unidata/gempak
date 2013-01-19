	SUBROUTINE RS_DLN2  ( bull, lenb, defalt, bindex, day, hour,
     +			      obtype, wspcod, iret )
C************************************************************************
C* RS_DLN2								*
C*									*
C* This subroutine decodes the second line in a bulletin and returns	*
C* the date, time, wind speed units code, and observation type.  The	*
C* date/time group should be coded YYGGi(w).				*
C*									*
C* RS_DLN2  ( BULL, LENB, DEFALT, BINDEX, DAY, HOUR, OBTYPE, WSPCOD,	*
C*	      IRET )							*
C* 									*
C* Input parameters:							*
C*	BULL		CHAR*		Bulletin			*
C*	LENB		INTEGER		Bulletin length			*
C*	DEFALT		INTEGER		Missing value code		*
C*									*
C* Input/output parameters:						*
C*	BINDEX		INTEGER		Pointer to 2nd line -> first ob	*
C*	DAY		INTEGER		Day of observations		*
C*	HOUR		INTEGER		Hour of observations		*
C*									*
C* Output parameters:							*
C*	OBTYPE		CHAR*4		Contents of bulletin		*
C*					 SMUS = United States synoptics	*
C*					 SMNA = Foreign synoptics	*
C*					 SHBY = Ship or buoy reports	*
C*					 CMAM = C-man platform reports	*
C*					 XXXX = Unknown type		*
C*	WSPCOD		INTEGER		Wind speed units code		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = unsupported format	*
C*					 -1 = no data in bulletin	*
C*					  1 = unknown bulletin format	*
C*					  2 = unexp. length of time grp	*
C**									*
C* Log:									*
C* J. Nielsen/MIT	 2/89						*
C* F. Cuq/UCLA		 8/90	Integer*2 -> integer*4			*
C* J. Nielsen/TAMU	 2/92	Gempacized, made MMXX undecodeable	*
C************************************************************************
	CHARACTER*(*)	bull
	INTEGER		lenb, defalt
C*
	INTEGER		bindex, day, hour
C*
	CHARACTER*4	obtype
	INTEGER		wspcod, iret
C*
	INTEGER		shift, lindex, leng, dayin, hourin
	CHARACTER	line*80, word*4
C------------------------------------------------------------------------
	iret = 0
	initdx = bindex
	wspcod = defalt
C
C*	Check length of bulletin
C
	IF  ( bindex + 30 .gt. lenb )  THEN
	    iret = -1
	    RETURN
	ENDIF
C
C*	Find the second line of bulletin
C
	leng = INDEX ( bull ( bindex : bindex+79 ), CHAR(10) ) - 1
	line = bull ( bindex : bindex+leng-1 )
C
C*	Attempt to identify report
C
	IF  ( leng .gt. 25 )  THEN
C
C*	Too long for WMO line 2; better be a US synoptic ob
C
	    IF  ( obtype .eq. 'SMUS' )  RETURN
	    obtype = 'XXXX'
	    iret = 1
	    RETURN
	ENDIF
C
C*	Set return pointer to line 3
C
	bindex = bindex + leng + 1
C
C*	Read first word in second line; if none, better be US synoptic
C
	lindex = 1
	inum = 4
	CALL RS_GCHR ( line, lindex, leng, inum, shift, ier )
	IF  ( ier .lt. 0 )  THEN
	    IF  ( obtype .eq. 'SMUS' )  RETURN
	    obtype = 'XXXX'
	    iret = 1
	    RETURN
	ENDIF
	word = line ( lindex : lindex+3 )
C
C*	Determine report type
C
	IF  ( word .eq. 'BBXX' )  THEN
	    obtype = 'SHBY'
	ELSE IF  ( word .eq. 'AAXX' )  THEN
	    obtype = 'SMNA'
	ELSE IF  ( word .eq. 'CMAN' )  THEN
	    obtype = 'CMAN'
	ELSE IF  ( word .eq. 'MMXX' )  THEN
	    obtype = 'XXXX'
	    iret = -2
	    RETURN
	ELSE
	    obtype = 'XXXX'
	    iret = 1
	    RETURN
	ENDIF
C
C*	Find time group
C
	inum = 4
	CALL RS_GGRP ( line, lindex, leng, inum, shift, ier )
	IF  ( ier .lt. 0 )  THEN
	    iret = 2
	    RETURN
	ENDIF
	inum = 5
	CALL RS_GGRP ( line, lindex, leng, inum, shift, ier )
C
C*	If only four digits found, assume IIGG
C
	IF  ( ier .lt. 0 )  iret = 2
C
C*	Get day and hour, check for plausibility
C
	CALL RS_NUMD ( line ( lindex: ), 2, defalt, dayin, ier ) 
	IF  ( dayin .gt. 31 )  RETURN
	CALL RS_NUMD ( line ( lindex+2: ), 2, defalt, hourin, ier )
	IF  ( hourin .gt. 24 )  RETURN
	day = dayin
	hour = hourin
	IF  ( iret .eq. 2 )  RETURN
C
C*	Five digits, get wind speed units code
C
	CALL RS_NUMD ( line ( lindex+4: ), 1, defalt, wspcod, ier )
C
	RETURN
	END

