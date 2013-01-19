	SUBROUTINE RS_DHDR  ( bull, lenb, defalt, bindex, day, 
     +			      hour, obtype, iret )
C************************************************************************
C* RS_DHDR								*
C*									*
C* This subroutine determines the number of lines in a bulletin, 	*
C* decodes the day and hour from the bulletin header, and sets bindex	*
C* to point to the first character in the second line of the bulletin.	*
C* The first six contiguous digits beyond character 11 in the header    *
C* must contain the date and time.					*
C*									*
C* RS_DHDR  ( BULL, LENB, DEFALT, BINDEX, DAY, HOUR, OBTYPE, IRET)	*
C* 									*
C* Input parameters:							*
C*	BULL		CHAR*		Bulletin			*
C*	LENB		INTEGER		Bulletin length			*
C*	DEFALT		INTEGER		Missing value code		*
C*									*
C* Output parameters:							*
C*	BINDEX		INTEGER		Pointer to current char		*
C*	DAY		INTEGER		Day from bulletin header	*
C*	HOUR		INTEGER		Hour from bulletin header	*
C*	OBTYPE		CHAR*4		First four header characters	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +1 = could not find time field	*
C**									*
C* Log:									*
C* J. Nielsen/MIT	 2/89						*
C* J. Cowie/NPS		 4/89		Handle 2 additional lines at	*
C*					 start of bulletin		*
C* F. Cuq/UCLA		 8/90		Integer*2 -> Integer*4		*
C* J. Nielsen/TAMU	 2/92		Gempacized			*
C************************************************************************
	CHARACTER*(*)	bull
	INTEGER		lenb, defalt
C*
	INTEGER		bindex, day, hour, iret
	CHARACTER*4	obtype
C*
	CHARACTER*80	line
	INTEGER		leng, lindex, shift
C------------------------------------------------------------------------
	iret = 0
	day = defalt
	hour = defalt
C
C*	Strip carriage returns
C
	DO  i = 1, lenb-1
	    IF  ( ICHAR ( bull (i:i) ) .eq. 13 )  bull (i:i) = ' '
	END DO
C
C*	Find the bulletin header line (the first line which starts with
C*	a capital letter)
C
	bindex = 1
	DO WHILE  ( bull (bindex:bindex) .lt. 'A' )
	    bindex = bindex + 1
	END DO
	obtype = bull ( bindex : bindex+3 )
C
C*	Find end of first line; set pointer to start of second line
C
	leng = INDEX ( bull ( bindex : bindex+79 ), CHAR(10) ) - 1
	line = bull ( bindex : bindex+leng-1 )
	bindex = bindex + leng + 1
C
C*	Decode date and time
C
	lindex = 11
	num = 6
	CALL RS_GGRP  ( line, lindex, leng, num, shift, ier )
	IF  ( ier .lt. 0 )  THEN
	  iret = 1
	ELSE
	  inum = 2
	  CALL RS_NUMD  ( line ( lindex: ), 2, defalt, day, ier )
	  CALL RS_NUMD  ( line ( lindex+2: ), 2, defalt, hour, ier )
	ENDIF
C
	RETURN
	END
