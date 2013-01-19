	SUBROUTINE SNESTN  ( lunedt, time, istime, stid, istnm, slat, 
     +			     slon, selv, iret )
C************************************************************************
C* SNESTN								*
C*									*
C* This subroutine finds the next station header in the EDIT file.	*
C*									*
C* SNESTN ( LUNEDT, TIME, ISTIME, STID, ISTNM, SLAT, SLON, SELV, IRET ) *
C*									*
C* Input parameters:							*
C*	LUNEDT		INTEGER		LUN for edit file		*
C*									*
C* Output parameters:							*
C*	TIME		CHAR*		Time				*
C*      ISTIME		INTEGER		Station time HHMM format	*
C*	STID		CHAR*		Station identifier		*
C*	ISTNM		INTEGER		Station number			*
C*	SLAT		REAL		Station latitude		*
C*	SLON		REAL		Station longitude		*
C*	SELV		REAL		Station elevation		*
C*	IRET		INTEGER		Return code			*
C*					 +1 = end of file		*
C*					  0 = normal			*
C*					 -6 = station header not found	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/88						*
C* M. desJardins/GSFC	12/89	Fixed // for Apollo			*
C* S. Schotz/GSC	12/89	Now gets station time			*
C* S. Schotz/GSC	12/89	Account for missing station header	*
C* 				parameters				*
C* S. Schotz/GSC	 8/90	Corrected infinite loop errro		*
C* T. Piper/SAIC	 4/02	Added iostat check for st_lcuc/st_ldsp	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stid, time
C*
	CHARACTER	record*132, ckey (20)*16, ttt*20, stkeys(7)*6
	LOGICAL		eof
C*
	DATA		stkeys/'TIME', 'STID', 'STNM', 'SLAT', 'SLON',
     +                         'SELV', 'STIM'/
C------------------------------------------------------------------------
	iret   = 0
C
C*	Initialize station header.
C
	time  = ' '
	stid  = ' '
	istnm = IMISSD
	slat  = RMISSD
	slon  = RMISSD
	selv  = RMISSD
C
C*	Loop through records looking for the record containing stations.
C
	iostat = 0
	eof    = .false.
	DO WHILE  ( iostat .eq. 0 )
	    READ   ( lunedt, 10, IOSTAT = iostat )  record
10	    FORMAT ( A )
	    IF  ( iostat .eq. 0 )  THEN
	        CALL ST_LCUC  ( record, record, ier )
	        CALL ST_LDSP  ( record, record, n, ier )
	    ELSE
		eof = .true.
	    END IF
C
C*	    Skip blank records.
C
	    IF  ( ( iostat .eq. 0 ) .and. ( n .gt. 0 ) )  THEN
C
C*		Check for = meaning this is a keyword.  If record
C*		is not blank and there is no =, then station header
C*		has been read.
C
		ip = INDEX  ( record, '=' )
		IF  ( ip .eq. 0 )  THEN
		    iostat = -1
		    CALL FL_BKSP  ( lunedt, ier )
C
C*		    If this is a PARM statement, it is an error.
C
		  ELSE IF  ( INDEX ( record, 'PARM' ) .ne. 0 )  THEN
		    iret = -14
		    RETURN
		  ELSE IF  ( INDEX ( record, ';' ) .eq. 0 )  THEN
C
C*		    Break record into substrings.
C
		    CALL ST_C2C  ( record, 20, ckey, n, ier )
C
C*		    Loop through checking for station keywords.
C
		    i = 1
		    DO WHILE  ( i .lt. n )
C
C*			Fill in values for keywords.
C
			IF  ( ckey (i) .eq. 'TIME' )  THEN
			    CALL ST_FIND ( ckey(i+1), stkeys, 7, ipos,
     +                                     ier )
                            IF ( ipos .eq. 0) THEN
  		                time = ckey (i+1)
			        CALL ST_LSTR  ( time, lent, ier )
                                ttt = time
			        time = ttt ( 1:lent ) // '/' // 
     +                                 ckey (i+2)
			        i = i + 3
                            ELSE
                                i = i + 1
                            END IF
			  ELSE IF  ( ckey (i) .eq. 'STID' )  THEN
			    CALL ST_FIND ( ckey(i+1), stkeys, 7, ipos,
     +                                     ier )
                            IF ( ipos .eq. 0) THEN
			        stid = ckey (i+1)
                                i = i + 2
                            ELSE
                                i = i + 1
                            END IF
			  ELSE IF  ( ckey (i) .eq. 'STNM' )  THEN
			    CALL ST_FIND ( ckey(i+1), stkeys, 7, ipos,
     +                                     ier )
                            IF ( ipos .eq. 0) THEN
			        CALL ST_NUMB  ( ckey (i+1), istnm, ier )
                                i = i + 2
                            ELSE
                                i = i + 1
                            END IF
			  ELSE IF  ( ckey (i) .eq. 'SLAT' )  THEN
			    CALL ST_FIND ( ckey(i+1), stkeys, 7, ipos,
     +                                     ier )
                            IF ( ipos .eq. 0) THEN
			        CALL ST_CRNM  ( ckey (i+1), slat, ier )
                                i = i + 2
                            ELSE
                                i = i + 1
                            END IF
			  ELSE IF  ( ckey (i) .eq. 'SLON' )  THEN
			    CALL ST_FIND ( ckey(i+1), stkeys, 7, ipos,
     +                                     ier )
                            IF ( ipos .eq. 0) THEN
			        CALL ST_CRNM  ( ckey (i+1), slon, ier )
                                i = i + 2
                            ELSE
                                i = i + 1
                            END IF
			  ELSE IF  ( ckey (i) .eq. 'SELV' )  THEN
			    CALL ST_FIND ( ckey(i+1), stkeys, 7, ipos,
     +                                     ier )
                            IF ( ipos .eq. 0) THEN
			        CALL ST_CRNM  ( ckey (i+1), selv, ier )
                                i = i + 2
                            ELSE
                                i = i + 1
                            END IF
                          ELSE IF  ( ckey (i) .eq. 'STIM' )  THEN
			    CALL ST_FIND ( ckey(i+1), stkeys, 7, ipos,
     +                                     ier )
                            IF ( ipos .eq. 0) THEN
			        CALL ST_NUMB ( ckey (i+1), istime, ier )
                                i = i + 2
                            ELSE
                                i = i + 1
                            END IF
                          ELSE
                            i = i + 1
			END IF
		    END DO
		END IF
	    END IF
	END DO
C
C*	If no station was found, return error.
C
	IF  ( ( time .eq. ' ' ) .or. ( ( stid .eq. ' ' ) .and.
     +				       ( istnm .eq. IMISSD ) ) )  THEN
	    IF  ( eof )  THEN
		iret = +1
	      ELSE
		iret = -6
	    END IF
	END IF
C*
	RETURN
	END
