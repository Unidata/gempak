	SUBROUTINE G2T_RDPARM ( lunp, iret )
C************************************************************************
C* G2T_RDPARM								*
C*									*
C* This subroutine reads a table of parameters from 'G2T_PARM.TBL' for	*
C* grid-to-text.							*
C*									*
C* G2T_GTEXT  ( LUNP, IRET )						*
C*									*
C* Input parameters:							*
C*	LUNB		INTEGER		LUN for G2T_TXT.TBL		*
C*	NUMEG2T		INTEGER		Text number 			*
C*	STR(2)		CHAR*		Input wind/wave strings		*
C*									*
C* Output parameters:							*
C*	OUTMSG		CHAR*		Output message			*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*					+1 = text number not found 	*
C*					+4 = key word not found 	*
C**									*
C* Log:									*
C* T. Lee/SAIC		11/07	Created					*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER 	bufr*80, carr(4)*20
	INTEGER		indx(10)
	LOGICAL		done, contin
	INCLUDE		'EQUAL.FNC'
C*-----------------------------------------------------------------------
	iret = 0
C
C*	Search for the text number.
C
	done = .false.
	DO WHILE  ( .not. done )
	    READ   ( lunp, 1000, iostat = ier ) bufr
1000	    FORMAT ( A )
	    IF  ( ier .ne. 0 )  THEN
		done = .true.
	      ELSE
		CALL ST_LCUC ( bufr, bufr, ier )
		IF ( bufr ( 1:1 ) .ne. '!' )  THEN
		    indx ( 1 ) = INDEX ( bufr, 'NCNTER' )
		    indx ( 2 ) = INDEX ( bufr, 'LGHTS' )
		    indx ( 3 ) = INDEX ( bufr, 'MNWDIF' )
		    indx ( 4 ) = INDEX ( bufr, 'MXWDIF' )
		    indx ( 5 ) = INDEX ( bufr, 'MNSDIF' )
		    indx ( 6 ) = INDEX ( bufr, 'MXSDIF' )
		    indx ( 7 ) = INDEX ( bufr, 'MCLOCK' )
		    indx ( 8 ) = INDEX ( bufr, 'WAVE_RANGE' )
		    indx ( 9 ) = INDEX ( bufr, 'WIND_RANGE' )
C
		    IF ( indx ( 1 ) .gt. 0 )  THEN  
			CALL ST_CLST (bufr, ' ', ' ', 2, carr, num, ier)
			CALL ST_NUMB ( carr ( 2 ), NCNTER, ier )
		      ELSE IF ( indx ( 2 ) .gt. 0 )  THEN
			CALL ST_CLST (bufr, ' ', ' ', 2, carr, num, ier)
			CALL ST_NUMB ( carr ( 2 ), LGHTS, ier )
		      ELSE IF ( indx ( 3 ) .gt. 0 )  THEN
			CALL ST_CLST (bufr, ' ', ' ', 2, carr, num, ier)
			CALL ST_NUMB ( carr ( 2 ), MNWDIF, ier )
		      ELSE IF ( indx ( 4 ) .gt. 0 )  THEN
			CALL ST_CLST (bufr, ' ', ' ', 2, carr, num, ier)
			CALL ST_NUMB ( carr ( 2 ), MXWDIF, ier )
		      ELSE IF ( indx ( 5 ) .gt. 0 )  THEN
			CALL ST_CLST (bufr, ' ', ' ', 2, carr, num, ier)
			CALL ST_NUMB ( carr ( 2 ), MNSDIF, ier )
		      ELSE IF ( indx ( 6 ) .gt. 0 )  THEN
			CALL ST_CLST (bufr, ' ', ' ', 2, carr, num, ier)
			CALL ST_NUMB ( carr ( 2 ), MXSDIF, ier )
		      ELSE IF ( indx ( 7 ) .gt. 0 )  THEN
			CALL ST_CLST (bufr, ' ', ' ', 2, carr, num, ier)
			CALL ST_NUMB ( carr ( 2 ), MCLOCK, ier )
		      ELSE IF ( indx ( 8 ) .gt. 0 )  THEN
			ii = 0
			contin = .true.
			DO WHILE ( contin )
			    READ   ( lunp, 1000, iostat = ier ) bufr
			    ii = ii + 1
			    CALL ST_CLST ( bufr, ' ', ' ', 4, carr, num,
     +					   ier )
			    DO kk = 1, 4
				CALL ST_NUMB ( carr ( kk ), 
     +					       MNWGAP ( ii, kk ), ier )
			    END DO
			    IF ( MNWGAP ( ii, 2 ) .eq. -IMISSD )
     +				contin = .false.
			END DO
			llwgap = ii
		      ELSE IF ( indx ( 9 ) .gt. 0 )  THEN
			jj = 0
			contin = .true.
			DO WHILE ( contin )
			    READ   ( lunp, 1000, iostat = ier ) bufr
			    jj = jj + 1
			    CALL ST_CLST ( bufr, ' ', ' ', 4, carr, num,
     +					   ier )
			    DO kk = 1, 4
				CALL ST_NUMB ( carr ( kk ), 
     +					       MNSGAP ( jj, kk ), ier )
			    END DO
			    IF ( MNSGAP ( jj, 2 ) .eq. -IMISSD )
     +				contin = .false.
			END DO
			llsgap = jj
		    ENDIF
		END IF
	    END IF
	END DO
C*
	RETURN
	END
