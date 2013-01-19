	SUBROUTINE TG_RINC  ( tbegin, tend, tinc, itftyp, ntime, times, 
     +			      iret )
C************************************************************************
C* TG_RINC								*
C*									*
C* This subroutine returns the times in a time range with an		*
C* increment.								*
C*									*
C* TG_RINC  ( TBEGIN, TEND, TINC, ITFTYP, NTIME, TIMES, IRET )		*
C*									*
C* Input parameters:							*
C*	TBEGIN		CHAR*		Start time			*
C*	TEND		CHAR*		Stop time			*
C*	TINC		CHAR*		Time increment			*
C*	ITFTYP		INTEGER		Range type			*
C*					  1 = forecast range		*
C*					  2 = date/time range		*
C*									*
C* Output parameters:							*
C*	NTIME		INTEGER		Number of output times		*
C*	TIMES (NTIME)	CHAR*		Times in range			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-14 = invalid time increment	*
C*					-15 = too many times		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/89						*
C* M. desJardins/GSFC	10/31	Fix error codes				*
C* D. Kidwell/NCEP	 3/99	Fixed for Y2K                           *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	tbegin, tend, tinc, times (*)
C*
	INTEGER		idtbeg (3), idtend (3), idtarr (5)
	CHARACTER	dattim*20
C------------------------------------------------------------------------
	iret  = 0
	ntime = 0
C
C*	Get the time increment in hours and minutes.
C
	CALL TG_IFTM  ( 'A', tinc, ihhhmm, iret )
	ihour  = ihhhmm / 100
	iminut = ihhhmm - ihour * 100
	IF  ( ( iret .ne. 0 ) .or. ( ihhhmm .le. 0 ) )  THEN
	    iret = -14
	    RETURN
	END IF
C
C*	Break start and stop times into parts.
C
	CALL TG_CTOI  ( tbegin, idtbeg, ier )
	CALL TG_CTOI  ( tend,   idtend, ier )
C
C*	Check case where forecast time is to be incremented.
C
	IF  ( itftyp .eq. 1 )  THEN
C
C*	    Increment forecast time and check against last time.
C
	    jhour  = idtbeg (3) / 100
	    jminut = idtbeg (3) - jhour * 100
	    DO WHILE  ( idtbeg (3) .le. idtend (3) )
		IF  ( ntime .ge. LLMXGT )  THEN
		    iret = -15
		    RETURN
		END IF
		ntime = ntime + 1
		CALL TG_ITOC  ( idtbeg, times (ntime), ier )
		jhour  = jhour  + ihour
		jminut = jminut + iminut
		DO WHILE  ( jminut .ge. 60 )
		    jhour  = jhour  + 1
		    jminut = jminut - 60
		END DO
		idtbeg (3) = jhour * 100 + jminut
	    END DO
C
C*	    Otherwise, add to second time.
C
	  ELSE
C
C*	    Increment date/time and check against last time.
C
	    jhour  = idtbeg (2) / 100
	    jminut = idtbeg (2) - jhour * 100
C
C*	    Use 4-digit year for date comparison.
C
	    CALL TI_YYMD ( idtbeg ( 1 ), ibeg1, ier )
	    CALL TI_YYMD ( idtend ( 1 ), iend1, ier )
C
	    DO WHILE  ( ( ibeg1 .lt. iend1 ) .or.
     +			( ( ibeg1 .eq. iend1 ) .and.
     +			  ( idtbeg (2) .le. idtend (2) ) ) )
		IF  ( ntime .ge. LLMXGT )  THEN
		    iret = -15
		    RETURN
		END IF
		ntime = ntime + 1
		CALL TG_ITOC  ( idtbeg, times (ntime), ier )
C
C*		Add hours and minutes.
C
		jhour  = jhour  + ihour
		jminut = jminut + iminut
C
C*		Correct hours and minutes.
C
		DO WHILE  ( jminut .ge. 60 )
		    jhour  = jhour  + 1
		    jminut = jminut - 60
		END DO
		nday = 0
		DO WHILE  ( jhour .ge. 24 )
		    nday  = nday + 1
		    jhour = jhour - 24
		END DO
C
C*		Add on required number of days.
C
		idtbeg (2) = jhour * 100 + jminut
		IF  ( nday .gt. 0 )  THEN
		    CALL TI_CDTM  ( idtbeg (1), idtbeg (2), dattim, i )
		    CALL TI_CTOI  ( dattim, idtarr, ier )
		    DO  iday = 1, nday
			CALL TI_ADDD  ( idtarr, idtarr, ier )
		    END DO
		    CALL TI_ITOC  ( idtarr, dattim, ier )
		    CALL TI_IDTM  ( dattim, idtbeg (1), idtbeg (2), i )
	            CALL TI_YYMD  ( idtbeg ( 1 ), ibeg1, ier )
		END IF
	    END DO
	END IF
C*
	RETURN
	END
