	SUBROUTINE RD_RNGD ( rpline, ln, lstp, kpshr, jftmst, jftmen,
     +			     rlinl, rlinu, iret )
C************************************************************************
C* RD_RNGD								*
C*									*
C* This subroutine decodes a line of the RDF report that can have a	*
C* range of numerical data such as .01-.09.  It	may also have the	*
C* character "T" in place of a numeric for snow range.  Substitute	*
C* a value of .005 for it.						*
C*									*
C* RD_RNGD ( RPLINE, LN, LSTP, KPSHR, JFTMST, JFTMEN, RLINL, RLINU,	*
C*           IRET )							*
C*									*
C* Input parameters:							*
C*      RPLINE		CHAR*		Report line			*
C*      LN		INTEGER		Length of report line		*
C*	LSTP		INTEGER		Last position of parameter name	*
C*      KPSHR (*)	INTEGER		Positions of forecast hours	*
C*      JFTMST		INTEGER		Index of first valid GEMFTM	*
C*      JFTMEN		INTEGER		Index of last GEMFTM		*
C*									*
C* Output parameters:							*
C*      RLINL (*)	REAL		Forecast data (lower range)	*
C*      RLINU (*)	REAL		Forecast data (upper range)	*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -1 = report line format error	*
C*					  -2 = no data			*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	11/02						*
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER*(*)	rpline
	INTEGER		kpshr (*)
	REAL		rlinl (*), rlinu (*)
C*
	CHARACTER	tstr*12
	logical		fnd
C------------------------------------------------------------------------
	iret = 0
	DO ii = 1, jftmen
	    rlinl ( ii ) = RMISSD
	    rlinu ( ii ) = RMISSD
	END DO
C
C*      Find beginning and end of report line to get data.
C*      The position of the beginning of data, ibeg is at the first
C*      space following the parameter name.  The position of the end
C*      of data, iend is at the carriage return.
C
	iend = INDEX ( rpline (:ln), CHCR )
	IF ( iend .eq. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
	ibeg = 0
	ii = lstp + 1
	IF ( ii .eq. iend ) THEN
	    iret = -2
	    RETURN
	END IF
	DO WHILE ( ibeg .eq. 0 .and. ii .lt. iend )
	    IF ( rpline ( ii:ii ) .eq. ' ' ) THEN
		ibeg = ii
	    END IF
	    ii = ii + 1
	END DO
	IF ( ibeg .eq. 0 ) THEN
	    iret = -1
	    RETURN
	END IF

	jen = 0
	ii = iend
	DO WHILE ( ii .gt. ibeg .and.
     +	       ( rpline(ii:ii) .eq. ' ' .or. rpline(ii:ii) .eq. CHCR ) )
	    ii = ii - 1
	    jen = ii
	END DO
	IF ( jen .le. ibeg ) THEN
	    iret = -2
	    RETURN
	END IF
C
C*	Parse the specified line of the report
C
	ist = jftmst
	DO WHILE ( kpshr (ist) .le. lstp )
	    ist = ist + 1
	END DO
	kbeg = MAX ( ibeg,  kpshr ( ist ) - 3 )
       	DO WHILE ( jen .gt. kbeg )
C
C*	    Check if range data
C*	    First find start of this substring
C
	    ii = jen - 1
	    fnd = .false.
	    jdash = 0
    	    DO WHILE ( .not. fnd .and. ii .ge. ibeg - 1 )
	        IF ( rpline ( ii:ii ) .eq. ' ' ) THEN
		    iim1 = ii - 1
    		    IF ( iim1 .ge. ibeg - 2 ) THEN
      			IF ( rpline ( iim1:iim1 ) .eq. ' ' .or.
     +				ii .le. ibeg ) THEN
			    jbg = ii + 1
			    fnd = .true.
C
C*			  Exclude cases such '- ' as in '.0- T'
C*			  from error.  (Since range data for 12 hour
C*			  parameters, there should not be data 
C*			  being delimited by only one ' '.)
C
		          ELSE IF ( rpline ( iim1:iim1 ) .ne. '-' ) THEN
			    iret = -1
			    RETURN
			END IF
		    END IF
		  ELSE IF ( rpline ( ii:ii ) .eq. '-' ) THEN
		    jdash = ii
		END IF
		ii = ii - 1
	    END DO

	    CALL RD_XFHR ( jen, kpshr, ist, jftmen, idxfhr, ier )
	    IF ( idxfhr .ne. 0 .and. fnd ) THEN
C
C*	        Convert value(s) to reals.
C*		RD_TRCE converts "T" to .005.
C
	        IF ( jdash .eq. 0 ) THEN
	            tstr = rpline (jbg:jen)
      		    CALL ST_CRND ( tstr, rlinl ( idxfhr ), ndec, ier )
		    ln = jen - jbg + 1
		    IF ( ier .ne. 0 ) THEN
			CALL RD_TRCE ( tstr, ln, rlinl (idxfhr), ier )
		    END IF
		    rlinu ( idxfhr ) = rlinl ( idxfhr )
	          ELSE
		    tstr = rpline (jbg:jdash-1)
      		    CALL ST_CRND ( tstr, rlinl ( idxfhr ), ndec, ier )
		    ln = jdash - jbg
		    IF ( ier .ne. 0 ) THEN
                        CALL RD_TRCE ( tstr, ln, rlinl (idxfhr), ier )
                    END IF
		    IF ( rlinl ( idxfhr ) .ne. RMISSD ) THEN
		        tstr = rpline (jdash+1:jen)
      		        CALL ST_CRND ( tstr, rlinu ( idxfhr ), ndec, ier )
			ln = jen - jdash 
		        IF ( ier .ne. 0 ) THEN
			    CALL RD_TRCE ( tstr, ln, rlinu (idxfhr), ier )
			    IF ( rlinu (idxfhr) .eq. RMISSD ) THEN
				rlinl ( idxfhr ) = RMISSD
			    END IF
		        END IF
		    END IF
C
C*		    Make sure upper range is higher than lower range to
C*		    take care of range data such as: T-00 
C 
		    IF ( rlinl (idxfhr) .gt. rlinu (idxfhr) ) THEN
		        rtemp = rlinl (idxfhr)
		        rlinl (idxfhr) = rlinu (idxfhr)
		        rlinu (idxfhr) = rtemp
		    END IF
	        END IF
	    END IF
C
C*	    Find last position of next parameter value
C
	    jen = jbg - 1
	    IF ( jen .ge. kpshr(ist) - 1 ) THEN
	        ii = jen
		DO WHILE ( ii .gt. kbeg .and. rpline(ii:ii) .eq. ' ' )
		    ii = ii - 1
		    jen = ii 
	        END DO
	    END IF
	END DO
C*
	RETURN
	END
