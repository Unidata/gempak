	SUBROUTINE RD_IDAT ( rpline, ln, lstp, kpshr, jftmst, jftmen,
     +			     mxmnfl, line, iret )
C************************************************************************
C* RD_IDAT								*
C*									*
C* This subroutine decodes a line of the RDF report that has numerical	*
C* data.								*
C*									*
C* RD_IDAT ( RPLINE, LN, LSTP, KPSHR, JFTMST, JFTMEN, MXMNFL, LINE,	*
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*      RPLINE		CHAR*		Report line			*
C*	LN		INTEGER		Length of report line		*
C*	LSTP		INTEGER		Last position of parameter name	* 
C*      KPSHR (*)	INTEGER		Positions of forecast hours	*
C*      JFTMST		INTEGER		Index of first valid GEMFTM	*
C*      JFTMEN		INTEGER		Index of last GEMFTM		*
C*	MXMNFL		LOGICAL		FLag for MX/MN data		*
C*									*
C* Output parameters:							*
C*      LINE (*)	INTEGER		Forecast data			*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -1 = report line format error	*
C*					  -2 = no data			*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 9/02						*
C* F. J. Yen/NCEP	11/02	Reworked for additional formats & parms	*
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER*(*)	rpline
	INTEGER		kpshr(*), line (*)
	LOGICAL		mxmnfl
C*
	CHARACTER	tstr*3
C------------------------------------------------------------------------
	iret = 0
	DO ii = 1, jftmen
	    line ( ii ) = IMISSD
	END DO
C
C*	Find beginning and end of report line to get data.
C*	The position of the beginning of data, ibeg is at the first
C*	space following the parameter name.  The position of the end
C*	of data, iend is at the carriage return.
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

	kmxmn = 0
	ist = jftmst
    	DO WHILE ( kpshr (ist) .le. lstp )
	    ist = ist + 1
	END DO
	kbeg = MAX ( ibeg,  kpshr ( ist ) - 3 )
   	DO WHILE ( jen .gt. kbeg )
	    jbg = jen - 2
            IF ( jbg .ge. kbeg ) THEN
	        tstr = rpline ( jbg:jen )
	        IF ( tstr ( 2:2 ) .eq. ' ' .and.
     +		         tstr ( 1:1 ) .ne. ' ' ) THEN
		    iret = -1
		    RETURN
		END IF
	      ELSE
		tstr = '  ' // rpline ( jen:jen )
	    END IF

C
C*	    Find closest forecast hour
C
	    CALL RD_XFHR ( jen, kpshr, ist, jftmen, idxfhr, ier )
   	    IF ( ier .eq. 0 ) THEN
C
C*	        Convert each string to an integer.
C
      		CALL ST_NUMB ( tstr, line ( idxfhr ), ierr )

	        IF ( tstr (3:3) .eq. 'M' ) THEN
		   line ( idxfhr ) = IMISSD
		   ierr = 0
	        END IF
	        IF ( ierr .ne. 0 ) THEN
		    iret = -1
		    RETURN
	        END IF
	        IF ( line (idxfhr) .eq. 999 ) THEN
		   line ( idxfhr ) = IMISSD
	        END IF
	 		
	      ELSE IF ( mxmnfl ) THEN
		IF ( jen .ge. kpshr(jftmen) ) THEN
		   kmxmn = 1
		END IF
	    END IF
C
C*	    Find the last position of the next parameter
C
	    jen = jbg - 1
	    ii = jen
	    DO WHILE ( ii .gt. kbeg .and. rpline(ii:ii) .eq. ' ' )
		ii = ii - 1
		jen = ii 
	    END DO
	END DO
C
C*	Get rid of false minus numbers if group of MX/MN data
C*	due to the dash delimeters.
C
	IF ( mxmnfl ) THEN
	    ii = jftmen
	    DO WHILE ( ii .ge. ist + 2 )
		IF ( line (ii) .ne. IMISSD .and.
     +			    line (ii) .lt. 0 ) THEN
     		    IF ( line (ii - 1) .ne. IMISSD .and.
     +				line (ii - 1) .lt. 0  .and.
     +			        line (ii - 2) .gt. 0 )  THEN
		      line (ii) = - line (ii)
		      line (ii - 1) = - line (ii - 1)
		      ii = ii - 3
		     ELSE IF ( kmxmn .eq. 1 .and.
     +			        line (ii - 1) .gt. 0 ) THEN
		      line (ii) = - line (ii)
		      ii = ii - 2
		     ELSE
		      ii = ii - 1
		    END IF
		  ELSE
		    ii = ii - 1
		END IF
	    END DO
	END IF
C*
	RETURN
	END
