	SUBROUTINE RD_CDAT ( rpline, ln, lstp, kpshr, jftmst, jftmen,
     +			     cline, iret )
C************************************************************************
C* RD_CDAT								*
C*									*
C* This subroutine decodes a line of the RDF report that has character	*
C* data.								*
C*									*
C* RD_CDAT ( RPLINE, LN, LSTP, KPSHR, JFTMST, JFTMEN, CLINE, IRET )	*
C*									*
C* Input parameters:							*
C*      RPLINE		CHAR*		Report line			*
C*      LN		INTEGER		Length of report line		*
C8	LSTP		INTEGER		Last position of parameter name *
C*      KPSHR (*)	INTEGER		Positions of forecast hours	*
C*      JFTMST		INTEGER		Index of first valid fcst hour	*
C*      JFTMEN		INTEGER		Index of last fcst hour to use	*
C*									*
C* Output parameters:							*
C*      CLINE (*)	CHAR		Forecast data			*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -1 = report line format error	*
C*					  -2 = no data			*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 9/02						*
C* F. J. Yen/NCEP	11/02	Reworked for additional formats & parms	*
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER*(*)	rpline, cline (*)
	INTEGER		kpshr(*)
C*
	CHARACTER	tstr*3
C------------------------------------------------------------------------
	iret = 0
	DO ii = 1, jftmen
	    cline ( ii ) = ' XX'
	END DO
C
C*	Find beginning and end of report line to get data
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
	    jbg = jen - 2
	    IF ( jbg .ge. kbeg ) THEN
                tstr = rpline ( jbg:jen )
                IF ( tstr ( 2:2 ) .eq. ' ' .and.
     +                   tstr ( 1:1 ) .ne. ' ' ) THEN
                    iret = -1
                    RETURN
                END IF
              ELSE
                tstr = '  ' // rpline ( jen:jen )
	    END IF
C
C*	    Find closest forecast hour.
C
            CALL RD_XFHR ( jen, kpshr, ist, jftmen, idxfhr, ier )
	    IF ( idxfhr .ne. 0 ) THEN
	        cline (idxfhr) = tstr
	    END IF
C
C*	        Find last position of next parameter value.
C
	    jen = jbg - 1
	    ii = jen
	    DO WHILE ( ii .gt. kbeg .and. rpline(ii:ii) .eq. ' ' )
	        ii = ii - 1
	        jen = ii
	    END DO
	END DO
C*
	RETURN
	END
