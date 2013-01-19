	SUBROUTINE SV_CNTT ( btin, jcnt, lenb, cnties, exptim, jx, iret )
C************************************************************************
C* SV_CNTT								*
C*									*
C* This subroutine gets the county (or zone) string and end time string *
C* from the WMO bulletin.						*
C*									*
C* SV_CNTT  ( BTIN, JCNT, LENB, CNTIES, EXPTIM, JX, IRET )		*
C*									*
C* Input parameters:							*
C*	BTIN		CHAR*		ST_UNPRed Bulletin		*
C*	JCNT		INTEGER		Start position			*
C*	LENB		INTEGER		Length of bulletin		*
C*									*
C* Output parameters:							*
C*      CNTIES		CHAR*		Counties (or zones) string	*
C*      EXPTIM		CHAR*		Expiration time			*
C*      JX		INTEGER		Position of pointer in string	*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					 -10 = county string not found	*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	12/00						*
C* F. J. Yen/NCEP	01/01	Modified to disregard '$$' delimeter.	*
C* F. J. Yen/NCEP	09/02	Expanded for ">"; added zone to prolog.	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	btin, cnties, exptim	
C*
	LOGICAL		start, expfnd
	CHARACTER       substr (2)*1
	INTEGER         ilens (2)
	DATA            substr / '-', '>' /
	DATA            ilens  / 1, 1 /

C------------------------------------------------------------------------
	iret = 0
	cnties = ' '
	exptim = ' '
	ibcnty = 0
C
C*	Find the indices for the counties and ending time string.
C
	jx = jcnt - 1
	start = .false.
	DO WHILE ( jx .lt. lenb - 6 .and. .not. start )
	    jx = jx + 1
	    ldashr = INDEX ( btin(jx:lenb), '-' )
	    CALL ST_NXTS ( btin(jx:lenb), 1, lenb-jx+1, substr, ilens,
     +			2, ldashr, isubst, ier )
	    IF ( ier .ne. 0 ) THEN
		iret = -10
		RETURN
	    END IF
	    IF ( ldashr .lt. 7 ) THEN
		jx = jx + ldashr - 1
	      ELSE
		jx = jx + ldashr - 7
		CALL SV_ALNM ( btin(jx:lenb), 3, ityp, ier )
		ibcnty = jx
		jx = jx + 3
		IF ( ityp .eq. 2 ) THEN
		    CALL SV_ALNM (btin(jx:lenb), 3, ityp, ier )
		    jx = jx + 3
		    IF ( ityp .eq. 1 ) THEN
		        start = .true.
		    END IF    
		  ELSE
		    jx = jx + 3
		END IF
	    END IF
	END DO
	expfnd = .false.
	IF ( start ) THEN
	    DO WHILE ( (jx .lt. lenb - 6) .and. (.not. expfnd) )
	        jx = jx + 1
		CALL ST_NXTS ( btin(jx:lenb), 1, lenb-jx+1, substr, ilens,
     +                  2, ldashr, isubst, ierd )
		IF ( ldashr .ge. 4 .and. ldashr .le. 6 ) THEN
		    nc = ldashr - 1
		    CALL SV_ALNM ( btin(jx:lenb), nc, ityp, ier )
		    IF ( ityp .eq. 1 ) THEN
			jx = jx + nc
		      ELSE
			iret = -10
			RETURN
		    END IF
		  ELSE IF ( ldashr .ge. 7 .and. ldashr .le. 8 ) THEN			
		    nc = ldashr - 1
		    IF ( ldashr .eq. 8 .and. btin(jx:jx) .eq. ' ' ) THEN
			jx = jx + 1
			nc = nc - 1
		    END IF
		    CALL SV_ALNM ( btin(jx:lenb), nc, ityp, ier )
		    IF ( ityp .eq. 1 ) THEN
			iecnty = jx - 1
			exptim = btin(jx:jx+nc)
			expfnd = .true.
		    END IF
		    jx = jx + nc
		  ELSE
		    IF ( btin(jx:jx) .eq. ' ' ) THEN
			jx = jx + 1
		    END IF 
		    nc = 6
		    CALL SV_ALNM ( btin(jx:lenb), nc, ityp, ier )
		    IF ( ityp .eq. 1 ) THEN
			iecnty = jx - 1
			exptim = btin(jx:jx + nc - 1)
			expfnd = .true.
			jx = jx + nc
		      ELSE
			iret = -10
			RETURN
		    END IF
		END IF
	    END DO
	    IF ( expfnd ) THEN
		cnties = btin (ibcnty:iecnty)
	    END IF
	END IF
C*
	RETURN
	END
