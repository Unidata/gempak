	SUBROUTINE GH_BKTB ( iret )
C************************************************************************
C* GH_BKTB								*
C*									*
C* This subroutine reads the breakpoint table.				*
C*									*
C* GH_BKTB ( IRET )                                                     *
C*									*
C* Output Parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return             *
C*					 -9 = error in opening table    *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 4/03	                                        *
C* D. Kidwell/NCEP	11/03	Stid ->bkstid (in ghcmn), new bktbch fmt*
C* D. Kidwell/NCEP	 1/04	Changed storage for special fill areas  *
C* m.gamazaychikov/SAIC	03/04	from GH_BKRD				*
C* m.gamazaychikov/SAIC	07/04	removed obsolete lines of code		*
C* m.gamazaychikov/SAIC	01/05	added a check after call to ST_NUMB	*
C* M. Li/SAIC            1/06   Changed county codes to zone            *
C* S. Gilbert/NCEP	10/07	Added bkstate and bkcntry to common	*
C* S. Gilbert/NCEP	06/09	Added last ME sequence number to common	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ghcmn.cmn'
C*
	CHARACTER     	table*20, type*4, stnnam (MAXBK)*32
        CHARACTER       tag*25, dirsym*160, tblnam*72, value*12
	INTEGER		isnm (MAXBK), iarea (MAXBK), itype (MAXBK)
	REAL		selv (MAXBK)
C------------------------------------------------------------------------
	iret  = 0
	value = ' '
C
C*      Read the value of tag TCV_ZONE in prefs.tbl.
C
        tag     = 'TCV_ZONE'
        tblnam  = 'prefs.tbl'
        dirsym  = 'config'
C
        CALL ST_NULL ( tblnam, tblnam, lens, ier )
        CALL ST_NULL ( dirsym, dirsym, lens, ier )
        CALL ST_NULL ( tag, tag, lens, ier)
        CALL CTB_RDPRF ( tblnam, dirsym, tag, value, ier1 )
        CALL ST_RNUL ( value, value, lens, ier )
C
C*	Open the table.
C
	IF ( value .eq. 'COUNTY' ) THEN
	    table = 'tcabkpt.tbl'
	  ELSE
	    table = 'tcabkptlz.tbl'
	END IF
	type  = 'stns'
        CALL FL_TBOP ( table, type, lun, ier )
C
        IF ( ier .ne. 0 ) THEN
	    iret = -9
            RETURN
        END IF 
C
C*	Read the breakpoint table.
C
	maxstn = MAXBK
	CALL TB_ASTN ( lun, maxstn, nstn, bkstid, stnnam, isnm, bkstate,
     +		       bkcntry, bklat, bklon, selv, ibkpri, bktbch, ier )
C
C*	Get count of breakpoints.
C
	DO ii = 1, NAREAS
	    nbkpts ( ii ) = 0
	END DO
C
	DO ii = 1, nstn
	    iarea ( ii ) = ibkpri ( ii ) / 10
	    itype ( ii ) = MOD ( ibkpri ( ii ), 10 )
	    IF ( ( ( itype ( ii ) .eq. 0 ) .or. 
     +             ( itype ( ii ) .eq. 9 ) ) .and. 
     +		 ( iarea ( ii ) .ge. 1 ) .and. 
     +		 ( iarea ( ii ) .le. NAREAS ) ) THEN
		nbkpts ( iarea ( ii ) ) = nbkpts ( iarea ( ii ) ) + 1
	    END IF
	END DO
C
	larea = 0
	lines = 0
	DO ii = 1, nstn
     	    ibkseq ( ii ) = 0
	    IF ( iarea ( ii ) .ne. larea ) THEN
		iseq  = 0
		larea = iarea ( ii )
		indxbk ( larea ) = ii
	    END IF
	    IF ( ( itype ( ii ) .eq. 0 ) .or. 
     +           ( itype ( ii ) .eq. 9 ) ) THEN
		IF ( ( iarea ( ii ) .ge. 1 ) .and.
     +		     ( iarea ( ii ) .le. NAREAS ) ) THEN
		    iseq = iseq + 1
		    ibkseq ( ii ) = iseq
		END IF
	    END IF
	END DO
	indxbk ( NAREAS + 1 ) = nstn + 1
C
C*	Get any integer plotting range check values from the table
C
	DO jj = 1, 9
	    DO ii = 1, 2
		isnd ( ii, jj ) = 0
	    END DO
	END DO
	nsnds = 0
C
	DO ii = 1, nstn
	    IF ( iarea ( ii ) .eq. IUSGEC ) THEN
      		IF ( bktbch ( ii ) (:2 ) .ne. ' ' ) THEN
		    CALL ST_NUMB ( bktbch ( ii ) ( :2 ), isndx, ier )
                    IF ( ier. eq. 0 ) THEN 
		        n1 = isndx / 10
		        n2 = MOD ( isndx, 10 )
		        IF ( ibkseq ( ii ) .gt. 0 ) THEN
		            IF ( ( n1 .ge. 1 ) .and. 
     +			         ( n1 .le. 9 ) .and.
     +			         ( ( n2 .eq. 1 ) .or. ( n2 .eq. 2 ) ) )
     +			           isnd ( n2, n1 ) = ibkseq ( ii )
                            IF ( nsnds .lt. n1 ) nsnds = n1
		        END IF
		    END IF
		END IF
                IF ( bkstate(ii) .eq. 'ME' ) iseqme = ibkseq(ii)
	    END IF
C
C*          Save sequence number for last california breakpoint in
C*          section IPACIF
C
	    IF ( iarea ( ii ) .eq. IPACIF ) THEN
                IF ( bkstate(ii) .eq. 'CA' ) iseqca = ibkseq(ii)
	    END IF
	END DO
C
        CALL FL_CLOS ( lun, ier ) 
C*
	RETURN
	END
