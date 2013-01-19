	SUBROUTINE GG_SPLT2 ( itxcol, mcolr, mtype, sizem, mwid, 
     +				stnfil, iret )
C************************************************************************
C* GG_SPLT: Modified for efficiency
C*									*
	INCLUDE		'GEMPRM.PRM'
	character stnfil*(*)

	CHARACTER	stid(LLSTFL)*8, stnnam(LLSTFL)*32,
     +			stat(LLSTFL)*2, coun(LLSTFL)*2, tbchrs*14
	INTEGER		istnm(LLSTFL), ispri(LLSTFL)
	REAL		rlat(LLSTFL), rlon(LLSTFL), relv(LLSTFL), rot
	LOGICAL		done
C------------------------------------------------------------------------
	iret  = -1
	CALL FL_TBOP ( stnfil, 'stns', lun, ier )
	IF ( ier .ne. 0 ) return
	i = 0
	done = .false.
	DO WHILE ( ( i .lt. LLSTFL ) .and. ( .not. done ) )
	    i = i + 1
	    CALL TB_RSTN ( lun, stid (i), stnnam (i), istnm (i),
     +			   stat (i), coun (i), rlat (i), rlon (i),
     +			   relv (i), ispri (i), tbchrs, iret )
	    IF ( iret .ne. 0 ) done = .true.
	END DO
	nstns = i - 1
	CALL FL_CLOS ( lun, iret )
	ixoff = 0
	IF ( mcolr .ne. 0 ) THEN 
	  CALL GSMRKR (mtype,2,sizem,mwid,ier)
          CALL GSCOLR ( mcolr, iret )
	  CALL GMARK ( 'M', nstns, rlat, rlon, iret )
          ixoff = 2 
        end if
	IF ( itxcol .ne. 0 ) THEN
	    CALL GSCOLR ( itxcol, iret )
	    rot   = 0.0
	    iyoff = 0
	    DO i = 1, nstns
C                IF ( stid(i)(1:1) .eq. ' ' ) 
C     +			CALL ST_INCH(istnm(i),stid(i),iret)
		CALL ST_LSTR ( stnnam(i), ls, ier )
C		IF ( mcolr .eq. 0 ) ixoff = -max(ls-1,0)
		CALL GTEXT ( 'M', rlat(i), rlon(i), stnnam(i)(:ls), 
     +		rot, ixoff, iyoff, iret )
	    END DO
	END IF
	iret = 0
	RETURN
	END

