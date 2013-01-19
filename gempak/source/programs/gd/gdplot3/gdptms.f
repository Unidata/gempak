	SUBROUTINE GDPTMS ( gdatim, gdfile, cycle, maxt,
     +			    ntimes, timfnd, iret )
C************************************************************************
C* GDPTMS								*
C*									*
C* This subroutine creates an array of times for use by the subroutine	*
C* GDPLTB.  GDATIM and GDFILE may each be !-parseable input strings.	*
C* The number of times returned, NTIMES, is determined (and limited )	*
C* by the number of times found from the first !-entry of GDFILE.  	*
C* If other !-entries from GDFILE have fewer times, those extra time 	*
C* slots will be null.							*
C*									*
C* GDPTMS ( GDATIM, GDFILE, CYCLE, MAXT, NTIMES, TIMFND, IRET )		*
C*									*
C* Input parameters:							*
C*	GDATIM		CHAR*		GEMPAK date/time string		*
C*	GDFILE		CHAR*		GEMPAK grid file name		*
C*	CYCLE		CHAR*		Cycle reference time		*
C*	MAXT		INTEGER		Maximum number of times allowed	*
C*									*
C* Output parameters:							*
C*	NTIMES		INTEGER		Number of times in animation	*
C*					    sequence			*
C*	TIMFND(MAXTMS)	CHAR*	 	Array of times			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*                                       -1 = error in processing GDATIM*
C*                                      -20 = no times selected         *
C*                                      -29 = file open failure         *
C*                                      -30 = error opening file        *
C*                                      -31 = navigation not the same   *
C*                                      -33 = too many files to open    *
C*                                      -34 = more than one output file *
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	10/96	New for gdplot.				*
C* D.W.Plummer/NCEP	 2/97	Parse gdfile; length check for GDATIM	*
C* D.W.Plummer/NCEP	 7/97	Increased MAXTMS from 50 to 500		*
C* D.W.Plummer/NCEP	 8/98	Changed calling sequence		*
C* D.W.Plummer/NCEP	 5/00	Changes for cycle processing		*
C* T. Lee/GSC		 7/00	Increased MAXTMS & array size for cycles*
C* S. Jacobs/NCEP	 5/01	Increased time string lengths		*
C* R. Tian/SAIC         12/04   Changes for time/file mngmnt            *
C************************************************************************
 	INCLUDE		'GEMPRM.PRM'
        INCLUDE         'gdplot.cmn'
C*
	PARAMETER	( MAXGDT = 256 )
	PARAMETER	( MAXTMS = LLMXGT )
	CHARACTER	gdt*256
C*
	CHARACTER*(*)	gdatim, gdfile, timfnd(*), cycle
C
	CHARACTER	gdatmb*128, timtmp(MAXTMS)*40, trange*36
	CHARACTER	substr*40
	LOGICAL		proces, done
	CHARACTER	gd(MAXB)*128
C*
C-----------------------------------------------------------------------
C
	iret = 0
C
	CALL ST_LSTR ( gdatim, lgda, iret )
	IF ( lgda .gt. MAXGDT )  THEN
	    iret = -1
	    RETURN
	END IF
C
	CALL ST_LCUC ( gdatim, gdt, iret )
	CALL ST_RMBL ( gdt, gdt, lgda, iret )
	CALL GDINST ( gdfile, '<', MAXB, .true., gd, ngd, iret )
C
	proces = .true.
	ntimes = 0
C
	substr = ' '
	lss = 0
C
C*	First fill in any '()' strings with a substring value.
C
	iptr = 1
	ifound = INDEX ( gdt(iptr:), '(' )
	ilp = ifound + iptr - 1
	DO WHILE ( proces .and. ifound .ne. 0 )
	    irp = INDEX ( gdt(ilp:), ')' ) + ilp - 1
	    IF ( irp .le. ilp )  proces = .false.
	    IF ( irp .eq. ilp+1 )  THEN
		IF ( lss .le. 0 )  proces = .false.
		CALL ST_LSTR ( gdt, lgda, iret )
		itot = lgda - ( irp - ilp - 1 ) + lss
		IF ( itot .gt. MAXGDT )  THEN
		    iret = -1
		    RETURN
		END IF
		IF ( proces )  THEN
		   gdt = gdt(:ilp) // substr(:lss) // gdt(irp:)
		   iptr = ilp + lss + 1
		END IF
	      ELSE
		substr = gdt(ilp+1:irp-1)
		CALL ST_LSTR ( substr, lss, iret )
		iptr = irp + 1
	    END IF
	    ifound = INDEX ( gdt(iptr:), '(' )
	    ilp = ifound + iptr - 1
	END DO
C
	DO  n = 1, maxt
	    timfnd ( n ) = ' '
	END DO
C
C*	Add parentheses if user has omitted them when using a bang.
C
	ifndlp = INDEX ( gdt, '(' )
	ifndbn = INDEX ( gdt, '!' )
	IF ( ifndlp .eq. 0  .and.  ifndbn .ne. 0 )  THEN
		CALL ST_LSTR ( gdt, lgda, iret )
		itot = lgda + 2
		IF ( itot .gt. MAXGDT )  THEN
		    iret = -1
		    RETURN
		END IF
		gdt = "(" // gdt(:lgda) // ")"
	END IF
C
C*	For each bang, build string and send to GR_TMFL
C
	done = .false.
	DO  nb = 1, MAXB
C
	    IF ( .not. done )  THEN
	        CALL GDPBAN ( gdt, nb, MAXB, gdatmb, nbs, iret)
	        done = iret .ne. 0 .or. 
     +			( nb .gt. nbs .and. nb .gt. ngd )
	    END IF
C
	    IF ( .not. done )  THEN
C
C*          Get times.
C*
		CALL DG_NFIL ( gd(nb), ' ', ier1 )
		CALL DG_NDTM ( gdatmb, ier2 )
		CALL DG_QTMS ( MAXTMS, .true., timtmp, nt, trange, ier3 )
                proces = .not. ( ier1 .ne. 0 .or. ier2 .ne. 0 .or.
     +                           ier3 .ne. 0 .or. nt .le. 0 )
C
	        IF ( proces )  THEN
		    IF ( nb .eq. 1 )  ntimes = nt
		    DO  k = 1, ntimes
			IF ( k .le. maxt )  THEN
		          CALL ST_LSTR ( timfnd(k), ltf, iret )
			  IF ( k .gt. nt )  THEN
		            IF ( ltf .ne. 0 )  THEN
		              timfnd(k) = timfnd(k)(:ltf) // '!'
		              ltf = ltf + 1
		            END IF
			  ELSE
		            CALL ST_LSTR ( timtmp(k), ltt, iret )
		            IF ( ltf .ne. 0 )  THEN
		              timfnd(k) = timfnd(k)(:ltf) // '!'
		              ltf = ltf + 1
		            END IF
		            timfnd(k) = timfnd(k)(:ltf)//timtmp(k)(:ltt)
		          END IF
		        END IF
		    END DO
		ELSE
		    done = .true.
	        END IF
C
	    END IF
C
        END DO
C
	IF ( .not. proces .and. iret .eq. 0 )  iret = -1
C
	RETURN
C
	END
