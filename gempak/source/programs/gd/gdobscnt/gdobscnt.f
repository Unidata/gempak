	PROGRAM GDOBSCNT
C************************************************************************
C* GDOBSCNT								*
C*									*
C* This program creates a grid containing the number of observations	*
C* within a specified radius of a grid point.				*
C**									*
C* Log:									*
C* Chiz/Unidata		10/01	Initial coding				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(LLMXLN)	gdfile, filnam, sffile, cradius, dattim
C*
	CHARACTER	gdattim(2)*20, stid*8, sffcur*(LLMXLN),
     +			parm*12, cprj*10
C*
	CHARACTER	prmdst (MMPARM)*4
	CHARACTER	timlst (LLMXTM)*15, timfnd (LLMXTM)*15,
     +			timout*(LLMXLN)
                       
C*
	INTEGER		ivcord, nbits, ighdr(LLGDHD), level(2), kx, ky,
     +			kxky
C*
	LOGICAL		respnd, done, proces, newfil, exist
C*	
	REAL		rnvblk(LLNNAV), anlblk(LLNANL), grid(LLMXGD), 
     +			qmin, qmax, rdif, rbits
C*
	REAL		rlat(LLMXGD), rlon(LLMXGD), dist(LLMXGD)
C
C-----------------------------------------------------------------------
C*	Initialize user interface
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'IP', iperr, ' ', ier )
	    CALL SS_EXIT
	END IF
C
	done = .false.
C
	CALL IP_IDNT ( 'GDOBSCNT', ier )
C
C	
	DO  WHILE  ( .not. done )
C
	    proces = .true.
C
C*	    Get input parameters.
C
	    CALL GPINP  ( sffile, dattim, gdfile, cradius, iperr)

	    IF  ( iperr .eq. 0 )  THEN
C
C*		Convert radius to a real number
C
		CALL ST_C2R (cradius, 1, dist, inum, iret )
		IF (iret .eq. 0) THEN
		   frad = dist(1)
		ELSE
		   frad = RMISSD
		END IF
C
C*		Open the surface file
C
		sffcur = ' '
		CALL FL_MFIL ( sffile, ' ', filnam, iret )
		IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )
                CALL SFLFIL  ( filnam, sffcur, isflno, newfil, iflsrc,
     +                     prmdst, npmdst, ier )
                IF  ( ier .ne. 0 )  proces = .false.
C
C*              Get the navigation and analysis blocks from the
C*              existing file.
C
		CALL FL_MFIL ( gdfile, ' ', filnam, ier )
		IF ( proces .and. (ier .eq. 0)) THEN
		    CALL GD_OPNF ( filnam, .true., iflno, inav, rnvblk,
     +                             ianl,  anlblk, ihd, maxg, ier )
		    IF ( ier .eq. 0) THEN
			CALL GR_RNAV  ( rnvblk, cprj, kx, ky, ier )
			CALL GR_SNAV  ( inav, rnvblk, ier )
			CALL GR_LTLN ( kx, ky, rlat, rlon, ier )
			kxky = kx*ky
		    ELSE
			proces = .false.
		    END IF
C
		    IF (proces) THEN
C
C*		       Get list of times in the surface file
C
		       CALL SF_GTIM ( isflno, LLMXTM, ntime, timlst, ier )
C
C*		       Get times in user range
C
		       CALL TI_FIND ( dattim, ntime, timlst, timout,
     +			  	      ntout, timfnd, ier)
C
C*		       Create a grid for every time in the surface file
C
		       DO i=1,ntout
C
C*	    		   Initialize grid to 0 at every point
C
	    		   DO j=1,LLMXGD
	       		      grid(j) = 0
	    		   END DO
C
C*			   Save count range for grid packing
		           qmin = 0
			   qmax = 0

			   write(*,*) 'Processing ',timfnd(i)
C
C*			   Set reference time in surface file
C
			   CALL SF_STIM ( isflno, timfnd(i), ier )
C
C*			   Loop over each station at this time
C
			   DO WHILE (ier .eq. 0)
C
				CALL SF_SNXT ( isflno, stid, istnm, slat, 
     +					slon, selv, ispri, ier )
C
				exist = .false.
				IF ( ier .eq. 0 )
     +				   CALL SF_QDAT ( isflno, exist, ier )
				IF ( exist ) THEN
				   IF ( slat .eq. RMISSD ) exist = .false.
				   IF ( slon .eq. RMISSD ) exist = .false.
				END IF
C
				IF ( ( ier .eq. 0 ) .and. exist ) THEN
C
C*				   Compute the distance from every
C*				   grid point to this station
C
				   CALL CLO_DIST ( slat, slon, kxky, 
     +				      rlat, rlon, dist, ierr )
C
C*				   Increment count for each grid point
C*				   within the desired radius
C
			           DO j=1,kxky
				      IF ( ( dist(j) .lt. frad ) .and.
     +					   ( dist(j) .ge. 0))  THEN
				         grid(j) = grid(j) + 1
				         IF (grid(j) .gt. qmax ) 
     +					     qmax = grid(j)
				         END IF
				   END DO
				END IF
			   END DO

			   DO icnt = 1,LLGDHD
			      ighdr(icnt) = 0
			   END DO
                           gdattim(1) = timfnd(i)
                           gdattim(2) = ' '
                           level(1) = 0
                           level(2) = -1
                           ivcord = 0
                           parm = 'ODEN'

C
C*		           Compute number of packing bits
C
		           IF ( ( qmin .eq. RMISSD ) .or. 
     +			      ( qmax .eq. RMISSD ) ) THEN
		              nbits = 16
		           ELSE
		               rdif = qmax - qmin
			       rbits = abs ( alog ( rdif ) ) / alog ( 2.0 )
			       nbits = int(rbits) + 1
		           END IF

		           IF ( ( nbits .lt. 2) .or. ( nbits .gt. 32 ) ) THEN
                               CALL GD_WPGD(iflno, grid, kx, ky, ighdr, 
     +		     	       gdattim, level, ivcord, parm, .true., MDGNON,
     +                         nbits, ier)
		           ELSE
                               CALL GD_WPGD(iflno, grid, kx, ky, ighdr, 
     +			       gdattim, level, ivcord, parm, .true., MDGGRB,
     +                         nbits, ier)
		           END IF
		           IF ( ier .ne. 0 ) THEN
			      CALL ER_WMSG  ( 'GD', ier, ' ', ier )
		           END IF
		       END DO
		    END IF
		    CALL GD_CLOS(iflno, ier)
C
		END IF
		CALL SF_CLOS(isflno, ier)
	    END IF
C
C*	    Call the dynamic tutor.
C
      	    CALL IP_DYNM ( done, iret )
	END DO
C*
	IF  ( iperr .ne. 0 )  CALL ER_WMSG  ( 'IP', iperr, ' ', ier )
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
