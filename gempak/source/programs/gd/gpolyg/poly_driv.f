	SUBROUTINE POLY_DRIV ( nt, time, grid, iret )
C************************************************************************
C* POLY_DRIV	  							*
C*									*
C* This subroutine groups wind data based on warning categories.  The	*
C* distance between the groups are defined by DISTNM in poly_parm.tbl.	*
C* The outline of each individual group is made of a warning polygon.	*
C* This polygon is clipped by a bound file to create CAP message.	*
C*									*
C* POLY_DRIV ( NT, TIME, GRID, IRET )					*
C*									*
C* Input parameters:							*
C*	NT		INTEGER		Nth grid time			*
C*	TIME		CHAR*		Grid time			*
C*	GRID(LLMXGD)	REAL		Gridded wind data		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*									*
C**									*
C* Log:									*
C* T. Lee/SAIC		 2/08						*
C************************************************************************
	INCLUDE		'gpolyg.cmn'
	INCLUDE		'ERROR.PRM'
	CHARACTER*(*)	time (2)
	REAL		grid (igxd,igyd)
	REAL		rlat (igxd,igyd), rlon (igxd,igyd)
	CHARACTER	sys_G*2, sys_M*2
	LOGICAL		proces, first
	INCLUDE		'ERMISS.FNC'
	INCLUDE		'EQUAL.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
	proces = .true.
	first  = .false.
	sys_G  = 'G'
	sys_M  = 'M'
	CALL ST_NULL ( sys_G, sys_G, lens, ier )
	CALL ST_NULL ( sys_M, sys_M, lens, ier )
	CALL ST_LSTR ( time ( 1 ), lstr, ier )
C
C*	Store grid lat/lon from one dimension array to two dimension.
C
	first = ( nt. eq. 1 )
	IF ( first )  THEN
	    kk = 0
	    DO jj = 1, igyd
		DO ii = 1, igxd
		    kk = kk  + 1
		    rlat ( ii, jj ) = ggrlat ( kk )
		    rlon ( ii, jj ) = ggrlon ( kk )
		END DO
	    END DO
	END IF
C
	IF  ( proces )  THEN
	    DO kj = 1, igyd
                DO ki = 1, igxd
		    IF ( ERMISS ( grid ( ki, kj ) ) )  THEN
		      ELSE
			sped = PR_MSKN ( grid ( ki, kj ) )
C
C*			Save wind data in different groups based on the
C			warning categories: Gale, Storm and Hurricane.
C
			rr = rlat ( ki, kj )
			ss = rlon ( ki, kj )
			IF ( cflag ( sped ) )  THEN
			    CALL POLY_GROUP ( 4, rr, ss, ki, kj, ier )
			    IF ( ier .ne. 0 )  THEN
				CALL ER_WMSG ( 'gpolyg', ier, ' ', ierr)
				RETURN
			    END IF
			END IF
C
			IF ( gflag ( sped ) )  THEN
			    CALL POLY_GROUP ( 3, rr, ss, ki, kj, ier )
			    IF ( ier .ne. 0 )  THEN
				CALL ER_WMSG ( 'gpolyg', ier, ' ', ierr)
				RETURN
			    END IF
			END IF
C
			IF ( sflag ( sped ) )  THEN
			    CALL POLY_GROUP ( 2, rr, ss, ki, kj, ier )
			    IF ( ier .ne. 0 )  THEN
				CALL ER_WMSG ( 'gpolyg', ier, ' ', ierr)
				RETURN
			    END IF
			END IF
C
			IF ( hflag ( sped ) )  THEN
			    CALL POLY_GROUP ( 1, rr, ss, ki, kj, ier )
			    IF ( ier .ne. 0 )  THEN
				CALL ER_WMSG ( 'gpolyg', ier, ' ', ierr)
				RETURN
			    END IF
			END IF
                    END IF
		END DO
	    END DO
	END IF
C
C*	Create warning polygons.
C
	CALL POLY_WARN ( rlat, rlon, iret )
C*
	RETURN
	END
