	SUBROUTINE GDEDATF ( iproc, lun, iflno, ipktyp, nbits, 
     +			     kx, ky, grid, iret )
C************************************************************************
C* GDEDATF                                                              *
C*                                                                      *
C* This subroutine reads and processes information and data in the	*
*  edit file.								* 
C*                                                                      *
C* GDEDATF ( IPROC, LUN, IFLNO, IPKTYP, NBITS, KX, KY, GRID, IRET )	* 
C*                                                                      *
C* Input parameters:                                                    *
C*      IPROC           INTEGER         Process flag                    *
C*      LUN             INTEGER         Logical unit number             *
C*      IFLNO           INTEGER         File access number          	*
C*      IPKTYP          INTEGER         GEMPAK packing type             *
C*      NBITS           INTEGER         Number of bits              	*
C*      KX              INTEGER         Number of points in x dir  	*
C*      KY              INTEGER         Number of points in y dir  	*
C*                                                                      *
C* Work parameters:                                                     *
C*      GRID(*)         REAL            Work array for grid  		*
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* M. Li/SAIC           11/07                                           *
C************************************************************************

	INCLUDE		'GEMPRM.PRM'
C*
	REAL            grid (*)
C*
	CHARACTER	time (2)*20, parm*12
	INTEGER		level (2)
	LOGICAL		proces, found
	INTEGER		ighdr  ( LLGDHD )
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
		iret = 0
                DO ii = 1, LLGDHD
                    ighdr ( ii ) = 0
                END DO
C
C*              Read and process information in the edit file.
C
                found = .false.
                igrid = 1
                IF ( iproc .eq. 0 ) THEN
                    proces = .false.
                  ELSE
                    proces = .true.
                END IF
                DO WHILE  ( proces )
C
C*                  Read in the grid information and data.
C
                    CALL GDEGHD  ( lun, ncol, nrow, time, level,
     +                             ivcord, parm, iscale, iret )
                    IF  ( iret .ne. 0 )  THEN
                        IF  ( .not. found )  THEN
                            CALL ER_WMSG  ( 'GDEDIT', iret, ' ', ier )
                        END IF
                        proces = .false.
C
C*                      Check that the grid is the same size as the
C*                      grids in the file.
C
                      ELSE IF  ( ( nrow .ne. ky ) .or.
     +                           ( ncol .ne. kx ) )  THEN
                        iret = -5
                        CALL ER_WMSG  ( 'GDEDIT', iret, ' ', ier )
                        proces = .false.
                      ELSE
                        found  = .true.
                    END IF
C
C*		    Read in the grid data.
C
		    IF  ( proces )  THEN
			CALL GDEGDT  ( lun, kx, ky, grid, iret )
C
C*			Scale the data.
C
			IF  ( iret .ne. 0 )  THEN
			     CALL ER_WMSG  ( 'GDEDIT', iret, ' ', ier )
			     proces = .false.
			  ELSE IF  ( iscale .ne. 0 )  THEN
			    kxky = kx * ky
			    scale = 1. / ( 10. ** iscale )
			    DO  i = 1, kxky
				IF  ( .not. ERMISS ( grid (i) ) )  THEN
				    grid (i) = grid (i) * scale
				END IF
			    END DO
			END IF
		    END IF
C
C*		    Give the user a chance to exit.
C
		    IF  ( proces )  THEN
			CALL GDEDSP  ( time, level, ivcord, parm,
     +				       iexit )
		    END IF
C
C*		    Write the edit grid to the grid file.
C
		    IF  ( proces .and. ( iexit .eq. 0 ) )  THEN
			CALL GD_WPGD  ( iflno, grid, kx, ky, ighdr, 
     +					time, level, ivcord, parm, 
     +					.true., ipktyp, nbits, iret )
			IF  ( iret .ne. 0 )  THEN
			    iret = -7
			    CALL ER_WMSG  ( 'GDEDIT', iret, ' ', ier )
			  ELSE
			    igrid = igrid + 1
			END IF
		    END IF
                END DO
C*
		RETURN
		END
