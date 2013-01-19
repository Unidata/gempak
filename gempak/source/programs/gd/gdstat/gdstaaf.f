      SUBROUTINE GDSTAAF ( kx, ky,
     +                     glevel, gvcord, gfunc, grdnam,
     +                     grid, sums, sumsq, gmax, gmin, cnt,
     +                     iret )
C************************************************************************
C* GDSTAAF                                                              *
C*                                                                      *
C* This subroutine reads the grid file, computes statistics,            *
C* and writes them to the output grid file.                             *
C*                                                                      *
C*   (CALL) GDSTAAF ( KX, KY, GLEVEL, GVCORD, GFUNC, GRDNAM,     *
C*  +                 GRID, SUMS, SUMSQ, GMAX, GMIN, CNT, IRET )        *
C*                                                                      *
C* Input parameters:                                                    *
C*      KX              INTEGER         Number of points in x dir       *
C*      KY              INTEGER         Number of points in y dir       *
C*      GLEVEL          CHAR*           Grid level parameter setting    *
C*      GVCORD          CHAR*           Grid vertical coordinate        *
C*      GFUNC           CHAR*           Scalar grid                     *
C*      GRDNAM          CHAR*           Grid parameter name             *
C*                                                                      *
C* Work parameters (allocated and freed in calling routine):            *
C*      GRID(*)         REAL            Work array for grid             *
C*      SUMS(*)         REAL            Work array for sums of grids    *
C*      SUMSQ(*)        REAL            Work array for sums of squares  *
C*      GMAX(*)         REAL            Work array for maxima           *
C*      GMIN(*)         REAL            Work array for minima           *
C*      CNT(*)          REAL            Work array for counts           *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* B. Hebbard/SAIC      03/08           "Original", with thanks to      *
C*                                      M. Li for similar GDEDITF       *
C*                                      See gdstat.f for history        *
C*                                      before this file was splif off  *
C************************************************************************

        INCLUDE         'GEMPRM.PRM'
C*
        CHARACTER*(*)   glevel, gvcord, gfunc, grdnam
        REAL            grid (*), sums (*), sumsq (*)
        REAL            gmax (*), gmin (*), cnt (*)
        LOGICAL         proces, first, gottm
        CHARACTER       parm*12, pfunc*72
        CHARACTER       dattim (2)*20, timfnd*36, timout (2)*20
        INTEGER         level (2)
C
C*
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------

C
C*          Initialize grids.
C
            DO  i = 1, KX * KY
                sums  (i) = RMISSD
                sumsq (i) = RMISSD
                cnt   (i) = RMISSD
                gmax  (i) = -1.1E31
                gmin  (i) = 1.1E31
            END DO
C
C*          Loop over times.
C
            itime = 1
            igrid = 0
            proces = .true.
            gottm  = .true.
C
            DO  WHILE ( gottm )
                first = ( itime .eq. 1 )
C
C*              Get the next time to process from time server.
C
                CALL DG_NTIM ( .true., .false., dattim, gottm, ier )
                IF ( gottm .and. ier .eq. 0 ) THEN
                    CALL TG_DUAL ( dattim, timfnd, ier )
C
                    CALL DG_GRIDN  ( timfnd, glevel, gvcord, gfunc,
     +                               pfunc, grid, kx, ky, dattim, level,
     +                               ivcord, parm, ier )
                    IF ( first ) THEN
                        kxsv = kx
                        kysv = ky
                      ELSE
                        IF ( kx .ne. kxsv .or. ky .ne. kysv ) THEN
                            ier = -8
                            CALL ER_WMSG ( 'GDSTAT', ier, ' ', irr )
                            proces = .false.
                        END IF
                    END IF
                    IF  ( ier .eq. 0 )  THEN
                        igrid = igrid + 1
C
C*                      Get output times.
C
                        IF ( first )  THEN
                            timout (1) = timfnd
                          ELSE
                            timout (2) = timfnd
                        ENDIF
C
C*                      Loop through all the points.
C
                        DO  j = 1, kx * ky
C
C*                          Check to see if data point is missing.
C
                            IF  ( .not. ERMISS ( grid (j) ) )  THEN
                                IF  ( ERMISS ( cnt (j) ) )  THEN
                                    sums  (j) = grid (j)
                                    sumsq (j) = grid (j) ** 2
                                    cnt   (j) = 1.
                                    IF ( grid (j) .gt. gmax (j) ) THEN
                                        gmax (j) = grid (j)
                                    END IF
                                    IF ( grid (j) .lt. gmin (j) ) THEN
                                        gmin (j) = grid (j)
                                    END IF
                                  ELSE
                                    sums  (j) = sums  (j) + grid (j)
                                    sumsq (j) = sumsq (j) + grid(j) ** 2
                                    cnt   (j) = cnt   (j) + 1.
                                    IF ( grid (j) .gt. gmax (j) ) THEN
                                        gmax (j) = grid (j)
                                    END IF
                                    IF ( grid (j) .lt. gmin (j) ) THEN
                                        gmin (j) = grid (j)
                                    END IF
                                 END IF
                            END IF
                        END DO
                      ELSE IF ( proces ) THEN
                        CALL ER_WMSG  ( 'DG', ier, pfunc, ier1 )
                    END IF
                    itime = itime + 1
                END IF
            END DO
            itime = itime - 1
C
C*          Check that there were at least four grids.
C*          (Otherwise standard deviation across them is undefined.)
C
            IF  ( proces .and. ( igrid .lt. 4 ) )  THEN
                iret = -6
                CALL ER_WMSG  ( 'GDSTAT', iret, ' ', ier )
                proces = .false.
            END IF
C
C*          Calculate statistics.
C
            IF  ( proces )  THEN
                DO  j = 1, kx * ky
                    knt = NINT ( cnt (j) )
                    IF  ( ( knt .eq. 0 ) .or.
     +                    ( knt .eq. NINT ( RMISSD ) ) )  THEN
                        sums  (j) = RMISSD
                        sumsq (j) = RMISSD
                        gmax (j) = RMISSD
                        gmin (j) = RMISSD
                      ELSE IF  ( knt .le. 4 )  THEN
                        sums  (j) = sums  (j) / cnt (j)
                        sumsq (j) = RMISSD
                      ELSE IF  ( knt .gt. 1 )  THEN
                        sums  (j) = sums  (j) / cnt (j)
                        sumsq (j) = SQRT ( sumsq (j) / cnt (j) -
     +                                     sums  (j) ** 2 )
                    END IF
                END DO
            END IF
C
C*          Write grids to output file.
C
            IF  ( proces )  THEN
                CALL GDYWRT  ( gmax, gmin, sums, sumsq, cnt, kx, ky,
     +                         timout, level, ivcord, parm, grdnam,
     +                         itime, igrid, iret )
            END IF
C*
          RETURN
          END
