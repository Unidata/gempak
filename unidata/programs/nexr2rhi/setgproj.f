	SUBROUTINE	SET_GPROJ ( clat, clon, nhxs, dhorz, iret )

	REAL	clat, clon, dhorz
	INTEGER	nhxs
	CHARACTER	proj*4
	LOGICAL	angflg

C
C*	Set projection for cartesian grid centered on Radar location
C
        proj  = 'CED'
        angl1 = clat
        angl2 = clon
        angl3 = 0.0
        angflg = .true.
C
C*      Compute the lat/lon of LL, UR points.
C
        alat = clat - ( ( nhxs - 1 ) * dhorz / 2.0) / 111.111
        alon = clon - ( ( nhxs - 1 ) * dhorz / 2.0) /
     +                          (111.111*COS(DTR*clat))
        blat = clat + ( ( nhxs - 1 ) * dhorz / 2.0) / 111.111
        blon = clon + ( ( nhxs - 1 ) * dhorz / 2.0) /
     +                          (111.111*COS(DTR*clat))

C
C*      Set the GRID projection.
C
        CALL GSGPRJ(proj, angl1, angl2, angl3,
     +              nhxs, nhxs, alat, alon, blat, blon, iret)
C
	IF ( iret .ne. 0 ) THEN
	    CALL ER_WMSG ( 'SET_GPROJ', iret, ' ', ier )
	END IF

	RETURN
	END

	SUBROUTINE	SET_CXSTNS ( cxstns )

	INCLUDE	'GEMPRM.PRM'

	CHARACTER*(*) 	cxstns
	CHARACTER	endpts*(LLMXLN)

	COMMON  / DCCMN /
     +		endpts

	endpts = cxstns

	RETURN
	END

	SUBROUTINE	GET_CXPTS ( rgx, rgy, rlt, rln, iret )

	INCLUDE 'GEMPRM.PRM'

	REAL	rgx(*), rgy(*), rlt(*), rln(*)
	CHARACTER	endpts*(LLMXLN), cxpts(2)*(LLMXLN)
        COMMON  / DCCMN /
     +          endpts

	iret = 0

	igt = INDEX ( endpts, '>' )
        IF ( igt .eq. 0 ) THEN
            iret = -4
            CALL ER_WMSG  ( 'NEXR2RHI', iret, ' ', ier )
        ELSE
            CALL ST_CLST ( endpts, '>', ' ', 2, cxpts, nums, ier )
            DO i = 1, 2
                CALL GR_PLOC ( cxpts (i), rgx(i), rgy(i), rlt(i), 
     +			rln(i), iret )

                IF ( iret .ne. 0 ) THEN
                    CALL ER_WMSG('GR', iret, ' ', ierr)
                END IF
            END DO
        END IF

	RETURN
	END
