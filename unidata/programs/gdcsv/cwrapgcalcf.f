	SUBROUTINE CWRAP_GCALCF ( timfnd, glevel, gvcord, gfunc, 
     +			grid, ksubx, ksuby, scale, nlun, luns, devs, 
     +			iret)
C
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	timfnd, glevel, gvcord, gfunc, devs(*),
     +			scale
	REAL		grid(*)
	INTEGER		ksubx(*), ksuby(*), nlun, luns(*), iret
C
	CHARACTER*(LLMXLN) pfunc, parm
	CHARACTER	time(2)*20
	INTEGER		lev(2)

        CALL DG_GRIDN (timfnd, glevel, gvcord, gfunc,
     +  		pfunc, grid, kx, ky, time,
     +  		lev, ivc, parm, iret)
C
        IF (iret .eq. 0) THEN
	    CALL IN_SCAL (scale, iscals, iscalv, iret)
	    CALL GR_SSCL (iscals, kx, ky, 1, 1, kx,
     +  	ky, grid, rmin, rmax, iret)
C 
	    CALL GDPOUT ( nlun, luns, devs, grid, kx, ky,
     +  		ksubx, ksuby, iret )
        ELSE
            CALL ER_WMSG ( 'DG', iret, ' ', ier )
        END IF


	RETURN
	END
