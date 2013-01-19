	SUBROUTINE CWRAP_GCALCF ( timfnd, glevel, gvcord, gfunc,
     +                  grid, kx, ky, scale, gintp, rgx, rgy, yy, iret)
C
	INCLUDE		'GEMPRM.PRM'

	CHARACTER*(*)	timfnd, glevel, gvcord, gfunc, scale
	REAL		grid(*), rgx, rgy, yy(*)
	INTEGER		iret
	LOGICAL		gintp
C
	CHARACTER*(LLMXLN) pfunc, parm
        CHARACTER       time(2)*20
        INTEGER         lev(2)

	CALL DG_GRIDN (timfnd, glevel, gvcord, gfunc,
     +                  pfunc, grid, kx, ky, time,
     +                  lev, ivc, parm, iret)

C
        IF (iret .eq. 0) THEN
            CALL IN_SCAL(scale,iscals,iscalv,iret)
            CALL GR_SSCL(iscals,kx,ky,1,1,kx,ky,grid,
     +      	rmin, rmax, iret)
C
            IF ( gintp ) THEN
                CALL GR_INTP(1,rgx,rgy,1,kx,ky,grid,yy, iret)
            else
                yy(1) = grid(5)
            END IF
	END IF
C
	RETURN
	END
