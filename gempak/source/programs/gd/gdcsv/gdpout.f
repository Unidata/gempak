	SUBROUTINE GDPOUT ( nlun, luns, devs, grid, kx, ky,
     +				ksubx, ksuby, iret )
C************************************************************************
C*									*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		grid ( kx, ky )
	INTEGER		nlun, luns (*), kx, ky, ksubx(*), ksuby(*)
	CHARACTER	devs (*), outstr(LLMXLN)
C*
	REAL		xs, ys, lats(LLMXGD), lons(LLMXGD)

	iret = 0
C*
	np = 1
	DO j = 1, ky
	    ys = float(j)
	    DO k = 1, kx
		xs = float(k)
		iv = (j - 1) * kx + k
		CALL GTRANS ( 'G', 'M', np, xs, ys, lats(iv), lons(iv), 
     +			   iret )
		DO i = 1, nlun
C
C*	            Loop through each output device.
C
		    IF ( grid(k, j) .ne. RMISSD ) THEN
                        write ( luns(i), 1000 ) ksubx(1) + k - 1, 
     +			    ksuby(1) + j - 1, lats(iv), lons(iv), 
     +			    grid(k, j)
1000                    FORMAT ( 2(I8,','), 2(F15.4,','), F15.5 )
		    END IF 
		END DO
	    END DO
	END DO
C
	RETURN
	END
