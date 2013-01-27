	SUBROUTINE SNDHTR ( pres, temp, dwpt )
C************************************************************************
C* SNDHTR								*
C*									*
C* This routine alters the surface layers by diabatic heating.		*
C*									*
C* SNDHTR ( PRES, TEMP, DWPT )						*
C*									*
C* Input/Output parameters:						*
C*	PRES		INTEGER		Pressure			*
C*	TEMP		INTEGER		Temperature			*
C*	DWPT		INTEGER		Dew point			*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92						*
C* J. Whistler/SSAI	 4/93		Cleaned up header		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		pres(*), temp(*), dwpt(*)
C*
	INTEGER		sbllmt, sbltop
C------------------------------------------------------------------------
C*	SBL thickness limit.
C
	sbllmt = 30
C
C*	Check if a surface layer inversion exists.
C*	If so destroy it via surface heating and mixing.
C
	IF  ( temp(1) .le. temp(2) ) THEN
C
C*	    Surface layer inversion does exists.
C
	    sbltop = pres(1) - sbllmt
	    j = 2
	    DO WHILE ( pres(j) .ge. sbltop .and.
     +		       temp(j) .le. temp(j+1) )
		j = j + 1
	    END DO
C
C*	    NOW DESTROY ADIABATICALLY:
C
	    j = j - 1
	    numlev = j
	    DO  WHILE ( j .gt. 0 )
C
		p  = FLOAT ( pres(j) )
		pp = FLOAT ( pres(j+1) )
		t  = FLOAT ( temp(j+1) )
C
C*		Change temp.
C
		theta   = t * (p/pp) ** RKAPPA
		tdiff   = theta - FLOAT ( temp(j) )
		temp(j) = INT ( theta )
C
C*		Warm dewpoint a fraction of the temperature increase.
C
		dwpt(j) = dwpt(j) + INT ( tdiff / (j+1) )
		j = j - 1
	    END DO
	END IF
C*
	RETURN
	END	
