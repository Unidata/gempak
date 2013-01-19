	SUBROUTINE MR_MISS  ( nlev, stndat, iret )
C************************************************************************
C* MR_MISS								*
C*									*
C* This subroutine replaces missing thermodynamic and wind data by	*
C* interpolating linearly with respect to log pressure.  The data	*
C* must be ordered from the surface to the top of the atmosphere	*
C* before this subroutine is called.					*
C*									*
C* MR_MISS  ( NLEV, STNDAT, IRET )					*
C*									*
C* Input parameters:							*
C*	NLEV		INTEGER		Number of levels		*
C*									*
C* Input and output parameters:						*
C*	STNDAT (6,NLEV)	REAL		Station data			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/86						*
C* M. desJardins/GSFC	 8/90	Eliminate dewpoints which appear after	*
C*				being missing for 100 mb		*
C* K. Brill/NMC		01/92	Don't eliminate dewpoints until after	*
C*				the first interpolation			*
C* S. Jacobs/NMC	 3/95	Eliminate interpolation of winds from	*
C*				below 100 mb to above 100 mb.		*
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *
C*                              DATA statement                          *
C* D. Kidwell/NCEP	 2/01   Remove check on first interpolation for *
C*				dewpoints; allow wind interp above 100mb*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		stndat (6,*)
	LOGICAL		intflg (5), angflg (5), found, more
	REAL		outdat (5)
	INCLUDE		'ERMISS.FNC'
C*
	DATA		intflg / 5*.true. /, 
     +			angflg / 3*.false.,.true.,.false./
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Loop through the interpolator twice, first for thermodynamic
C*	data and then for wind data.  Then add a final pass to 
C*	interpolate dewpoint where temperature is present.
C
	DO  iknt = 1, 3
	    iabove = 0
	    IF  ( iknt .eq. 1 ) THEN
		i2 = 2
		i3 = 3
	      ELSE IF  ( iknt .eq. 2 ) THEN
		i2 = 4
		i3 = 5
	      ELSE
		i2 = 3
		i3 = 3
	    END IF
C
C*	    Loop through all the levels.
C
	    i = 2
	    more = .true.
	    DO WHILE  ( ( i .le. nlev-1 ) .and. more )
C
C*		Check for missing data.
C
		IF  ( ERMISS ( stndat ( i2, i ) ) )  THEN
C
C*		    Find data at level above.  Data should already
C*		    be at level just below this.
C
		    IF  ( iabove .le. i ) THEN
			iabove = i + 1
			found  = .false.
			DO WHILE ( .not. found )
			    IF ( .not. ERMISS ( stndat (i2, iabove) ) )
     +								   THEN
				found = .true.
			      ELSE
				iabove = iabove + 1
				IF  ( iabove .gt. nlev ) THEN
				    found = .true.
				    iabove = 0
				    more   = .false.
				END IF
			    END IF
			END DO
		    END IF
C
C*		    Add extra check to eliminate dewpoints above 100
C*		    mb.
C
		    IF ( ( iknt .eq. 3 ) .and. ( iabove .ne. 0 ) ) THEN
			IF  ( ( stndat (1,i) - stndat (1,iabove) ) .gt.
     +				100. )  THEN
			    DO  j = iabove, nlev
			        stndat (3,j) = RMISSD
			    END DO
			    iabove = 0
			    more   = .false.
			END IF
		    END IF
C
C*		    Add check to eliminate interpolation of winds
C*		    from below 100 mb to above 100 mb. This eliminates
C*		    interpolation to very high level winds.
C
		    IF  ( ( iknt .eq. 2 ) .and. ( iabove .ne. 0 ) .and.
     +			  ( stndat ( 1, i-1 ) .gt. 100. ) .and.
     +			  ( stndat ( 1, iabove ) .lt. 100. ) ) THEN
			iabove = 0
		    END IF
C
C*		    If a level above this was found, interpolate with
C*		    respect to log p.
C
		    IF ( iabove .ne. 0 ) THEN
			DO  ijk = 1, 5
			    outdat ( ijk ) = RMISSD
			END DO
			CALL PC_INTP ( stndat (1,i), stndat (1,i-1),
     +			    stndat ( 1, iabove ), 5, intflg, angflg, 
     +			    outdat, iret )
			stndat ( i2, i ) = outdat (i2)
			stndat ( i3, i ) = outdat (i3)
		    END IF
		END IF
		i = i + 1
	    END DO
	END DO
C*
	RETURN
	END
