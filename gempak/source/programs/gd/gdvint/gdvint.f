	PROGRAM GDVINT
C************************************************************************
C*	GDVINT								*
C*									*
C* Main program to do general vertical interpolation of gridded data.	*
C*									*
C* Log:									*
C* K. Brill/NMC		06/92						*
C* K. Brill/NMC		10/92	Added VCOORD input for sfc VC's		*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* G. Hull/SAIC		04/08   25.21.4.7-Remove Grid size limit        *
C************************************************************************
C	INCLUDE         'GEMPRM.PRM'
	INCLUDE         'vicmn.cmn' 
	LOGICAL		exit
C------------------------------------------------------------------------
	ierr = 0
C
C*	Start the program.
C
	CALL VG_STRT ( ier )
C
C*	Begin program loop.
C
	exit = .false.
	DO WHILE ( .not. exit )
C
C*	    Get user input and put into cmn block.
C
	    IF ( ier .eq. 0 ) THEN
	    	CALL VG_USIN ( ier )
	    ELSE
	    	ierr = ier
	    END IF
C
C*	    Open the input and output files.
C
	    IF ( ier .eq. 0 ) THEN
	    	CALL VG_OPEN ( ier )
	    ELSE
	    	ierr = ier
	    END IF
C
C*          Initialize the internal arrays. (Do this now instead of 
C*          from VI_DRIVF since it will set nli, nlo, np)
C
	    IF ( ier .eq. 0 ) THEN
	        CALL VI_INIT( ier )
	    ELSE
	    	ierr = ier
	    END IF
C
C*	    Do the vertical interpolation. This will alloc the 
C*          cmn_data memory and call the fortran VI_DRIV
C
	    IF ( ier .eq. 0 ) THEN
	    	CALL VI_DRIVC ( (kxin*kyin), kxky, nli, nlo, np, ier )
	    ELSE
	    	ierr = ier
	    END IF
C
C*	    Print out an error message.
C
	    CALL VG_IERR ( ierr, ier )
C
C*	    Request to exit or continue.
C
	    CALL VG_EXIT ( exit, ier )
C*
	END DO
C*
	END
