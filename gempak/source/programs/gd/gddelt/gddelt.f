	PROGRAM  GDDELT
C************************************************************************
C* PROGRAM GDDELT							*
C*									*
C* This program deletes grids from GEMPAK grid files.			*
C**									*
C* Log:									*
C* M. Goodman/RDS	10/85						*
C* M. desJardins/GSFC	 7/87	Fixed for GEMPAK4			*
C* M. desJardins/GSFC	 8/88	Updated 				*
C* K. Brill/NMC		02/92	Use LLNNAV, LLNANL			*
C* L. Williams/EAI       3/94   Clean up declarations of user input	*
C*				variables				*
C* L. Williams/EAI	07/94	Removed call to GDDUPD			*
C* D. McCann/AWC	 8/95	Added delete by grid id			*
C* K. Tyle/GSC		12/95	Integrated into environment		*
C* K. Tyle/GSC		 8/96	Added FL_MFIL to search for file type	*
C* S. Maxwell/GSC	10/96	Removed call to GDPNAM, changed GDMERR	*
C* 				to DG_MERR; added call to GDDDEL to	*
C*				match grids				*
C* S. Jacobs/NCEP	12/96	Added check for error opening the file	*
C*				before deleting grids			*
C* S. Maxwell/GSC     	7/97    Increased input character length        *
C* R. Tian/SAIC          3/05   Changes for time/file mngmnt            *
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	gdfile*(LLMXLN), gvcord*(LLMXLN), glevel*(LLMXLN)
	CHARACTER	gdatim*(LLMXLN), filnam*(LLMXLN), gfunc*(LLMXLN)
C*
	CHARACTER	fsttm*20, lsttm*20
	LOGICAL		respnd, done, proces
C-----------------------------------------------------------------------
C*	Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'GDDELT', ier )
C
C*      Initialize grid library common area grdcmn.cmn
C
	    CALL GD_INIT  ( ier )
C
C*      Initialize the DG library.
C
	    CALL DG_INTL ( ier )
	    done = .false.
	ELSE
	    done = .true.
	END IF
C
C*	Main processing loop.
C
	DO WHILE  ( .not. done )
C
C*	    Set flag to indicate processing will be done.
C
	    proces = .true.
C
C*	    Read in the variables from the TAE.
C
	    CALL GDDINP  ( gdfile, gvcord, glevel, gdatim, 
     +	  		   gfunc, iperr )
	    IF  ( iperr .ne. 0 )  THEN
C
C*	        Exit if there is an error.
C
		done = .true.
	      ELSE
C
C*		'gdfile' has to be an actual grid file.
C
		CALL FL_INQR ( gdfile, proces, filnam, ier )
		IF ( .not. proces ) THEN
		    iret = -8
		    CALL ER_WMSG ( 'GDDELT', iret, gdfile, ier )
		END IF
C
C*	        Open the grid file if no error.
C
		IF ( proces ) THEN
		    CALL GD_OPEN ( filnam, .true., 0, 0, iflno, adum1,
     +                             adum2, maxgrd, ier )
		    CALL GD_NGRD ( iflno, numgrd, fsttm, lsttm, ier )
		    IF ( ier .ne. 0 ) THEN
		        proces = .false.
		      ELSE IF ( numgrd .lt. 1 ) THEN
		        ierr = -5
		        CALL ER_WMSG  ( 'GDDELT', ierr, filnam, ier )
		        proces = .false.
		    END IF
		END IF
C
		IF ( proces ) THEN
		    CALL GDDDEL ( iflno, numgrd, gdfile, gdatim,
     +		                  glevel, gvcord, gfunc, ier )
		END IF
C
C*		Prompt for next set of grids to be deleted.
C
		CALL IP_DYNM  ( done, ier )
	    END IF
	END DO
C
C*	Print general error messages if necessary.
C
	IF (iperr .ne. 0) CALL ER_WMSG ( 'GDDELT', iperr, ' ', ier )
	CALL IP_EXIT  ( iret )
C*
	END
