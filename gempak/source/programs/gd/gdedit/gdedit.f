	PROGRAM GDEDIT
C************************************************************************
C* GDEDIT								*
C*									*
C* This program reads a grid edit file and replaces the grids in the	*
C* grid file with the edited grids.					*
C**									*
C* Log:									*
C* M. Goodman/RDS	11/85						*
C* M. desJardins/GSFC	 7/87	Fixed					*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* M. desJardins/GSFC	 3/89	Added grid packing			*
C* L. Williams/EAI	 7/94	Removed call to GDEUPD			*
C* K. Tyle/GSC		 8/96	Added FL_MFIL to search for file type	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* J. Wu/GSC             7/00   Moved INCLUDE 'ERMISS.FNC' before the   *
C*                              DATA statement                          *
C* R. Tian/SAiC		 3/05	Changes for file mngmnt            	*
C* T. Lee/SAIC		12/05	Initialized ighdr			*
C* M. Li/SAIC		11/07   Added GDEDAT to remove grid limit	*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	gdefil*(LLMXLN), gdfile*(LLMXLN), gpack*(LLMXLN)
C*
	CHARACTER	filnam*72, proj*20
	REAL		rnav (LLNNAV)
	LOGICAL		respnd, done, proces, exist
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Initialize TAE. 
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'GDEDIT', ier )
C
C*      Initialize grid library common area grdcmn.cmn.
C 
	    CALL GD_INIT  ( ier )
	    done = .false.
	  ELSE
	    done = .true.
	END IF
C
C*	Main loop to read in TAE parameters and edit grid file.
C
	DO WHILE  ( .not. done )
C
C*	    Set flag to indicate processing will be done.
C
	    proces = .true.
C
C*	    Get input variables from the TAE.
C
	    CALL GDEINP  ( gdefil, gdfile, gpack, iperr )
C
C*	    Exit if there is an error.
C
	    IF  ( iperr .ne. 0 )  THEN
	        done = .true.
	      ELSE
C
C*		Translate the packing information.
C
		CALL GR_PACK  ( gpack, ipktyp, nbits, ier )
C
C*		Open the grid file and get the x and y dimensions of 
C*		the grid.
C
		CALL FL_INQR ( gdfile, exist, filnam, iret )
		IF ( .not. exist ) CALL ER_WMSG ( 'FL', iret, ' ', ier )    
		CALL GD_OPEN ( filnam, .true., 0, LLNNAV, iflno, adum,
     +                         rnav, maxgrd, iret )
		IF ( iret .ne. 0 ) THEN
		    iret = -8
		    CALL ER_WMSG  ( 'GDEDIT', iret, filnam, ier )
		    proces = .false.
		END IF
		CALL GR_RNAV  ( rnav, proj, kx, ky, ier )
C
C*		Open the edit file.
C
		IF ( proces ) THEN
		    CALL FL_SOPN  ( gdefil, lun, iret )
		    IF  ( iret .ne. 0 )  THEN
			CALL ER_WMSG  ( 'FL',  iret, gdefil, ier )
			proces = .false.
			CALL ER_WMSG  ( 'GDEDIT', -7, gdefil, ier )
		    END IF
		END IF
C
C*		Read and process information and grid data
C
		IF ( proces ) THEN
                    iproc = 1
                  ELSE
                    iproc = 0
		END IF
		CALL GDEDAT ( iproc, lun, iflno, ipktyp, nbits,
     +                       kx, ky, iret )
C
C*		Close edit file.
C
		CALL FL_CLOS  ( lun, iret )
C
C*		Call the dynamic tutor.
C
	        CALL IP_DYNM  ( done, iret )
	    END IF
	END DO
C
C*	Print general error message.
C
	IF ( iperr .ne. 0 ) CALL ER_WMSG ( 'GDEDIT', iperr, ' ', ier )
C
C*	Exit from the TAE.
C
	CALL IP_EXIT  ( iret )
C*
	END
