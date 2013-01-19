	PROGRAM GDCFIL
C************************************************************************
C* GDCFIL								*
C*									*
C* This program creates a new grid file.				*
C*									*
C* Log: 								*
C* I. Graffman/RDS	 6/88						*
C* M. desJardins/GSFC	 8/88	Lots of changes				*
C* G. Huffman/GSC	 4/89	Grid navigation table input		*
C* M. desJardins/GSFC	 5/89	Error messages; init ihd, anlblk	*
C* K. Brill/GSC          4/90   Added ANLYSS for ANLBLK input		*
C* K. Brill/NMC          7/90   Changes for table file listing		*
C* S. Schotz/GSC	10/90	Eliminated invalid error message	*
C* M. desJardins/NMC	 4/91	Eliminated xspace, yspace		*
C* J. Whistler/SSAI	 5/91	Added error message for large grids	*
C* K. Brill/NMC		02/92	Use LLNANL, LLNNAV			*
C* K. Brill/NMC		02/92	Use CPYFIL analysis block only when no	*
C*			   	analysis block is specified.		*
C* L. Williams/EAi      03/94   Clean up declarations of user input	*
C*				variables				*
C* L. Williams/EAI	 7/94	Removed call to GDCUPD			*
C* K. Tyle/GSC		 8/96	Added FL_MFIL to search for file type	*
C* S. Maxwell/GSC	 7/97	Increased input character length 	*
C* T. Lee/GSC		 7/99	Checked CPYFIL				*
C* D.W.Plummer/NCEP	 3/00	Added subarea option to CPYFIL		*
C* S. Jacobs/NCEP	 3/01	Changed FL_MFIL to FL_MFLS		*
C* K. Brill/HPC		11/02	Check KX * KY against LLMXTG		*
C* M. Li/SAIC		04/04	set ihd = LLGDHD			*
C* R. Tian/SAIC          3/05   Changes for time/file mngmnt            *
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	gdfile*(LLMXLN), proj*(LLMXLN), cpyfil*(LLMXLN),
     +			gdarea*(LLMXLN), anlyss*(LLMXLN), kxky*(LLMXLN),
     +		 	maxgrd*(LLMXLN)
C*
	LOGICAL		respnd, done, proces
C-----------------------------------------------------------------------
C*	Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'GDCFIL', ier )
	    done = .false.
	  ELSE
	    done = .true.
	END IF
	IF  ( .not. done )  THEN
C
C*      Initialize grid library common area grdcmn.cmn
C
            CALL GD_INIT  ( iret )
C
C*      Initialize GEMPLT in order to set grid navigation later.
C
	    CALL GG_INIT  ( 1, iret )
	    IF  ( iret .ne. 0 )  THEN
		iperr = -3
		done  = .true.
	    END IF
	END IF
C
C*	Create new grid files.
C
	DO WHILE  ( .not. done ) 
C
C*	    Get user input.
C
	    CALL GDCINP  ( gdfile, proj, gdarea, kxky, maxgrd, cpyfil,
     +			   anlyss, iperr )
	    IF  ( iperr .ne. 0 )  THEN
	        done   = .true.
		proces = .false.
	      ELSE
	        proces = .true.
	    END IF
C
C*	    Check that output file name is not blank.
C
	    IF  ( proces )  THEN
		IF  ( gdfile .eq. ' ' )  THEN
		    iret = -7
		    CALL ER_WMSG  ( 'GDCFIL', iret, gdfile, ier )
		    proces = .false.
		END IF
	    END IF
C
C*	    Give user option to exit.
C
	    IF  ( proces )  THEN
		CALL GDCDSP  ( gdfile, proj, gdarea, kxky, maxgrd,
     +			       cpyfil, anlyss, iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Create the grid file.
C
	    IF  ( proces )  THEN
		CALL GDGCFL  ( gdfile, proj, gdarea, kxky, maxgrd,
     +				cpyfil, anlyss, iret )
	        IF ( iret .ne. 0 ) THEN
		    CALL ER_WMSG  ( 'GDCFIL', iret, gdfile, ier )
		END IF
	    END IF
C
C*	    Call dynamic tutor.
C
	    CALL IP_DYNM  ( done, ier )
	END DO
C
C*	Final error messages.
C
	IF  ( iperr .ne. 0 )  CALL ER_WMSG ( 'GDCFIL', iperr, ' ', ier )
	CALL IP_EXIT  ( iret )
C*
	END
