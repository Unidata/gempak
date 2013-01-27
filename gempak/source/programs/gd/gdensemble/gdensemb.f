	PROGRAM  GDENSEMBLE
C************************************************************************
C* GDENSEMBLE								*
C*									*
C* This program computes ensemble statistics			*
C*									*
C**									*
C* Log:									*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	gdfile*72, gdoutf*72
C*
	REAL		rnvblk(LLNNAV), anlblk(LLNANL)
	REAL		rnvblk2(LLNNAV), anlblk2(LLNANL)
	INTEGER		igdfln, navsz, ianlsz, ihdrsz, maxgrd, iret
	INTEGER		igdfln2, navsz2, ianlsz2, ihdrsz2, maxgrd2, ier
	LOGICAL		respnd, done, proces
C-----------------------------------------------------------------------
C*	Initialize TAE and GEMPLT.
C*	Note that GEMPLT is only used to translate grid coordinates.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    mode = 1
	    CALL GG_INIT  ( mode, iperr )
	END IF
	IF  ( iperr .eq. 0 )  THEN
	    done = .false.
	  ELSE
	    done = .true.
	END IF
	CALL IP_IDNT  ( 'GDENSEMB', ier )
C
C*	Main loop to read in TAE parameters and list data.
C
	DO WHILE  ( .not. done )
C
C*	  Set flag to indicate processing will be done.
C
	  proces = .true.
C
C*	  Read in the variables from the TAE.
C
	  CALL GDLINP  ( gdfile)
C
C*	  Exit if there is an error
C
	  IF  ( iperr .ne. 0 )  THEN
	    done = .true.
	   ELSE
C
C*	    Open the grid files
C
	    CALL GD_OPNF(gdfile,.true.,igdfln,navsz,rnvblk,ianlsz,
     +                   anlblk,ihdrsz,maxgrd,iret)
	    IF  ( iret .ne. 0 )  THEN
		proces = .false.
	    END IF
C
	    if(proces) then
               CALL GDENSTAT(igdfln,navsz,rnvblk,ier) 
	       CALL GD_CLOS(igdfln,iret)
	    endif
C
C*	    Prompt for next listing to be done.
C
	    CALL IP_DYNM  ( done, ier )
	  END IF
	END DO
C
C*	Print general error messages if necessary.
C
	IF  (iperr .ne. 0)  CALL ER_WMSG ( 'GDENSEMB', iperr, ' ', ier )
C
C*	Exit from GEMPLT and the TAE.
C
	CALL GENDP  ( 0, iret )
	CALL IP_EXIT  ( iret )
C*
	END
