	PROGRAM SFDSL604
C************************************************************************
C* PROGRAM SFDSL604							*
C*	This program lists AIRWAYS data in a prescribed format.		*
C*									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK4 version				*
C* M. desJardins/GSFC	10/87	Added skip missing data flag		*
C* M. desJardins/GSFC	 6/88	Added keynam				*
C* M. desJardins/GSFC	11/89	Added conditions			*
C* M. desJardins/GSFC	 4/90	Replaced LC_UARE by SF_UARE		*
C* J. Whistler/SSAI	 5/91	Changed output*24 to output*48		*
C* S. Jacobs/EAI	 9/92	Added call to ST_LCUC for input time	*
C* L. Williams/EAI	 3/94	Clean up declarations of user input	*
C*				variables				*
C* L. Williams/EAI	 7/94	Removed call to SFL6UP			*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
C* K. Tyle/GSC		 8/96	Added ER_WMSG call after FL_MFIL call	*
C* K. Tyle/GSC		 1/97	Call ER_LMSG; eliminate TAE references	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* T. Piper/SAIC	 4/02	Fixed UMR; initialized iflno & aaa	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	file*(LLMXLN), output*(LLMXLN), area*(LLMXLN),
     +			ddd*(LLMXLN), keynam*(LLMXLN), sfparm*(LLMXLN)
	LOGICAL		skpmis
C*
	CHARACTER	sffile*72, times (LLMXTM)*20
	CHARACTER	aaa*48, dattim*48, stn*8, filnam*72
	CHARACTER	sfpold*72, outdev (4)*1 
	LOGICAL 	respnd, done, proces, cmpflg (MMPARM), newfil
	INTEGER		luns (4) 
	DATA		output/' '/, area/' '/, dattim/' '/, 
     +			sffile/' '/, sfpold/' '/, aaa/' '/
C------------------------------------------------------------------------
C*	Initialize GEMPAK variable block.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    done = .false.
	  ELSE
	    done = .true.
	END IF
	CALL IP_IDNT  ( 'SFDSL604', ier )
C
	iflno = 0
	DO WHILE  ( .not. done )
C
C*	    Read in variables from the interface.
C
	    CALL SFL6IN  ( file, output, area, ddd, skpmis, keynam, 
     +			   sfparm, iperr )
	    IF  ( iperr .lt. 0 )  THEN
		done = .true.
	      ELSE
		proces = .true.
C
C*		Open file and check whether file has changed.
C
	    	CALL FL_MFIL ( file, ' ', filnam, iret )
		IF ( iret .ne. 0 ) 
     +		     CALL ER_LMSG ( 0, 'FL', iret, ' ', ier )    
		CALL SFL6FL  ( filnam, sffile, iflno, newfil, iret )
		IF  ( iret .ne. 0 )  proces = .false.
C
C*		Check for new parameter conditions and set IDNTYP.
C
		IF  ( sfparm .ne. sfpold )  newfil = .true.
		sfpold = sfparm
C*
		CALL ST_LCUC  ( keynam, keynam, ier )
		IF  ( keynam .ne. 'STNM' )  keynam = 'STID'
C
C*		Set parameters and conditions to be computed.
C
		IF  ( proces .and. newfil )  THEN
		    CALL SFL6PM  ( sfparm, cmpflg, iret )
		    IF  ( iret .ne. 0 )  proces = .false.
		END IF
C*
		IF  ( proces )  
     +		    CALL IN_OUTT  ( output, 'SFDSL604', luns, nlun, 
     +				    outdev, ier )
C*
		IF  ( proces )  THEN
		    CALL SF_UARE  ( iflno, area, newfil, aaa, stn, ier )
		    IF  ( ier .ne. 0 )  proces = .false.
		END IF
C*
		IF  ( proces )  THEN
		    CALL ST_LCUC ( ddd, ddd, ier )
		    CALL SFL6TM ( ddd, newfil, dattim, iflno, 
     +				  ntime, times, iret )
		    IF  ( iret .ne. 0 )  proces = .false.
		END IF
C*
		IF ( proces ) THEN
		    CALL SFL6PR  ( iflno, cmpflg, nlun, luns, times, 
     +				   ntime, skpmis, keynam, ier )
		END IF
		CALL IP_DYNM  ( done, iret )
	    END IF
	END DO
	IF  (iperr .ne. 0)  
     +	    CALL ER_LMSG  ( 0, 'SFDSL604', iperr, ' ', ier )
	CALL IP_EXIT  ( iret )
	END
