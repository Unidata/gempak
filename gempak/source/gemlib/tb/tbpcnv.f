	SUBROUTINE TB_PCNV  ( tblnam, maxfnc, nfunc, parms, funcs,
     +			      prmin, iret )
C************************************************************************
C* TB_PCNV								*
C*									*
C* This subroutine reads in the parameter-function computation table	*
C* and decomposes it into functions and required parameters.  If the	*
C* function table cannot be opened or is too large for the buffer	*
C* space, an error message is written, but no error is returned.	*
C*									*
C* TB_PCNV  ( TBLNAM, MAXFNC, NFUNC, PARMS, FUNCS, PRMIN, IRET )	*
C*									*
C* Input parameters:							*
C*	TBLNAM		CHAR*		Parameter conversion table	*
C*	MAXFNC		INTEGER		Maximum number of functions	*
C*									*
C* Output parameters:							*
C*	NFUNC		INTEGER		Number of functions		*
C*	PARMS (NFUNC)	CHAR*		Computable functions		*
C*	FUNCS (NFUNC)	CHAR*		Function names			*
C*	PRMIN (4,NFUNC)	CHAR*		Input parameters to funcs	*
C*	IRET		INTEGER		Return code			*
C*				  	   0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/88						*
C* S. Jacobs/EAI	 3/93		Added tblnam to calling seq.	*
C* D. Keiser/GSC	12/95		Changed FL_TOPN to FL_TBOP	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
C*
	CHARACTER*(*)	tblnam, parms (*), funcs (*), prmin (4,*)
C*
	CHARACTER	string*80, ppp*24, fff*24, ppff (4)*24
	LOGICAL		good, done
C-----------------------------------------------------------------------
	iret  = 0
	nfunc = 0
C
C*	Open the parameter conversion table.
C
	CALL FL_TBOP  ( tblnam, 'parms', lun, ierr )
	IF  ( ierr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'FL', ierr, tblnam, ier )
	    RETURN
	END IF
C
C*	Loop through file reading functions.
C
	iostat = 0
	DO WHILE  ( iostat .eq. 0 )
C
C*	    Get the next record.
C
	    string (1:1) = '!'
	    DO  WHILE  ( string (1:1) .eq. '!' )
C
C*		Read the next record.
C
		READ   ( lun, 1000, IOSTAT = iostat )  string
1000		FORMAT ( A )
C
C*		Check for read error.
C
		IF  ( iostat .ne. 0 )  THEN
		    string = ' '
		END IF
	    END DO
C
C*	    Parse string if this is not the end of file.
C
	    good = .false.
	    IF  ( iostat .eq. 0 )  THEN
C
C*		Remove blanks and search for = sign.
C
		good = .true.
		CALL ST_RMBL  ( string, string, length, ier )
		ipeq = INDEX ( string, '=' )
C
C*		If there is no = sign, skip record.
C
		IF  ( ipeq .eq. 0 )  THEN
		    good = .false.
C
C*		    Get the parameter.
C
		  ELSE
		    ppp    = string ( 1 : ipeq-1 )
		    string = string ( ipeq+1 : )
C
C*		    Search for the "(" which terminates function.
C
		    iplp = INDEX  ( string, '(' )
		    IF  ( iplp .eq. 0 )  THEN
			good = .false.
		      ELSE
			fff    = string ( 1 : iplp-1 )
			string = string ( iplp+1 : )
C
C*			Loop finding input parameters for function.
C
			knt  = 0
			done = .false.
			DO WHILE  ( .not. done )
			    ipcom = INDEX ( string, ',' )
			    IF  ( ipcom .eq. 0 )  THEN
				ipcom = INDEX ( string, ')' )
				IF  ( ipcom .eq. 0 )  THEN
				    good = .false.
				  ELSE
				    done = .true.
				END IF
			    END IF
			    knt = knt + 1
			    IF  ( knt .gt. 4 )  THEN
				good = .false.
				done = .true.
			      ELSE
				ppff (knt) = string ( 1 : ipcom-1 )
				string = string ( ipcom+1 : )
			    END IF
			END DO
		    END IF
		END IF
	    END IF
C
C*	    Add function to table checking that there is room.
C
	    IF  ( good )  THEN
		IF  ( nfunc .eq. MAXFNC )  THEN
		    ierr = -6
		    CALL ER_WMSG  ( 'TB', ierr, ' ', ier )
		    iostat = -1
		  ELSE
		    nfunc = nfunc + 1
		    parms ( nfunc ) = ppp
		    funcs ( nfunc ) = fff
		    DO  i = 1, 4
			IF  ( i .le. knt )  THEN
			    prmin (i,nfunc) = ppff (i)
			  ELSE
			    prmin (i,nfunc) = ' '
			END IF
		    END DO
		END IF
	    END IF
	END DO
C
C*	Close the table.
C
	CALL FL_CLOS  ( lun, ier )
C*
	RETURN
	END
