	SUBROUTINE IP_STRP  ( progrm, iret )
C************************************************************************
C* IP_STRP	                                    			*
C*									*
C* This subroutine receives the name of the program being executed	*
C* and reads the program's .PDF file in GEMPDF to make a list of the	*
C* variables to be used.  If a variable from the PDF file is also	*
C* in GEMGLB.NTS, then a flag is set showing it is being used.		*
C* Otherwise, the variable is added to the global list.			*
C*									*
C* IP_STRP  ( PROGRM, IRET )						*
C*									*
C* Input parameters:							*
C*	PROGRM		CHAR*		Program name			*
C*									*
C* Output parameters:                                                   *
C*	IRET		INTEGER 	Return code			*
C*				   	  0 = normal return		*
C*					-10 = unable to open PDF file	*
C**	                                                                *
C* Log:									*
C* D. Moore/UOK-GCN	 6/89						*
C* M. desJardins/GSFC	 7/89	Modified for GEMPAK 5			*
C* M. desJardins/GSFC	11/89	Eliminate tabs to simplfy code		*
C*				Fix // for Apollo			*
C* M. desJardins/GSFC	 4/90	Error messages				*
C* M. desJardins/GSFC	10/90	Added DISPLAY function			*
C* K. Brill/NMC		 7/91	Change GEMEXE to GEMPDF			*
C* M. desJardins/NMC	 1/92	Look for PDF in local account first	*
C* S. Jacobs/NMC         2/94   Added file name return to FL_INQR       *
C* K. Tyle/GSC		 7/96	Renamed from NT_STRP			*
C* S. Jacobs/NCEP	 7/96	Changed environ vars to "$.../" format	*
C* T. Lee/GSC		11/98	Simplified PDF I/O; Removed TAE		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER*(*)	progrm
C*
	LOGICAL		end, found, exist
	CHARACTER	cp1*8, ch1*32 , fff*72, newfil*132
C-----------------------------------------------------------------------
	iret   = 0
	firstt = .true.
	ihead  = 0
	ilast  = 0
C
C*	Make the name of the .PDF and check to see if the file exists
C*	in the user's account.
C
	CALL ST_LSTR  ( progrm, lenp, ier )
	fff = progrm ( :lenp ) // '.PDF'
	CALL FL_INQR  ( fff, exist, newfil, ier )
	IF  ( .not. exist )  THEN
	    fff = '$GEMPDF/' // progrm ( :lenp) // '.PDF'
	    CALL FL_INQR  ( fff, exist, newfil, ier )
	END IF
C
	IF  ( exist )  THEN	
	    CALL FL_SOPN  ( fff, lun, ier )
	  ELSE
	    iret = -10
	    CALL ER_WMSG  ( 'IP', iret, progrm, ier )
	    RETURN
	END IF
C
C*	Read the PDF file to end-of-file.
C
	end    = .false.
	DO WHILE  ( .not. end )
C
C*	    Read the next line.
C
	    READ   ( lun, 1000, IOSTAT = iostat )  cp1, ch1
1000	    FORMAT ( A, A )
C
	    IF  ( iostat .ne. 0 ) THEN
		end = .true.
	      ELSE
C
C*		Search the global variable list to see if the 
C*		variable from the PDF file is there.  If it is, 
C*		set flag to indicate it is used in this program.
C
		found = .false.
		ip    = 1
		DO WHILE  ( (.not. found) .and. (ip .le. ncparm) )
		    IF  ( cparmn (ip) .eq. cp1 )  THEN
			IF  ( ihead .eq. 0 )  THEN
			    ihead = ip
			    ilast = ip
			  ELSE
			    iplink ( isave ) = ip
			    ilast = ip
			END IF
			isave = ip
			found = .true.
			chelp1 ( ip ) = ch1
		    END IF
		    ip = ip + 1
		END DO
C
C*		If the variable was not found in the global variable
C*		list, then add it if possible.
C
		IF  ( .not. found )  THEN
	    	    IF  ( ( ncparm .lt. MXIPPM ) .and. 
     +			  ( cp1 .ne. ' ' ) )  THEN
			ncparm = ncparm + 1
			cparmn ( ncparm ) = cp1
			cparmv ( ncparm ) = ' '
			chelp1 ( ncparm ) = ch1
			IF  ( ihead .eq. 0 )  THEN
			    ihead = ncparm
			    ilast = ncparm
			  ELSE
			    iplink ( isave ) = ncparm
			    ilast = ncparm
			END IF
			isave = ncparm
		    END IF
		END IF
	    END IF
	END DO
C
C*	Close the PDF file
C
	CALL FL_CLOS  ( lun, ier )
C*
	RETURN
	END
