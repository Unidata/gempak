	SUBROUTINE IP_EXIT  ( iret )
C************************************************************************
C* IP_EXIT								*
C*									*
C* This subroutine must be called to exit from the interface.  Current	*
C* variable values are written to the file containing global values.	*
C*									*
C* IP_EXIT  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C*				    	  0 = normal return		*
C*					 -1 = unable to open GEMGLB.NTS	*
C*				   	 -4 = error writing to file	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88						*
C* M. desJardins/GSFC	 4/90	Open file here instead of in NT_DFLT	*
C* K. Brill/NMC		 8/90	Add call to close GEMPAK files		*
C* K. Tyle/GSC		 7/96	Renamed from NT_EXIT			*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C------------------------------------------------------------------------
	iret   = 0
C
C*	Open the GEMGLB.NTS file.
C
	CALL FL_SWOP  ( IPFIL, iplun, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -1
	    CALL ER_WMSG  ( 'IP', iret, ' ', ier )
	    RETURN
	  ELSE
C
C*	    Write all the records to the global file.
C
	    DO  i = 1, ncparm
		WRITE  ( iplun, 1000, IOSTAT = iostat ) cparmn (i),
     +						    cparmv (i)
1000		FORMAT ( A, A )
		IF  ( iostat .ne. 0 )  iret = -4
	    END DO
C
C*	    Close the file.
C
	    CALL FL_CLOS  ( iplun, ier )
	END IF
C
C*	Write error message telling user that file was not updated.
C
	IF  ( iret .ne. 0 )  CALL ER_WMSG  ( 'IP', iret, ' ', ier )
C
C*	Close all GEMPAK files that might be open.
C
	CALL FL_CLAL ( ier )
C 
C*
	RETURN
	END
