	SUBROUTINE IP_VERS  ( iret )
C************************************************************************
C* IP_VERS								*
C*									*
C* This subroutine displays the version number of the installed		*
C* software.								*
C*									*
C* IP_VERS  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return 		*
C**									*
C* Log:									*
C* S. Jacobs/NMC	 9/95						*
C* S. Jacobs/NCEP	 1/96		Update to 5.3			*
C* S. Jacobs/NCEP	 5/96		Update to 5.3.1			*
C* S. Jacobs/NCEP	 5/96		Update to 5.4			*
C* K. Tyle/GSC		 7/96		Renamed from NT_VERS		*
C* S. Jacobs/NCEP	 8/96		Update to 5.4.1			*
C* S. Jacobs/NCEP	12/96		Update to 5.4.2			*
C* S. Jacobs/NCEP	 2/97		Changed to call SS_VERS		*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER	vmsg*72
C------------------------------------------------------------------------
	iret = 0
C
C*	Get the version string.
C
	CALL SS_VERS ( vmsg, ier )
C
C*	Write the version number to the terminal.
C
	WRITE ( 6, 1000 ) vmsg
1000	FORMAT ( / ' GEMPAK ', A / )
C*
	RETURN
	END
