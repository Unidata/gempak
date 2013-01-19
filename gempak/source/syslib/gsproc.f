	SUBROUTINE GSPROC  ( mproc, image, mbchan, istat, iret )
C************************************************************************
C* GSPROC								*
C* 									*
C* This subroutine creates a subprocess and a mailbox to use for	*
C* for communications.  It attempts to make a unique name for the	*
C* process and the mailbox based on the interactive terminal name.	*
C* The process being started should call GIPROC to attach itself 	*
C* to the communication channel.					*
C* 									*
C* GSPROC  ( MPROC, IMAGE, MBCHAN, ISTAT, IRET )			*
C* 									*
C* Input parameters:							*
C*	MPROC		INTEGER		Subprocess type			*
C*					  0 = gplt			*
C*					  1 = device driver		*
C* 	IMAGE		CHAR*		Executable name			*
C* 									*
C* Output parameters:							*
C* 	MBCHAN		INTEGER		Message queue number		*
C* 	ISTAT		INTEGER		Process active flag		*
C* 					  = 0  process started 		*
C* 					  = 1  process existed		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 4/82						*
C* G. Chatters/RDS	 3/84	Use spawn instead of create process	*
C* M. Vilardo/RDS	 3/85	Simplified for GEMPLT Version 3		*
C* M. desJardins/GSFC	 4/85						*
C* M. desJardins/GSFC	 5/88						*
C* M. desJardins/NMC	 1/92	Make VMS & UNIX calls identical		*
C* S. Jacobs/NMC         2/94   Added file name return to FL_INQR       *
C* S. Jacobs/NMC	 9/94	Removed SS_ENVR and ST_UCLC		*
C* S. Jacobs/NCEP	 7/96	Changed environ vars to "$.../" format	*
C* T. Piper/SAIC	11/07	Changed GEMEXE to OS_BIN		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	image
C*
	LOGICAL		exist
	CHARACTER	prcnam*72, newfil*132
C*
	INTEGER		ccheck, csproc
C------------------------------------------------------------------------
	iret = 0
C
C*	First check to see if the process is running.
C
	ier = ccheck  ( mproc, mbchan, istat, iret )
C
C*	If process is found, return.
C
	IF  ( istat .eq. 1 )  THEN
	    RETURN
	END IF
C
C*	If process is not found, build correct name.
C
	prcnam = image
	CALL FL_INQR  ( prcnam, exist, newfil, ier )
	IF  ( .not. exist )  THEN
	    prcnam = '$OS_BIN/' // image
	    CALL FL_INQR  ( prcnam, exist, newfil, ier )
	END IF
C
C*	If process cannot be found, return with error.
C
	IF  ( .not. exist )  THEN
	    iret = NOPROC
	    RETURN
	END IF
C
C*	Call C module to install process.
C
	CALL ST_NULL ( newfil, newfil, lenp, ier )
	ier = csproc ( mproc, newfil, mbchan, iret )
C*
	RETURN
	END
