	SUBROUTINE GINITP  ( mode, istat, iret )
C************************************************************************
C* GINITP								*
C* 									*
C* This subroutine initializes GEMPLT for an application program.  	*
C* It must be called before any other GEMPLT subroutine.  The GPLT      *
C* subprocess is begun if it has not been started by a previously       *
C* executed program.  MODE specifies whether map or graph plots are     *
C* to be made.				                                *
C*									*
C* GINITP  ( MODE, ISTAT, IRET )					*
C*									*
C* Input parameters:							*
C* 	MODE		INTEGER		Plot mode 			*
C*	 				   0 = no change		*
C*					   1 = map coordinates		*
C*					   2 = graph coordinates	*
C* 									*
C* Output parameters:							*
C* 	ISTAT		INTEGER		Status code			*
C* 					   0 = GPLT started 		*
C* 					   1 = GPLT previously started	*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 4/85	GEMPLT Version 3.1                      *
C* M. desJardins/GSFC	 5/88	Documentation				*
C* I. Graffman/RDS	 6/88	Clean up				*
C* M. desJardins/NMC	 7/91	UNIX version				*
C* M. desJardins/NMC	 1/92	Make UNIX & VMS calls identical		*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'ADBUFF.CMN'
C*
	INTEGER		isend ( 3 ), irecv ( 2 )
	CHARACTER	image*48 
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Get executable name.
C
	image = 'gplt'
C
C*	Start the subprocess.
C
	mproc = 0
	CALL GSPROC  ( mproc, image, mbchan, istat, iret )
C
C*	Return if there is an error.  Otherwise, initialize common area
C*	for messages.
C
	IF  ( iret .ne. 0 )  THEN
	    iret = NOPROC
	    RETURN
	  ELSE 
	    ntypsr = 0
	    irtype = 2
	    iwtype = 1
	END IF
C
C*	Load input parameters into buffer and write to the mailbox.
C
	isend ( 1 ) = 3
	isend ( 2 ) = FINITP	
	isend ( 3 ) = mode
C*
	CALL GPUT  ( isend, 3, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.  Ignore istat returned and use istat
C*	from above.
C                   
	CALL GGET  ( irecv, 2, ier )
	IF  ( ier .ne. NORMAL )  THEN
	    iret = ier
	  ELSE
	    iret = irecv ( 1 )
	END IF
C*
	RETURN
	END
