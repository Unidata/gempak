	SUBROUTINE GSFRNT  ( ifcod, pipsz, ipipst, ipipdr, iret )
C************************************************************************
C* GSFRNT								*
C*									*
C* This subroutine sets the front attributes including the front code,	*
C* the pip size, the pip stroke size, and the pip direction.		*
C*									*
C* The front code is a three digit number.  Each of the three digits    *
C* represents a separate piece of information about the front.  The     *
C* front code is interpreted as follows:                                *
C*									*
C*   digit 1: type         digit 2: intensity     digit 3: character    *
C*   -------------         ------------------     ------------------    *
C* 0 stationary           0 unspecified intensity 0 unspecified char    *
C* 1 stationary above sfc 1 weak, decreasing      1 frontal decrsing    *
C* 2 warm                 2 weak                  2 activity little chg *
C* 3 warm above sfc       3 weak, increasing      3 area increasing     *
C* 4 cold                 4 moderate, decreasing  4 intertropical       *
C* 5 cold above sfc       5 moderate              5 forming or suspectd *
C* 6 occlusion            6 moderate, increasing  6 quasi-stationary    *
C* 7 instability line     7 strong, decreasing    7 with waves          *
C* 8 intertropical line   8 strong                8 diffuse             *
C* 9 convergence line     9 strong, increasing    9 position doubtful   *
C*									*
C* Example:                                                             *
C*        425 translates to cold (4) weak (2) and forming (5).          *
C*									*
C* GSFRNT  ( IFCOD, PIPSZ, IPIPST, IPIPDR, IRET )			*
C*									*
C* Input parameters:							*
C*	IFCOD		INTEGER		Front code 			*
C*	PIPSZ		REAL		Pip size			*
C*	IPIPST		INTEGER		Pip stroke size - not used     	*
C*	IPIPDR		INTEGER		Pip direction			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* E. Wehner/EAI	10/96	Created					*
C* E. Wehner/EAi	11/96	Eliminated width parameter		*
C* S. Jacobs/NCEP	 2/97	Documentation changes			*
C* T. Piper/GSC		 5/98	Corrected typo in prolog                *
C* S. Jacobs/NCEP	 6/98	Changed int IPIPSZ to real PIPSZ	*
C* A. Hardy/GSC		 6/98	Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		 isend (3), isnd2 (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 6
	isend (2) = FSFRNT
	isend (3) = ifcod
C
	CALL GPUT  ( isend, 3, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR  ( pipsz, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	isnd2 (1) = ipipst
 	isnd2 (2) = ipipdr
C
	CALL GPUT  ( isnd2, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GGET  ( iret, 1, ier )
	IF  ( iret .ne. NORMAL )  iret = ier
C*
	RETURN
	END
