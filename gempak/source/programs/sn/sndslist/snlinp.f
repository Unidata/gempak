	SUBROUTINE SNLINP  ( snfile, area, dattim, snparm, stndex,
     +			     levels, vcoord, output, mrgdat, iret )
C************************************************************************
C* SNLINP								*
C*									*
C* This subroutine gets the input variables for SNLIST.			*
C*									*
C* SNLINP  ( SNFILE, AREA, DATTIM, SNPARM, STNDEX, LEVELS, VCOORD,	*
C*           OUTPUT, MRGDAT, IRET )					*
C**									*
C* Log:									*
C* S. Lilly/NCEP         7/10   Copied from SNLIST			*
C* S. Lilly/NCEP         7/10   Change the input variables list         *
C*                              pre-define the following variables:     *
C*                              "snparm; levels; vcoord; mrgdat;        *
C*                              and, stndex"                            *
C**									*
C*                              The following are input/output          *
C*                              variables:                              *
C*                              "SNFILE; AREA; DATTIM; and, OUTPUT      *
C************************************************************************
	CHARACTER*(*)	snfile, area, dattim, snparm, stndex, levels,
     +			vcoord, output, mrgdat
C------------------------------------------------------------------------
C*	Set the constant values for this program.
C
        snparm = 'pres;hght;tmpc;dwpc;relh;drct;sknt;mixr'
	levels = 'all'
	vcoord = 'pres'
	mrgdat = 'yes'
	stndex = ' '
C
C*	Get the user input variables.
C
	CALL IP_STR  ( 'SNFILE', snfile, ier1 )
	CALL IP_STR  ( 'AREA',   area,   ier2 )
	CALL IP_STR  ( 'DATTIM', dattim, ier3 )
	CALL IP_STR  ( 'OUTPUT', output, ier8 )
C
	iret = ier1 + ier2 + ier3 + ier8

	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
