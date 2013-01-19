	SUBROUTINE GDCDSP  ( gdfile, proj, gdarea, kxky, maxgrd,
     +			     cpyfil, anlyss, iret )
C************************************************************************
C* GDCDSP								*
C*									*
C* This subroutine displays a summary of the user options for GDCFIL.	*
C*									*
C* GDCDSP  ( GDFILE, PROJ, GDAREA, KXKY, MAXGRD, CPYFIL, ANLYSS, IRET )	*
C*           								*
C* Input parameters:							*
C*	GDFILE		CHAR*		Grid file name			*
C*	PROJ		CHAR*		Projection			*
C*	GDAREA		CHAR*		Grid area			*
C*	KXKY		CHAR*		Grid dimension			*
C*	MAXGRD		CHAR*		Maximun number of grids		*
C*	CPYFIL		CHAR*		Input for CPYFIL		*
C*	ANLYSS		CHAR*		Analysis block			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/88						*
C* K. Brill/GSC          4/90	Added ANLBLK				*
C* S. Schotz/GSC	 5/90	Get respnd locally from IP_RESP		*
C* M. desJardins/GSFC	 7/90	Write out MAXGRD			*
C* S. Schotz/GSC	10/90	Eliminate unnecessary blank lines	*
C* M. desJardins/NMC	 4/91	Cleaned up				*
C* T. Lee/GSC		 7/99	Bug fix on deltax/deltay calculation	* 
C* R. Tian/SAIC		 3/05	Changed input parameters		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdfile, proj, gdarea, kxky, maxgrd, cpyfil,
     +	                anlyss
C*
	LOGICAL		respnd
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	WRITE  ( 6, 1000 )
1000	FORMAT ( / ' GDCFIL PARAMETERS: ' / )
	WRITE  ( 6, 1001 ) gdfile
1001	FORMAT ( ' New grid file:         ', A )
C
C*	Check whether file was copied.
C
	IF  ( cpyfil .ne. ' ' )  THEN
	    WRITE  ( 6, 1002 ) cpyfil
1002	    FORMAT ( ' File copied:           ', A )
	END IF
C
C*	Write navigation parameters.
C
	WRITE  ( 6, 1005 )  proj, gdarea, kxky 
1005	FORMAT ( ' Grid projection:       ', A /
     +           ' Grid area:             ', A /
     +           ' Grid dimension:        ', A )
C
C*	Write analysis block information.
C
	WRITE ( 6, 1024 ) anlyss
1024    FORMAT ( ' Grid anlysis:          ', A )
C
C*	Write out maximum number of grids.
C
C	WRITE  (6, 1010)
1010    FORMAT ( / )
	WRITE  (6, 1030)  maxgrd
1030	FORMAT ( ' Maximum number of grids that can be added to file',
     +             2X, A )
C
C*	Get user response.
C
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd )  THEN
	    CALL TM_ACCP  ( ier )
	    IF  ( ier .eq. 2 )  iret = -1
	END IF
C*
	RETURN
	END
