	SUBROUTINE GD_GTIM  ( iacss, maxtim, timarr, ntimes, iret )
C************************************************************************
C* GD_GTIM								*
C*									*
C* This subroutine returns all the times present in a grid file.	*
C* Only the first times are returned.  They are sorted from earliest	*
C* to latest.								*
C*									*
C* GD_GTIM  ( IACSS, MAXTIM, TIMARR, NTIMES, IRET )			*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		Grid access number		*
C*	MAXTIM		INTEGER		Maximum number of times		*
C*									*
C* Output parameters:							*
C*	TIMARR (NTIMES)	CHAR*		GEMPAK times			*
C*	NTIMES		INTEGER		Number of times			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = read/write error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/88						*
C* M. desJardins/GSFC	 4/89	Changed sorting				*
C* S. Jacobs/NCEP	 9/98	Added check to return <= MAXTIM times	*
C* R. Tian/SAIC          1/04   Added GD_FCHK call                      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	CHARACTER*(*)	timarr (*)
C-----------------------------------------------------------------------
        iret = 0
C
C*      Convert access number to DM number.
C
        CALL GD_FCHK ( iacss, igdfln, iret )
        IF ( iret .ne. 0 ) THEN
            RETURN
        END IF
C
C*	Loop through the times saved in common.
C
	ntimes = ktgrid ( igdfln )
	IF  ( ntimes .gt. maxtim )  ntimes = maxtim
	DO  i = 1, ntimes
C
C*	    Convert the times from integers to characters.
C
	    CALL TG_ITOC  ( igdatm (1,i,igdfln), timarr (i), ier )
	END DO
C*
	RETURN
	END
