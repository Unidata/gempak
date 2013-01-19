	SUBROUTINE GD_FLTM  ( filnms, nfiles, maxt, 
     +			      ngdftm, gdtlst, iret )
C************************************************************************
C* GD_FLTM								*
C*									*
C* This subroutine returns all the times present in a list of grid	*
C* files.								*
C*									*
C* GD_FLTM  ( FILNMS, NFILES, MAXT, NGDFTM, GDTLST, IRET )		*
C*									*
C* Input parameters:							*
C*	FILNMS(LLMXGT)	CHAR*256	Grid file names array		*
C*	NFILES		INTEGER		Number of grid filename		*
C*	MAXT  		INTEGER		Maximum number of times allowed	*
C*									*
C* Output parameters:							*
C*	NGDFTM		INTEGER		Number of times			*
C*	GDTLST(*)	CHAR*		List of GEMPAK times		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = read/write error		*
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	 8/98						*
C* S. Jacobs/NCEP	 9/98	Changed maxtim to LLMXGT in GD_GTIM call*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	CHARACTER	filnms(LLMXGT)*256
	CHARACTER*(*)	gdtlst(*)
	CHARACTER*20 	timarr (LLMXGT)
C
	REAL		anlblk(128), rnvblk(256)
C-----------------------------------------------------------------------
	iret = 0
	ngdftm = 0
C
C*	    Get all times from the files returned.
C
	    ngdftm = 0
	    DO  nf = 1, nfiles
C
	        CALL ST_LSTR ( filnms(nf), lf, iret )
	        CALL GD_OFIL ( filnms(nf)(:lf), .false., .true.,
     +			       igdfln, navsz, rnvblk, ianlsz, anlblk,
     +			       ihdrsz, maxgrd, iret )
C
	        IF ( iret .eq. 0 )  THEN
C
	            CALL GD_GTIM ( igdfln, LLMXGT, timarr, nt, iret )
C
C*	            Loop through the times returned.
C
	            DO  i = 1, nt
C
C*	                Convert the times from integers to characters.
C
		        IF ( ngdftm .lt. maxt )  THEN
	                    ngdftm = ngdftm + 1
	                    CALL TG_ITOC  ( igdatm (1,i,igdfln), 
     +				            gdtlst (ngdftm), ier )
		        END IF
	            END DO
C
	            CALL GD_CLOS ( igdfln, ier )
C
	        END IF
C
	    END DO
C*
	RETURN
	END
