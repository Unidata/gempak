	SUBROUTINE RU_DRP2  ( nlev1, data, nlev2, wdata, sfcdir, sfcspd,
     +                        iret )
C************************************************************************
C* RU_DRP2                                                              *
C*                                                                      *
C* This subroutine scans the mandatory wind levels and the significant 	*
C* wind levels to find the level with highest pressure with non-missing	*
C* wind direction and speed.  It returns these values to be substituted	*
C* for the missing surface wind values.					*
C*                                                                      *
C* RU_DRP2  ( NLEV1, DATA, NLEV2, WDATA, SFCDIR, SFCSPD, IRET )		*
C*                                                                      *
C* Input parameters:                                                    *
C*	NLEV1		INTEGER		Number of mandatory levels	*
C*	DATA  (6,*)	REAL		Mandatory level data (TTAA)	*
C*	NLEV1		INTEGER		Number of sig wind presr lvls	*
C*	WDATA (3,*)	REAL		Sig wind data on presrs (TTBB)	*
C*                                                                      *
C* Output parameters:                                                   *
C*	SFCDIR		REAL		Surface wind direction		*
C*	SFCSPD		REAL		Surface wind speed		*
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* m.gamazaychikov/SAIC 07/05						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		data (6,*), wdata (3,*)
C*
	LOGICAL		done
C* 
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret   = 0
	sfcdir = RMISSD
	sfcspd = RMISSD
	dird   = RMISSD
	dirw   = RMISSD
	spdd   = RMISSD
	spdw   = RMISSD
	prsd   = RMISSD
	prsw   = RMISSD
C
C*	Find the highest mandatory data pressure with non-missing
C*	wind values
C
	ii = 2
	done   = .false.
	DO WHILE ( .not. done )
	   IF ( .not. ERMISS ( data ( 1, ii ) ) .and.
     +          .not. ERMISS ( data ( 4, ii ) ) .and. 
     +          .not. ERMISS ( data ( 5, ii ) ) ) THEN
	      prsd = data ( 1, ii )
	      dird = data ( 4, ii )
	      spdd = data ( 5, ii )
	      done = .true.
	   END IF
	   ii = ii + 1
	   IF ( ii .gt. nlev1 ) done = .true.
	END DO
C
C*	Find the highest significant data pressure with non-missing
C*	wind values
C
	ii = 1
	done   = .false.
	DO WHILE ( .not. done )
	   IF ( .not. ERMISS ( wdata ( 1, ii ) ) .and.
     +          .not. ERMISS ( wdata ( 2, ii ) ) .and. 
     +          .not. ERMISS ( wdata ( 3, ii ) ) ) THEN
	      prsw = wdata ( 1, ii )
	      dirw = wdata ( 2, ii )
	      spdw = wdata ( 3, ii )
	      done = .true.
	   END IF
	   ii = ii + 1
	   IF ( ii .gt. nlev2 ) done = .true.
	END DO
C
C*	Determine which level has the higher pressure 
C*      and store level wind data
C
	IF ( prsd .ge. prsw ) THEN
	   sfcdir = dird
	   sfcspd = spdd
	  ELSE
	   sfcdir = dirw
	   sfcspd = spdw
	END IF
C*
	RETURN
	END
