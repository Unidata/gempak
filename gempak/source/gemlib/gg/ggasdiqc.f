         SUBROUTINE GG_ASDIQC (key, numpts, ages, lats, lons, hts, iret)
C**********************************************************************
C* GG_ASDIQC
C*
C* This routine checks to make sure the line segment lengths are within
C* reason.  It does this by computing the average speed over the 
C* distance travelled.  If the average speed <= 700 KTs, we set
C* iret to 0, to pass the line segment. Otherwise we set iret to -1,
C* to signal a problem and fail the line segment.
C*
C* At a future time, this routine may drop only bad points out of the
C* line segment, if spurious points can be reasonably determined.
C* 
C* GG_ASDIQC ( key, numpts, ages, lats, lons, hts, iret )
C*           
C* Input parameters:
C*      key        CHAR *       The Airport/Flight ID
C*      numpts     INTEGER      The number of points
C*      ages(*)   INTEGER      Array of ages in  minutes of locations
C*      lats(*)   REAL         Array of latitudes
C*      lons(*)   REAL         Array of longitudes
C*      hts(*)    REAL         Array of heights (hundreds of feet)
C*
C* Output parameters:
C*      iret       INTEGER      Return code: 0 for passes, -1 for fail
C* 
C**
C* Log:
C* L. Hinson/AWC   05/12
C**********************************************************************

         INCLUDE 'GEMPRM.PRM'
C*       Set speed limit to 700 knots on this QC         
         PARAMETER ( MXSPD = 700 )
         CHARACTER*(*) key
         INTEGER numpts
         INTEGER ages (*)
         REAL lats (*), lons (*)
         INTEGER hts (*)
         INTEGER iret
         REAL dist (LLMXPT)
         REAL tdiff (LLMXPT)
         REAL totaldist =0.0
         INTEGER totalage = 0.0
         numgdpts = 0
         iret = -1         
         IF (numpts .gt. 1) THEN
           DO ii = 1, numpts - 1
C*           Calculate the distance between successive points in meters
             CALL CLO_DIST (lats(ii), lons(ii), 1, 
     +                      lats(ii+1), lons(ii+1),
     +                      dist(ii), ier)
C*           Calculate the age diff between successive pts in seconds
             tdiff(ii) = abs((ages(ii+1) - ages(ii))*60)
             totalage = totalage + tdiff(ii)
             totaldist = totaldist + dist(ii)             
           END DO
C*         Calculate the speed over the distance travelled in KTs.
           IF (totaldist/totalage*1.943 .lt. MXSPD)  THEN
C*           WRITE (*,*) 'Key=', key
C*           WRITE (*,*) 'Total distance=', totaldist/1852.0, ' nm'
C*           WRITE (*,*) 'Avg speed=', totaldist/totalage*1.943, ' kts'
             iret = 0
           ELSE
             iret = -1
           END IF
         ELSE 
           iret = 0
         END IF
C*                        
       RETURN       
       END     
             
