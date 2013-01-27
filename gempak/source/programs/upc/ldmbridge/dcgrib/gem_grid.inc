C************************************************************************
C* gem_grid.inc								*
C*									*
C* FORTRAN common block file for gribtogem decoder			*
C*									*
C* Log									*
C* Unknown								*
C* P.Bruehl/Unidata	5/95	Gribtogem Version 2			*
C*									*
C************************************************************************
      PARAMETER ( ngrid=54 )
      PARAMETER ( NUM_PROJ=256 )

      INTEGER     nmc(NUM_PROJ), ecmwf(NUM_PROJ), tgrid_id(ngrid)
      INTEGER     ukm(NUM_PROJ)
      INTEGER     scol(ngrid), srow(ngrid), test_id
      INTEGER     ngx(ngrid), ngy(ngrid)
      REAL        rlatb(ngrid), rlatt(ngrid), rlonl(ngrid), rlonr(ngrid)
      REAL        ang1(ngrid), ang2(ngrid), ang3(ngrid)
      LOGICAL     subgrid(ngrid)
      CHARACTER*3 proj(ngrid)

      COMMON /gem_grid/ nmc, ecmwf, ukm, tgrid_id, scol, srow,
     &     test_id, ngx, ngy, rlatb, rlatt, rlonl, rlonr,
     &     ang1, ang2, ang3, subgrid

      COMMON /gem_grid_c/ proj
