#include <stdio.h>

#define MAXSGRID	512

struct grid_type{
int cntr,gid,cid,nx,ny,scol,srow;
float ang[3],gar[4];
char proj[9];
};


#ifdef SGRID_DEF
struct grid_type gtype[MAXSGRID];
int sgridval;
int nsgridvals;

#else

extern struct grid_type gtype[MAXSGRID];
extern int sgridval;
extern int nsgridvals;
#endif

