void ARABOX(int area,int fstln,int lstln,int fstel,int lstel,
            int band,char *pqty,int spac,int elelen,int *scale,int *buf);
            /*int band,char *pqty,int spac,int elelen,int *buf);*/
int  araget (int fd, int offset, int nbyte, int *array);
int araopt (int fd, int slot, int nopt, int *opt, int *val);
int clsara (int fd);
int opnara (char *area, char red_wrt);
int  redara (int fd, int line, int elem, int nele, int band, int *buf);
int  readd (int fd, int direct[]);
int  writd (int fd, int direct[]);
int  redpfx (int fd, int line, int *buf);
int  wrtara (int fd, int line, int *buf);
