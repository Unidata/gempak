/* png function prototypes */

#include "png.h"

void png_set_memheap(char *memheap);
void pngout_init(int width, int height);
void pngwrite(char *memheap);
int  png_get_prodlen();
void pngout_end();


