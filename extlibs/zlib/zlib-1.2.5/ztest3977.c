#include <stdio.h>
#include <stdarg.h>

int mytest(const char *fmt, ...)
{
  char buf[20];
  va_list ap;

  va_start(ap, fmt);
  vsnprintf(buf, sizeof(buf), fmt, ap);
  va_end(ap);
  return 0;
}

int main()
{
  return (mytest("Hello%d\n", 1));
}
