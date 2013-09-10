#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <readline/readline.h>
#include <readline/history.h>

char * readline (const char *prompt);

FILE* out, *in;

void main ()
{
  out = fdopen(7, "a");
  in = fdopen (6, "r");

  //  fputs("Test first command\n", out);
  
  char buf[10000];
  char* rlbuf;
  while (1)
    {
      int flags;
      
      if (-1 == (flags = fcntl(6, F_GETFL, 0)))
	flags = 0;
      fcntl(6, F_SETFL, flags | O_NONBLOCK);

      //      gets(buf);
      rlbuf = readline ("next ? ");
      add_history(rlbuf);
      fputs(rlbuf, out);
      fputs("\n", out);
      free(rlbuf);

      fflush (out);

      usleep (100000);
      while (fgets(buf, 10000, in) != NULL)  /* messages */
	{
	  printf ("%s", buf);
	}
    }
}
