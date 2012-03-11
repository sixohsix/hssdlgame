#include <stdio.h>
#include <SDL.h>

#ifdef __GLASGOW_HASKELL__
#include "HsSdlTest_stub.h"
#endif

#ifdef __GLASGOW_HASKELL__
extern void __stginit_HsSdlTest ( void );
#endif

int SDL_main(int argc, char *argv[])
{
  int i;

  hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
  hs_add_root(__stginit_HsSdlTest);
#endif

  my_main();

  hs_exit();
  return 0;
}
