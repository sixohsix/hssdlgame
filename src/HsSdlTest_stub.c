#define IN_STG_CODE 0
#include "Rts.h"
#include "Stg.h"
#ifdef __cplusplus
extern "C" {
#endif
 
extern StgClosure HsSdlTest_zdfmyzumainzua1OV_closure;
void my_main(void)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,&HsSdlTest_zdfmyzumainzua1OV_closure) ,&ret);
rts_checkSchedStatus("my_main",cap);
rts_unlock(cap);
}
static void stginit_export_HsSdlTest_zdfmyzumainzua1OV() __attribute__((constructor));
static void stginit_export_HsSdlTest_zdfmyzumainzua1OV()
{getStablePtr((StgPtr) &HsSdlTest_zdfmyzumainzua1OV_closure);}
#ifdef __cplusplus
}
#endif

