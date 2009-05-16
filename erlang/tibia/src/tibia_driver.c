#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <libpq-fe.h>

#include <erl_driver.h>
#include <ei.h>


/* Driver interface declarations */
static int tibia_control(ErlDrvData          handle,
			 unsigned int        command,
			 char *buf,      int buf_len,
			 char **res_buf, int res_buf_len);
static ErlDrvData tibia_start(ErlDrvPort port, char *buf);
static void       tibia_stop(ErlDrvData handle);
static void       tibia_finish(void);

static ErlDrvEntry tibia_driver = {
    NULL,                        /* init */
    tibia_start, 
    tibia_stop, 
    NULL,                        /* output */
    NULL,                        /* ready_input */
    NULL,                        /* ready_output */ 
    "tibia_driver",              /* the name of the driver */
    tibia_finish,                /* finish */
    NULL,                        /* handle */
    tibia_control, 
    NULL,                        /* timeout */
    NULL,                        /* outputv */
    NULL,                        /* ready_async */
    NULL,                        /* flush */
    NULL,                        /* call */
    NULL                         /* event */
};


typedef struct our_data_s {
    PGconn* conn;
} our_data_t;

/* Keep the following definitions in alignment with the
 * defines in erl_pq_sync.erl
 */

#define XTEA_DECRYPT   'D'
#define XTEA_ENCRYPT   'E'



/* INITIALIZATION AFTER LOADING */

/* 
 * This is the init function called after this driver has been loaded.
 * It must *not* be declared static. Must return the address to 
 * the driver entry.
 */

DRIVER_INIT(tibia_drv)
{
    return &tibia_driver;
}


static char* get_s(const char* buf, int len);
static int encrypt(uint32_t v[2], const uint32_t k[4]);
static int decrypt(uint32_t v[2], const uint32_t k[4]);

/* Since we are operating in binary mode, the return value from control
 * is irrelevant, as long as it is not negative.
 */

static ErlDrvData tibia_start(ErlDrvPort port, char *buf)
{
  our_data_t* data;

  data = (our_data_t*)driver_alloc(sizeof(our_data_t));
  data->conn = NULL;
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

  return (ErlDrvData)data;
}

static void tibia_finish(void)
{
  return;
}

static ErlDrvBinary* ei_x_to_new_binary(ei_x_buff* x)
{
    ErlDrvBinary* bin = driver_alloc_binary(x->index);
    if (bin != NULL)
	memcpy(&bin->orig_bytes[0], x->buff, x->index);
    return bin;
}

static int tibia_control(ErlDrvData          handle,
			 unsigned int        command,
			 char *buf,      int buf_len,
			 char **res_buf, int res_buf_len)
{
    int r;
    ei_x_buff x;
    our_data_t* data = (our_data_t*)handle;
    char* s = get_s(buf, buf_len);
    ei_x_new_with_version(&x);
    switch (command) {
        case XTEA_DECRYPT:    r = decrypt(s, data, &x);  break;
        case XTEA_ENCRYPT:    r = encrypt(data, &x);     break;
        default:              r = -1;                    break;
    }
    *rbuf = (char*)ei_x_to_new_binary(&x);
    ei_x_free(&x);
    driver_free(s);
    return r;
}




void encrypt(uint32_t v[2], const uint32_t k[4])
{
  uint32_t v0=v[0];
  uint32_t v1=v[1];
  uint32_t sum=0;
  uint32_t delta=0x61C88647;
  for (int32_t i = 0; i < 32; i++)
    {
      v0 += ((v1 << 4 ^ v1 >> 5) + v1) ^ (sum + k[sum & 3]);
      sum -= delta;
      v1 += ((v0 << 4 ^ v0 >> 5) + v0) ^ (sum + k[sum>>11 & 3]);
    }
  v[0]=v0; v[1]=v1;
}
 
void decrypt(uint32_t v[2], const uint32_t k[4])
{
  uint32_t v0=v[0];
  uint32_t v1=v[1];
  uint32_t delta=0x61C88647;
  uint32_t sum=0xC6EF3720;
  for (int32_t i = 0; i < 32; i++)
    {
      v1 -= ((v0 << 4 ^ v0 >> 5) + v0) ^ (sum + k[sum>>11 & 3]);
      sum += delta;
      v0 -= ((v1 << 4 ^ v1 >> 5) + v1) ^ (sum + k[sum & 3]);
    }
  v[0]=v0; v[1]=v1;
}
