#include <stdio.h>
#include <stdlib.h>
#include "omp.h"
void omp_set_num_threads(int num_threads)
{
}

int omp_get_num_threads(void)
{
return 1;
}

int omp_get_max_threads(void)
{
return 1;
}

int omp_get_thread_num(void)
{
return 0;
}

int omp_get_num_procs(void)
{
return 1;
}

int omp_in_parallel(void)
{
return 0;
}

void omp_set_dynamic(int dynamic_threads)
{
}

int omp_get_dynamic(void)
{
return 0;
}

int omp_get_cancellation(void)
{
return 0;
}

void omp_set_nested(int nested)
{
}

int omp_get_nested(void)
{
return 0;
}

void omp_set_schedule(omp_sched_t kind, int modifier)
{
}

void omp_get_schedule(omp_sched_t *kind, int *modifier)
{
*kind = omp_sched_static;
*modifier = 0;
}

int omp_get_thread_limit(void)
{
return 1;
}

void omp_set_max_active_levels(int max_active_levels)
{
}

int omp_get_max_active_levels(void)
{
return 0;
}

int omp_get_level(void)
{
return 0;
}

int omp_get_ancestor_thread_num(int level)
{
if (level == 0)
{
return 0;
}
else
{
return -1;
}
}

int omp_get_team_size(int level)
{
if (level == 0)
{
return 1;
}
else
{
return -1;
}
}

int omp_get_active_level(void)
{
return 0;
}

int omp_in_final(void)
{
return 1;
}

omp_proc_bind_t omp_get_proc_bind(void)
{
return omp_proc_bind_false;
}

void omp_set_default_device(int device_num)
{
}

int omp_get_default_device(void)
{
return 0;
}

int omp_get_num_devices(void)
{
return 0;
}

int omp_get_num_teams(void)
{
return 1;
}

int omp_get_team_num(void)
{
return 0;
}

int omp_is_initial_device(void)
{
return 1;
}

struct __omp_lock
{
int lock;
};

enum { UNLOCKED = -1, INIT, LOCKED };

void omp_init_lock(omp_lock_t *arg)
{
struct __omp_lock *lock = (struct __omp_lock *)arg;
lock->lock = UNLOCKED;
}

void omp_destroy_lock(omp_lock_t *arg)
{
struct __omp_lock *lock = (struct __omp_lock *)arg;
lock->lock = INIT;
}

void omp_set_lock(omp_lock_t *arg)
{
struct __omp_lock *lock = (struct __omp_lock *)arg;
if (lock->lock == UNLOCKED)
{
lock->lock = LOCKED;
}
else if (lock->lock == LOCKED)
{
fprintf(stderr,
"error: deadlock in using lock variable\n");
exit(1);
}
else
{
fprintf(stderr, "error: lock not initialized\n");
exit(1);
}
}

void omp_unset_lock(omp_lock_t *arg)
{
struct __omp_lock *lock = (struct __omp_lock *)arg;
if (lock->lock == LOCKED)
{
lock->lock = UNLOCKED;
}
else if (lock->lock == UNLOCKED)
{
fprintf(stderr, "error: lock not set\n");
exit(1);
}
else
{
fprintf(stderr, "error: lock not initialized\n");
exit(1);
}
}

int omp_test_lock(omp_lock_t *arg)
{
struct __omp_lock *lock = (struct __omp_lock *)arg;
if (lock->lock == UNLOCKED)
{
lock->lock = LOCKED;
return 1;
}
else if (lock->lock == LOCKED)
{
return 0;
}
else
{
fprintf(stderr, "error: lock not initialized\n");
exit(1);
}
}

struct __omp_nest_lock
{
short owner;
short count;
};

enum { NOOWNER = -1, MASTER = 0 };

void omp_init_nest_lock(omp_nest_lock_t *arg)
{
struct __omp_nest_lock *nlock=(struct __omp_nest_lock *)arg;
nlock->owner = NOOWNER;
nlock->count = 0;
}

void omp_destroy_nest_lock(omp_nest_lock_t *arg)
{
struct __omp_nest_lock *nlock=(struct __omp_nest_lock *)arg;
nlock->owner = NOOWNER;
nlock->count = UNLOCKED;
}

void omp_set_nest_lock(omp_nest_lock_t *arg)
{
struct __omp_nest_lock *nlock=(struct __omp_nest_lock *)arg;
if (nlock->owner == MASTER && nlock->count >= 1)
{
nlock->count++;
}
else if (nlock->owner == NOOWNER && nlock->count == 0)
{
nlock->owner = MASTER;
nlock->count = 1;
}
else
{
fprintf(stderr,
"error: lock corrupted or not initialized\n");
exit(1);
}
}

void omp_unset_nest_lock(omp_nest_lock_t *arg)
{
struct __omp_nest_lock *nlock=(struct __omp_nest_lock *)arg;
if (nlock->owner == MASTER && nlock->count >= 1)
{
nlock->count--;
if (nlock->count == 0)
{
nlock->owner = NOOWNER;
}
}
else if (nlock->owner == NOOWNER && nlock->count == 0)
{
fprintf(stderr, "error: lock not set\n");
exit(1);
}
else
{
fprintf(stderr,
"error: lock corrupted or not initialized\n");
exit(1);
}
}

int omp_test_nest_lock(omp_nest_lock_t *arg)
{
struct __omp_nest_lock *nlock=(struct __omp_nest_lock *)arg;
omp_set_nest_lock(arg);
return nlock->count;
}

#include <stdlib.h>
#include <sys/time.h>

static double scale=.000001;

double omp_get_wtime(void)
{
/* This function does not provide a working
* wallclock timer. Replace it with a version
* customized for the target machine.
*/
struct timeval tv;
gettimeofday(&tv,NULL);
return (tv.tv_usec * scale) + tv.tv_sec; // convert tv to double in seconds
}

double omp_get_wtime_f(void)  // to be called from fortran
{
/* This function does not provide a working
* wallclock timer. Replace it with a version
* customized for the target machine.
*/
return omp_get_wtime();  // call C function
}

double omp_get_wtick(void)
{
/* This function does not provide a working
* clock tick function. Replace it with
* a version customized for the target machine.
*/
return .000001;  // microseconds
}
