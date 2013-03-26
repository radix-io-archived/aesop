#ifndef RESOURCE_WORKER_WORKER_H
#define RESOURCE_WORKER_WORKER_H

/**
 * The worker resource enables exposes the polling functionality
 * to non-resource code.
 *
 * It allows a function to be scheduled for later execution.
 * When the worker resource is polled, it will call all the scheduled
 * functions.
 *
 * This can also be used to make sure a function is called from the highest
 * level (i.e. the polling loop)
 */

typedef struct aesop_worker_t aesop_worker_t;

typedef void (*aesop_worker_function_t) (void * data);

/**
 * Initialize the worker API.
 * Returns AE_SUCCESS if no error.
 * Can be called multiple times.
 */
int aesop_worker_init (void);


/**
 * Release worker API.
 */
int aesop_worker_finalize (void);

/**
 * Register a worker (function + data value), which can be scheduled for
 * execution by the aesop_worker_schedule call.
 * Returns the worker handle, or NULL in case of error.
 */
aesop_worker_t * aesop_worker_register (
      aesop_worker_function_t func,
      void * data);

/**
 * Release resources associated with the worker.
 * The worker should not be scheduled when calling this function.
 * Returns AE_SUCCESS if no error.
 */
int aesop_worker_unregister (aesop_worker_t * worker);


/**
 * Returns AE_SUCCESS if no error.
 * It is not an error to schedule an entry which had already been scheduled.
 * Note: the same worker will always only be called once, no matter how many
 * times it has been scheduled.
 *
 * If the same function needs to be scheduled multiple times, register the
 * function multiple times.
 */
int aesop_worker_schedule (aesop_worker_t * worker);


#endif
