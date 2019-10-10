//  Hopefully useful software for FORTRAN and C
//  Copyright (C) 2019  Division de Recherche en Prevision Numerique
//                      Environnement Canada
// 
//  This is free software; you can redistribute it and/or
//  modify it under the terms of the GNU Lesser General Public
//  License as published by the Free Software Foundation,
//  version 2.1 of the License.
// 
//  This software is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  Lesser General Public License for more details.

#if defined(NO_STATIC_INLINE)
#define STATIC_INLINE static inline
#else
#if ! defined(STATIC_INLINE)
#define STATIC_INLINE
#endif
#endif

STATIC_INLINE int test_lock(int *lock){
  return *lock;
}

STATIC_INLINE int release_lock(int *lock){    // return 0 if not locked to start with
  return (__sync_val_compare_and_swap(lock,1, 0) == 1);
}

STATIC_INLINE void acquire_lock(int *lock){   // acquire a lock (LOCK MUST HAVE BEEN INITIALIZED TO ZERO)
  while(__sync_val_compare_and_swap(lock, 0, 1) != 0);
}

STATIC_INLINE int release_id_lock(int *lock, int id){    // return 0 if process/thread was not the owner of the lock and therefore cannot unlock
  return (__sync_val_compare_and_swap(lock, id+1, 0) == id+1);
}

STATIC_INLINE void acquire_id_lock(int *lock, int id){   // acquire a lock with id (LOCK MUST HAVE BEEN INITIALIZED TO ZERO)
  while(__sync_val_compare_and_swap(lock, 0, id+1) != 0) ;
}

