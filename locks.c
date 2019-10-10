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

#include "inline_locks.h"

int reset_lock(int *lock){   // return 0 if not locked to start with
  return release_lock(lock);
}

void set_lock(int *lock){   // acquire a lock (LOCK MUST HAVE BEEN INITIALIZED TO ZERO)
  acquire_lock(lock);
}

int reset_id_lock(int *lock, int id){    // return 0 if process/thread was not the owner of the lock and therefore cannot unlock
  return release_id_lock(lock, id);
}

void set_id_lock(int *lock, int id){   // acquire a lock with id (LOCK MUST HAVE BEEN INITIALIZED TO ZERO)
  acquire_id_lock(lock, id);
}

