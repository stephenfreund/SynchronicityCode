/*
 * StripedCuckooHashSet.java
 *
 * Created on November 1, 2006, 3:59 PM
 *
 * From "The Art of Multiprocessor Programming",
 * by Maurice Herlihy and Nir Shavit.
 * Copyright 2006 Elsevier Inc. All rights reserved.
 */
package hash;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Concurrent Cuckoo hashing using lock striping.
 * @param int Item type.
 * @author Maurice Herlihy
 */
public class StripedCuckooHashSet extends PhasedCuckooHashSet{
  final ReentrantLock[][] lock;
  
  /**
   * Constructor
   * @param capacity Internal array size.
   */
  public StripedCuckooHashSet(int capacity) {
    super(capacity);
    lock  = new ReentrantLock[2][capacity];
    for (int i = 0; i < 2; i++) {
      for (int j = 0; j < capacity; j++) {
        lock[i][j] = new ReentrantLock();
      }
    }
  }
  
  /**
   * double the set size
   */
  public void resize() {
    int oldCapacity = capacity;
    for (Lock _lock : lock[0]) {
      _lock.lock();
    }
    try {
      if (capacity != oldCapacity) {  // someone else resized first
        return;
      }
      List[][] oldTable = table;
      capacity = 2 * capacity;
      table = (List[][]) new List[2][capacity];
      for (List[] row : table) {
        for (int i = 0; i < row.length; i++) {
          row[i]  = new ArrayList(PROBE_SIZE);
        }
      }
      for (List[] row : oldTable) {
        for (List set : row) {
          for (int z : set) {
            add(z);
          }
        }
      }
    } finally {
      for (Lock _lock : lock[0]) {
        _lock.unlock();
      }
    }
  }
  
  /**
   * Synchronize before adding, removing, or testing for item
   * @param x item involved
   */
  public final void acquire(int x) {
    Lock lock0 = lock[0][hash0(x) % lock[0].length];
    Lock lock1 = lock[1][hash1(x) % lock[1].length];
    lock0.lock();
    lock1.lock();
  }
  /**
   * synchronize after adding, removing, or testing for item
   * @param x item involved
   */
  public final void release(int x) {
    Lock lock0 = lock[0][hash0(x) % lock[0].length];
    Lock lock1 = lock[1][hash1(x) % lock[1].length];
    lock0.unlock();
    lock1.unlock();
  }
}
