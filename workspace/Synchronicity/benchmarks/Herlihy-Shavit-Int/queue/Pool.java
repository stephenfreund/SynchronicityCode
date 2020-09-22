/*
 * Pool.java
 *
 * Created on 15 July 2007, 3:50 PM PDT
 *
 * From "Multiprocessor Synchronization and Concurrent Data Structures",
 * by Maurice Herlihy and Nir Shavit.
 * Copyright 2007 Elsevier Inc. All rights reserved.
 */

package queue;

/**
 * @param int item type
 * @author mph
 */
public interface Pool {
  void put(int item);
  int get();
}
