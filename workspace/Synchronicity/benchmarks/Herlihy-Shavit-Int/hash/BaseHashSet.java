/*
 * BaseHashSet.java
 *
 * Created on November 15, 2006, 3:23 PM
 *
 * From "The Art of Multiprocessor Programming",
 * by Maurice Herlihy and Nir Shavit.
 * Copyright 2006 Elsevier Inc. All rights reserved.
 */

package hash;

import java.util.ArrayList;
import java.util.List;

/**
 * Simple fine-grained hash map.
 * @param int item type
 * @author Maurice Herlihy
 */
public abstract class BaseHashSet {
  protected List[] table;
  protected int size;
  public BaseHashSet(int capacity) {
    size = 0;
    table = (List[]) new List[capacity];
    for (int i = 0; i < capacity; i++) {
      table[i] = new ArrayList();
    }
  }
  /**
   * Is item in set?
   * @param x item to test
   * @return <code>true</code> iff item present
   */
  public boolean contains(int x) {
    acquire(x);
    try {
      int myBucket = Math.abs(x.hashCode() % table.length);
      return table[myBucket].contains(x);
    } finally {
      release(x);
    }
  }
  /**
   * Add item to set
   * @param x item to add
   * @return <code>true</code> iff set changed
   */
  public boolean add(int x) {
    boolean result = false;
    acquire(x);
    try {
      int myBucket = Math.abs(x.hashCode() % table.length);
      result = table[myBucket].add(x);
      size = result ? size + 1 : size;
    } finally {
      release(x); // always unlock
    }
    if (policy())
      resize();
    return result;
  }
  /**
   * Remove item from set
   * @param x item to remove
   * @return <code>true</code> iff set changed
   */
  public boolean remove(int x) {
    acquire(x);
    try {
      int myBucket = Math.abs(x.hashCode() % table.length);
      boolean result = table[myBucket].remove(x);
      size = result ? size - 1 : size;
      return result;
    } finally {
      release(x); // always unlock
    }
  }
  
  /**
   * Synchronize before adding, removing, or testing for item
   * @param x item involved
   */
  public abstract void acquire(int x);
  /**
   * synchronize after adding, removing, or testing for item
   * @param x item involved
   */
  public abstract void release(int x);
  /**
   * double the set size
   */
  public abstract void resize();
  /**
   * decide whether to resize
   * @return whether to resize
   */
  public abstract boolean policy();
  
}

