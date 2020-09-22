/*
 * OptimisticList.java
 *
 * Created on January 4, 2006, 1:49 PM
 *
 * From "Multiprocessor Synchronization and Concurrent Data Structures",
 * by Maurice Herlihy and Nir Shavit.
 * Copyright 2006 Elsevier Inc. All rights reserved.
 */
package lists;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
/**
 * Optimistic List implementation.
 * @param int Item type.
 * @author Maurice Herlihy
 */
public class OptimisticList {
  /**
   * First list entry
   */
  private Entry head;
  /**
   * Constructor
   */
  public OptimisticList() {
    this.head  = new Entry(Integer.MIN_VALUE);
    this.head.next = new Entry(Integer.MAX_VALUE);
  }
  /**
   * Add an element.
   * @param item element to add
   * @return true iff element was not there already
   */
  public boolean add(int item) {
    while (true) {
      Entry pred = this.head;
      Entry curr = pred.next;
      while (curr.item <= item) {
        pred = curr; curr = curr.next;
      }
      pred.lock(); curr.lock();
      try {
        if (validate(pred, curr)) {
          if (curr.item == item) { // present
            return false;
          } else {               // not present
            Entry entry = new Entry(item);
            entry.next = curr;
            pred.next = entry;
            return true;
          }
        }
      } finally {                // always unlock
        pred.unlock(); curr.unlock();
      }
    }
  }
  /**
   * Remove an element.
   * @param item element to remove
   * @return true iff element was present
   */
  public boolean remove(int item) {
    while (true) {
      Entry pred = this.head;
      Entry curr = pred.next;
      while (curr.item < item) {
        pred = curr; curr = curr.next;
      }
      pred.lock(); curr.lock();
      try {
        if (validate(pred, curr)) {
          if (curr.item == item) { // present in list
            pred.next = curr.next;
            return true;
          } else {               // not present in list
            return false;
          }
        }
      } finally {                // always unlock
        pred.unlock(); curr.unlock();
      }
    }
  }
  /**
   * Test whether element is present
   * @param item element to test
   * @return true iff element is present
   */
  public boolean contains(int item) {
    while (true) {
      Entry pred = this.head; // sentinel node;
      Entry curr = pred.next;
      while (curr.item < item) {
        pred = curr; curr = curr.next;
      }
      try {
        pred.lock(); curr.lock();
        if (validate(pred, curr)) {
          return (curr.item == item);
        }
      } finally {                // always unlock
        pred.unlock(); curr.unlock();
      }
    }
  }
  /**
     * Check that prev and curr are still in list and adjacent
     * @param pred predecessor node
     * @param curr current node
     * @return whther predecessor and current have changed
     */
  private boolean validate(Entry pred, Entry curr) {
    Entry entry = head;
    while (entry.item <= pred.item) {
      if (entry == pred)
        return pred.next == curr;
      entry = entry.next;
    }
    return false;
  }
  /**
   * list entry
   */
  private class Entry {
    /**
     * actual item
     */
    int item;
    /**
     * item's hash code
     */
    Entry next;
    /**
     * Synchronizes entry.
     */
    Lock lock;
    /**
     * Constructor for usual entry
     * @param item element in list
     */
    Entry(int item) {
      this.item = item;
      lock = new ReentrantLock();
    }
    /**
     * Lock entry
     */
    void lock() {lock.lock();}
    /**
     * Unlock entry
     */
    void unlock() {lock.unlock();}
  }
}
