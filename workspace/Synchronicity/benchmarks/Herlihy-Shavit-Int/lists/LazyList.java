/*
 * LazyList.java
 *
 * Created on January 4, 2006, 1:41 PM
 *
 * From "Multiprocessor Synchronization and Concurrent Data Structures",
 * by Maurice Herlihy and Nir Shavit.
 * Copyright 2006 Elsevier Inc. All rights reserved.
 */

package lists;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Lazy list implementation: lock-free contains method.
 * @param int Item type.
 * @author Maurice Herlihy
 */
public class LazyList {
  /**
   * First list Node
   */
  private Node head;
  /**
   * Constructor
   */
  public LazyList() {
    // Add sentinels to start and end
    this.head  = new Node(Integer.MIN_VALUE);
    this.head.next = new Node(Integer.MAX_VALUE);
  }
  
  /**
   * Check that prev and curr are still in list and adjacent
   */
  private boolean validate(Node pred, Node curr) {
    return  !pred.marked && !curr.marked && pred.next == curr;
  }
  /**
   * Add an element.
   * @param item element to add
   * @return true iff element was not there already
   */
  public boolean add(int item) {
    while (true) {
      Node pred = this.head;
      Node curr = head.next;
      while (curr.item < item) {
        pred = curr; curr = curr.next;
      }
      pred.lock();
      try {
        curr.lock();
        try {
          if (validate(pred, curr)) {
            if (curr.item == item) { // present
              return false;
            } else {               // not present
              Node Node = new Node(item);
              Node.next = curr;
              pred.next = Node;
              return true;
            }
          }
        } finally { // always unlock
          curr.unlock();
        }
      } finally { // always unlock
        pred.unlock();
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
      Node pred = this.head;
      Node curr = head.next;
      while (curr.item < item) {
        pred = curr; curr = curr.next;
      }
      pred.lock();
      try {
        curr.lock();
        try {
          if (validate(pred, curr)) {
            if (curr.item != item) {    // present
              return false;
            } else {                  // absent
              curr.marked = true;     // logically remove
              pred.next = curr.next;  // physically remove
              return true;
            }
          }
        } finally {                   // always unlock curr
          curr.unlock();
        }
      } finally {                     // always unlock pred
        pred.unlock();
      }
    }
  }
  /**
   * Test whether element is present
   * @param item element to test
   * @return true iff element is present
   */
  public boolean contains(int item) {
    Node curr = this.head;
    while (curr.item < item)
      curr = curr.next;
    return curr.item == item && !curr.marked;
  }
  /**
   * list Node
   */
  private class Node {
    /**
     * actual item
     */
    int item;
    /**
     * next Node in list
     */
    Node next;
    /**
     * If true, Node is logically deleted.
     */
    boolean marked;
    /**
     * Synchronizes Node.
     */
    Lock lock;
    /**
     * Constructor for usual Node
     * @param item element in list
     */
    Node(int item) {      // usual constructor
      this.item = item;
      this.next = null;
      this.marked = false;
      this.lock = new ReentrantLock();
    }
    /**
     * Lock Node
     */
    void lock() {lock.lock();}
    /**
     * Unlock Node
     */
    void unlock() {lock.unlock();}
  }
}

