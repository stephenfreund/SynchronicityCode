/*
 * FineList.java
 *
 * Created on January 3, 2006, 6:50 PM
 *
 * From "Multiprocessor Synchronization and Concurrent Data Structures",
 * by Maurice Herlihy and Nir Shavit.
 * Copyright 2006 Elsevier Inc. All rights reserved.
 */
package lists;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
/**
 * Fine-grained synchronization: lock coupling (hand-over-hand locking).
 * @param int Item type.
 * @author Maurice Herlihy
 */
public class FineList {
  /**
   * First list entry
   */
  private Node head;
  /**
   * Constructor
   */
  public FineList() {
    // Add sentinels to start and end
    head      = new Node(Integer.MIN_VALUE);
    head.next = new Node(Integer.MAX_VALUE);
  }
  /**
   * Add an element.
   * @param item element to add
   * @return true iff element was not there already
   */
  public boolean add(int item) {
    head.lock();
    Node pred = head;
    try {
      Node curr = pred.next;
      curr.lock();
      try {
        while (curr.item < item) {
          pred.unlock();
          pred = curr;
          curr = curr.next;
          curr.lock();
        }
        if (curr.item == item) {
          return false;
        }
        Node newNode = new Node(item);
        newNode.next = curr;
        pred.next = newNode;
        return true;
      } finally {
        curr.unlock();
      }
    } finally {
      pred.unlock();
    }
  }
  /**
   * Remove an element.
   * @param item element to remove
   * @return true iff element was present
   */
  public boolean remove(int item) {
    Node pred = null, curr = null;
    head.lock();
    try {
      pred = head;
      curr = pred.next;
      curr.lock();
      try {
        while (curr.item < item) {
          pred.unlock();
          pred = curr;
          curr = curr.next;
          curr.lock();
        }
        if (curr.item == item) {
          pred.next = curr.next;
          return true;
        }
        return false;
      } finally {
        curr.unlock();
      }
    } finally {
      pred.unlock();
    }
  }
  public boolean contains(int item) {
    Node last = null, pred = null, curr = null;
    head.lock();
    try {
      pred = head;
      curr = pred.next;
      curr.lock();
      try {
        while (curr.item < item) {
          pred.unlock();
          pred = curr;
          curr = curr.next;
          curr.lock();
        }
        return (curr.item == item);
      } finally {
        curr.unlock();
      }
    } finally {
      pred.unlock();
    }
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
     * synchronizes individual Node
     */
    Lock lock;
    /**
     * Constructor for usual Node
     * @param item element in list
     */
    Node(int item) {
      this.item = item;
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

