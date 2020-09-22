/*
 * CoarseList.java
 *
 * Created on January 3, 2006, 5:02 PM
 *
 * From "Multiprocessor Synchronization and Concurrent Data Structures",
 * by Maurice Herlihy and Nir Shavit.
 * Copyright 2006 Elsevier Inc. All rights reserved.
 */
package lists;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
/**
 * List using coarse-grained synchronization.
 * @param int Item type.
 * @author Maurice Herlihy
 */
public class CoarseList {
  /**
   * First list Node
   */
  private Node head;
  /**
   * Last list Node
   */
  private Node tail;
  /**
   * Synchronizes access to list
   */
  private Lock lock = new ReentrantLock();

  /**
   * Constructor
   */
  public CoarseList() {
    // Add sentinels to start and end
    head = new Node(Integer.MIN_VALUE);
    tail = new Node(Integer.MAX_VALUE);
    head.next = this.tail;
  }

  /**
   * Add an element.
   *
   * @param item element to add
   * @return true iff element was not there already
   */

  public boolean add(int item) {
    Node pred, curr;
    lock.lock();
    try {
      pred = head;
      curr = pred.next;
      while (curr.item < item) {
        pred = curr;
        curr = curr.next;
      }
      if (item == curr.item) {
        return false;
      } else {
        Node node = new Node(item);
        node.next = curr;
        pred.next = node;
        return true;
      }
    } finally {
      lock.unlock();
    }
  }

  /**
   * Remove an element.
   *
   * @param item element to remove
   * @return true iff element was present
   */
  public boolean remove(int item) {
    Node pred, curr;
    lock.lock();
    try {
      pred = this.head;
      curr = pred.next;
      while (curr.item < item) {
        pred = curr;
        curr = curr.next;
      }
      if (item == curr.item) {  // present
        pred.next = curr.next;
        return true;
      } else {
        return false;         // not present
      }
    } finally {               // always unlock
      lock.unlock();
    }
  }

  /**
   * Test whether element is present
   *
   * @param item element to test
   * @return true iff element is present
   */
  public boolean contains(int item) {
    Node pred, curr;
    lock.lock();
    try {
      pred = head;
      curr = pred.next;
      while (curr.item < item) {
        pred = curr;
        curr = curr.next;
      }
      return (item == curr.item);
    } finally {               // always unlock
      lock.unlock();
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
     * Constructor for usual Node
     *
     * @param item element in list
     */
    Node(int item) {
      this.item = item;
    }
  }
}