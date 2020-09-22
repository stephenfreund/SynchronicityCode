/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package queue;

import java.util.logging.Level;
import java.util.logging.Logger;

/*
 * BoundedQueue.java
 *
 * Created on December 27, 2005, 7:14 PM
 *
 * The Art of Multiprocessor Programming, by Maurice Herlihy and Nir Shavit.
 * Copyright 2006 Elsevier Inc. All rights reserved.
 */
public class Queue {
  int items[];
  int head, size;
  int capacity;
  /**
   * Constructor.
   * @param capacity Max number of items allowed in queue.
   */
  public Queue(int capacity) {
    items = (int[]) new int[capacity];
    head = size = 0;
  }
  /**
   * Remove and return head of queue.
   * @return remove first item in queue
   */
  public synchronized int deq() {
    while (size == 0) {
      try {
        wait();
      } catch (InterruptedException ex) {
      }
    }
    notifyAll();
    size--;
    return items[head++];
  }
  /**
   * Append item to end of queue.
   * @param x item to append
   */
  public synchronized void enq(int x) {
    while (size == capacity) {
      try {
        wait();
      } catch (InterruptedException ex) {
      }
    }
    notifyAll();
    items[(head + size) % capacity] = x;
    size++;
  }
}
