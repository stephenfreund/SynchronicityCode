/*
 * LockFreeList.java
 *
 * Created on January 4, 2006, 2:41 PM
 *
 * From "Multiprocessor Synchronization and Concurrent Data Structures",
 * by Maurice Herlihy and Nir Shavit.
 * Copyright 2006 Elsevier Inc. All rights reserved.
 */

package lists;

import java.util.concurrent.atomic.AtomicMarkableReference;

/**
 * Lock-free List based on M. Michael's algorithm.
 * @param int Item type.
 * @author Maurice Herlihy
 */
public class LockFreeList {
  /**
   * First list node
   */
  Node head;
  /**
   * Constructor
   */
  public LockFreeList() {
    this.head  = new Node(Integer.MIN_VALUE);
    Node tail = new Node(Integer.MAX_VALUE);
    while (!head.next.compareAndSet(null, tail, false, false));
  }
  /**
   * Add an element.
   * @param item element to add
   * @return true iff element was not there already
   */
  public boolean add(int item) {
    boolean splice;
    while (true) {
      // find predecessor and curren entries
      Window window = find(head, item);
      Node pred = window.pred, curr = window.curr;
      // is the item present?
      if (curr.item == item) {
        return false;
      } else {
        // splice in new node
        Node node = new Node(item);
        node.next = new AtomicMarkableReference(curr, false);
        if (pred.next.compareAndSet(curr, node, false, false)) {
          return true;
        }
      }
    }
  }
  /**
   * Remove an element.
   * @param item element to remove
   * @return true iff element was present
   */
  public boolean remove(int item) {
    boolean snip;
    while (true) {
      // find predecessor and curren entries
      Window window = find(head, item);
      Node pred = window.pred, curr = window.curr;
      // is the item present?
      if (curr.item != item) {
        return false;
      } else {
        // snip out matching node
        Node succ = curr.next.getReference();
        snip = curr.next.attemptMark(succ, true);
        if (!snip)
          continue;
        pred.next.compareAndSet(curr, succ, false, false);
        return true;
      }
    }
  }
  /**
   * Test whether element is present
   * @param item element to test
   * @return true iff element is present
   */
  public boolean contains(int item) {
    // find predecessor and curren entries
    Window window = find(head, item);
    Node pred = window.pred, curr = window.curr;
    return (curr.item == item);
  }
  /**
   * list node
   */
  private class Node {
    /**
     * actual item
     */
    int item;
    /**
     * item's hash code
     */
    AtomicMarkableReference<Node> next;
    /**
     * Constructor for usual node
     * @param item element in list
     */
    Node(int item) {      // usual constructor
      this.item = item;
      this.next = new AtomicMarkableReference<Node>(null, false);
    }
  }
  
  /**
   * Pair of adjacent list entries.
   */
  class Window {
    /**
     * Earlier node.
     */
    public Node pred;
    /**
     * Later node.
     */
    public Node curr;
    /**
     * Constructor.
     */
    Window(Node pred, Node curr) {
      this.pred = pred; this.curr = curr;
    }
  }
  
  /**
   * If element is present, returns node and predecessor. If absent, returns
   * node with least larger item.
   * @param head start of list
   * @param item item to search for
   * @return If element is present, returns node and predecessor. If absent, returns
   * node with least larger item.
   */
  public Window find(Node head, int item) {
    Node pred = null, curr = null, succ = null;
    boolean[] marked = {false}; // is curr marked?
    boolean snip;
    retry: while (true) {
      pred = head;
      curr = pred.next.getReference();
      while (true) {
        succ = curr.next.get(marked); 
        while (marked[0]) {           // replace curr if marked
          snip = pred.next.compareAndSet(curr, succ, false, false);
          if (!snip) continue retry;
          curr = pred.next.getReference();
          succ = curr.next.get(marked);
        }
        if (curr.item >= item)
          return new Window(pred, curr);
        pred = curr;
        curr = succ;
      }
    }
  }
}
