///*
// * SynchronousQueue.java
// *
// * Created on March 12, 2006, 5:41 PM
// *
// * From "The Art of Multiprocessor Programming"
// * by Maurice Herlihy and Nir Shavit.
// * Copyright 2006 Elsevier Inc. All rights reserved.
// */
//
//package queue;
//
//import java.util.concurrent.locks.Condition;
//import java.util.concurrent.locks.Lock;
//import java.util.concurrent.locks.ReentrantLock;
//
///**
// * Naive synchronous queue.
// * Based on "Scalable Synchronous Queues" by Scherer, Lea, and Scott.
// * @param int item type
// * @author Maurice Herlihy
// */
//public class SynchronousQueue {
//  int item = -1;
//  boolean enqueuing;
//  Lock lock;
//  Condition condition;
//
//  public SynchronousQueue() {
//    enqueuing = false;
//    lock = new ReentrantLock();
//    condition = lock.newCondition();
//  }
//  public int deq() throws InterruptedException {
//    lock.lock();
//    try {
//      while (item == null) {
//        condition.await();
//      }
//      int t = item;
//      item = null;
//      condition.signalAll();
//      return t;
//    } finally {
//      lock.unlock();
//    }
//  }
//  public void enq(int value) throws InterruptedException {
//    if (value == null) throw new NullPointerException();
//    try {
//      while (enqueuing) { // another enqueuer's turn
//        condition.await();
//      }
//      enqueuing = true; // my turn starts
//      item = value;
//      condition.signalAll();
//      while (item != null) {
//        condition.await();
//      }
//      enqueuing = false;  // my turn ends
//      condition.signalAll();
//    } finally {
//      lock.unlock();
//    }
//  }
//}
