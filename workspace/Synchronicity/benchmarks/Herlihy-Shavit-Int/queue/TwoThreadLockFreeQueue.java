///*
// * TwoThreadLockFreeQueue.java
// *
// * Created on June 22, 2007, 2:25 PM
// *
// * The Art of Multiprocessor Programming, by Maurice Herlihy and Nir Shavit.
// * by Maurice Herlihy and Nir Shavit.
// * Copyright 20065 Elsevier Inc. All rights reserved.
// */
//
//package queue;
//
//class TwoThreadLockFreeQueue {
//  int head = 0;   // next item to dequeue
//  int tail = 0;   // next empty slot
//  int[] items; // queue contents
//  public TwoThreadLockFreeQueue(int capacity) {
//    head = 0; tail = 0;
//    items = (int[]) new Object[capacity];
//  }
//  public void enq(int x) {
//    // spin while full
//    while (tail - head == items.length) {}; // spin
//    items[tail % items.length] = x;
//    tail++;
//  }
//  public Object deq() {
//    // spin while empty
//    while (tail - head == 0) {}; // spin
//    Object x = items[head % items.length];
//    head++;
//    return x;
//  }
//}