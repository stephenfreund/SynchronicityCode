package anchor;

public class Queue {

  volatile int tail;
  volatile int head;

  Pair[] elems;

  public Queue() {
    this.elems = new Pair[512];
  }

  public boolean enqueue(Pair x) {
    if ((this.tail + 1) % this.elems.length != this.head) {
      this.elems[this.tail] = x;
      this.tail = (this.tail + 1) % this.elems.length;
      return true;
    } else {
      return false;
    }
  }

  public Pair dequeue() {
    if (this.head != this.tail) {
      Pair result = this.elems[this.head];
      this.head = (this.head + 1) % this.elems.length;
      return result;
    } else {
      return null;
    }
  }
}

