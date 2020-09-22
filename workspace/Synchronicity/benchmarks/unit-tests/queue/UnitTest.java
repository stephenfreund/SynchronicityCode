
import anchor.Queue;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Derived from code by Maurice Herlihy
 */
public class UnitTest {
	private final static int THREADS = 8;
	private final static int TEST_SIZE = 512;
	private final static int PER_THREAD = TEST_SIZE / THREADS;
	int index;
	Queue instance = new Queue();
	boolean[] map = new boolean[TEST_SIZE];
	Thread[] thread = new Thread[THREADS];

	@Test
	public void testSequential() {
		System.out.println("sequential push and pop");

		for (int i = 0; i < TEST_SIZE; i++) {
			instance.enqueue(i);
		}
		for (int i = 0; i < TEST_SIZE; i++) {
			int j = instance.dequeue();
			if (j != i) {
				fail("bad dequeue: " + j + " expected " + i);
			}
		}
	}

	@Test
	public void testParallelEnqueue()  throws Exception {
		System.out.println("parallel enqueue");
		for (int i = 0; i < THREADS; i++) {
			thread[i] = new EnqueueThread(i * PER_THREAD);
		}
		for (int i = 0; i < THREADS; i ++) {
			thread[i].start();
		}
		for (int i = 0; i < THREADS; i ++) {
			thread[i].join();
		}
		for (int i = 0; i < TEST_SIZE; i++) {
			int j = instance.dequeue();
			if (map[j]) {
				fail("duplicate pop: " + j);
			} else {
				map[j] = true;
			}
		}
	}

	@Test
	public void testParallelDequeue()  throws Exception {
		System.out.println("parallel dequeue");
		for (int i = 0; i < TEST_SIZE; i++) {
			map[i] = false;
		}
		for (int i = 0; i < TEST_SIZE; i++) {
			instance.enqueue(i);
		}
		for (int i = 0; i < THREADS; i++) {
			thread[i] = new DequeueThread();
		}
		for (int i = 0; i < THREADS; i ++) {
			thread[i].start();
		}
		for (int i = 0; i < THREADS; i ++) {
			thread[i].join();
		}
	}

	@Test
	public void testParallelBoth()  throws Exception {
		System.out.println("parallel both");
		Thread[] myThreads = new Thread[2 * THREADS];
		for (int i = 0; i < THREADS; i++) {
			myThreads[i] = new EnqueueThread(i * PER_THREAD);
			myThreads[i + THREADS] = new DequeueThread();
		}
		for (int i = 0; i < 2 * THREADS; i ++) {
			myThreads[i].start();
		}
		for (int i = 0; i < 2 * THREADS; i ++) {
			myThreads[i].join();
		}
	}
	class EnqueueThread extends Thread {
		int value;
		EnqueueThread(int i) {
			value = i;
		}
		public void run() {
			for (int i = 0; i < PER_THREAD; i++) {
				instance.enqueue(value + i);
			}
		}
	}
	class DequeueThread extends Thread {
		public void run() {
			for (int i = 0; i < PER_THREAD; i++) {
				int value = instance.dequeue();
				if (map[value]) {
					fail("DequeueThread: duplicate pop");
				}
				map[value] = true;
			}
		}
	}

}
