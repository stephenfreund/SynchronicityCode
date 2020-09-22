
import anchor.Stack;
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
	Stack instance = new Stack();
	boolean[] map = new boolean[TEST_SIZE];
	Thread[] thread = new Thread[THREADS];

	@Test
	public void testSequential() {
		System.out.println("sequential push and pop");

		for (int i = 0; i < TEST_SIZE; i++) {
			instance.push(i);
		}
		for (int i = TEST_SIZE-1; i >= 0; i--) {
			int j = instance.pop();
			if (j != i) {
				fail("bad pop: " + j + " expected " + i);
			}
		}
	}

	@Test
	public void testParallelPush()  throws Exception {
		System.out.println("parallel push");
		for (int i = 0; i < THREADS; i++) {
			thread[i] = new PushThread(i * PER_THREAD);
		}
		for (int i = 0; i < THREADS; i ++) {
			thread[i].start();
		}
		for (int i = 0; i < THREADS; i ++) {
			thread[i].join();
		}
		for (int i = 0; i < TEST_SIZE; i++) {
			int j = instance.pop();
			if (map[j]) {
				fail("duplicate pop: " + j);
			} else {
				map[j] = true;
			}
		}
	}

	@Test
	public void testParallelPop()  throws Exception {
		System.out.println("parallel pop");
		for (int i = 0; i < TEST_SIZE; i++) {
			map[i] = false;
		}
		for (int i = 0; i < TEST_SIZE; i++) {
			instance.push(i);
		}
		for (int i = 0; i < THREADS; i++) {
			thread[i] = new PopThread();
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
			myThreads[i] = new PushThread(i * PER_THREAD);
			myThreads[i + THREADS] = new PopThread();
		}
		for (int i = 0; i < 2 * THREADS; i ++) {
			myThreads[i].start();
		}
		for (int i = 0; i < 2 * THREADS; i ++) {
			myThreads[i].join();
		}
	}
	class PushThread extends Thread {
		int value;
		PushThread(int i) {
			value = i;
		}
		public void run() {
			for (int i = 0; i < PER_THREAD; i++) {
				instance.push(value + i);
			}
		}
	}
	class PopThread extends Thread {
		public void run() {
			int i = 0;
			while (i < PER_THREAD) {
				int value = instance.pop();
				if (value != -1) {
					if (map[value]) {
						fail("PopThread: duplicate pop");
					}
					map[value] = true;
					i++;
				}
			}
		}
	}

}
