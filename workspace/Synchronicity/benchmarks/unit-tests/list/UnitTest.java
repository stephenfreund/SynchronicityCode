
import anchor.List;
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
	List instance = new List();
	boolean[] map = new boolean[TEST_SIZE];
	Thread[] thread = new Thread[THREADS];

	@Test
	public void testSequential() {
		System.out.println("sequential add, contains, and remove");

		for (int i = 0; i < TEST_SIZE; i++) {
			instance.add(i);
		}
		for (int i = 0; i < TEST_SIZE; i++) {
			if (!instance.contains(i)) {
				fail("bad contains: " + i );
			}
		}
		for (int i = 0; i < TEST_SIZE; i++) {
			if (!instance.remove(i)) {
				fail("bad remove: " + i );
			}
		}
	}

	@Test
	public void testParallelAdd()  throws Exception {
		System.out.println("parallel add");
		for (int i = 0; i < THREADS; i++) {
			thread[i] = new AddThread(i * PER_THREAD);
		}
		for (int i = 0; i < THREADS; i ++) {
			thread[i].start();
		}
		for (int i = 0; i < THREADS; i ++) {
			thread[i].join();
		}
		for (int i = 0; i < TEST_SIZE; i++) {
			if (!instance.contains(i)) {
				fail("bad contains: " + i );
			}
		}
		for (int i = 0; i < TEST_SIZE; i++) {
			if (!instance.remove(i)) {
				fail("bad remove: " + i );
			}
		}
	}

	@Test
	public void testParallelRemove()  throws Exception {
		System.out.println("parallel remove");
		for (int i = 0; i < TEST_SIZE; i++) {
			instance.add(i);
		}
		for (int i = 0; i < TEST_SIZE; i++) {
			if (!instance.contains(i)) {
				fail("bad contains: " + i );
			}
		}
		for (int i = 0; i < THREADS; i++) {
			thread[i] = new RemoveThread(i * PER_THREAD);
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
			myThreads[i] = new AddThread(i * PER_THREAD);
			myThreads[i + THREADS] = new SafeRemoveThread(i * PER_THREAD);
		}
		for (int i = 0; i < 2 * THREADS; i ++) {
			myThreads[i].start();
		}
		for (int i = 0; i < 2 * THREADS; i ++) {
			myThreads[i].join();
		}
	}
	class AddThread extends Thread {
		int value;
		AddThread(int i) {
			value = i;
		}
		public void run() {
			for (int i = 0; i < PER_THREAD; i++) {
				if (value + i == 128) {
					int z =0;
				}
				instance.add(value + i);
			}
		}
	}
	class RemoveThread extends Thread {
		int value;
		RemoveThread(int i) {
			value = i;
		}
		public void run() {
			for (int i = 0; i < PER_THREAD; i++) {
				if (!instance.remove(value + i)) {
					fail("RemoveThread: duplicate remove: " + (value + i));
				}
			}
		}
	}
	class SafeRemoveThread extends Thread {
		int value;
		SafeRemoveThread(int i) {
			value = i;
		}
		public void run() {
			for (int i = 0; i < PER_THREAD; i++) {
				while (!instance.contains(value + i)) { }
				if (!instance.remove(value + i)) {
					fail("SafeRemoveThread: duplicate remove: " + (value + i));
				}
			}
		}
	}
}
