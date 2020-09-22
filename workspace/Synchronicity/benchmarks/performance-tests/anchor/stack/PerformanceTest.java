
import anchor.Stack;

/**
 * Derived from code by Maurice Herlihy
 */
public class PerformanceTest {
	private static int TEST_SIZE = 512;
	private static int THREADS = 8;
	private static int ROUNDS = 10;
	private static int PER_THREAD = TEST_SIZE / THREADS;

	private static Stack;

	public static void main(String args[]) throws Exception {
		TEST_SIZE = Integer.parseInt(args[0]);
		THREADS = Integer.parseInt(args[1]);
		ROUNDS = Integer.parseInt(args[2]);
		PER_THREAD = TEST_SIZE / THREADS;

		long total = 0;
		for (int i = 0; i < ROUNDS; i++) {
			long t = oneRound();
			System.out.println(t);
			total += t;
		}
		System.out.println(total / ROUNDS);
	}

	private static long oneRound() throws Exception {
		instance = new Stack();
		long nanos = System.currentTimeMillis();
		Thread[] myThreads = new Thread[2 * THREADS];
		for (int i = 0; i < THREADS; i++) {
			myThreads[i] = new PushThread(i * PER_THREAD);
			myThreads[i + THREADS] = new PopThread();
		}
		for (int i = 0; i < 2 * THREADS; i++) {
			myThreads[i].start();
		}
		for (int i = 0; i < 2 * THREADS; i++) {
			myThreads[i].join();
		}
		return System.currentTimeMillis() - nanos;
	}

	static class PushThread extends Thread {
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
	static class PopThread extends Thread {
		public void run() {
			for (int i = 0; i < PER_THREAD; i++) {
				int value = instance.pop();
			}
		}
	}

}
