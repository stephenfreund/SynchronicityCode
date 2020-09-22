
import lists.CoarseList;

/**
 * Derived from code by Maurice Herlihy
 */
public class CoarseListPerformanceTest {
    private static int TEST_SIZE = 512;
    private static int THREADS = 8;
    private static int ROUNDS = 10;
    private static int PER_THREAD = TEST_SIZE / THREADS;

    private static CoarseList instance;

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
        instance = new CoarseList();
        long nanos = System.currentTimeMillis();
        Thread[] myThreads = new Thread[2 * THREADS];
        for (int i = 0; i < THREADS; i++) {
            myThreads[i] = new AddThread(i * PER_THREAD);
            myThreads[i + THREADS] = new ContainsThread(i * PER_THREAD);
        }
        for (int i = 0; i < 2 * THREADS; i++) {
            myThreads[i].start();
        }
        for (int i = 0; i < 2 * THREADS; i++) {
            myThreads[i].join();
        }
        return System.currentTimeMillis() - nanos;
    }

    static class AddThread extends Thread {
        int value;

        AddThread(int i) {
            value = i;
        }

        public void run() {
            for (int i = 0; i < PER_THREAD/2; i++) {
                if (value + i == 128) {
                    int z = 0;
                }
                instance.add(value + i);
            }
            for (int i = 0; i < PER_THREAD/2; i++) {
                if (!instance.remove(value + i)) {
                    assert false;
                }
            }
        }
    }

    static class ContainsThread extends Thread {
        int value;

        ContainsThread(int i) {
            value = i;
        }

        public void run() {
            for (int i = 0; i < PER_THREAD; i++) {
                instance.contains(value+i);
            }
        }
    }
}
