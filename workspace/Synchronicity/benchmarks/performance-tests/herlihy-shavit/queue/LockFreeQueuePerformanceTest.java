
import queue.LockFreeQueue;

/**
 * Derived from code by Maurice Herlihy
 */
public class LockFreeQueuePerformanceTest {
    private static int TEST_SIZE = 512;
    private static int THREADS = 8;
    private static int ROUNDS = 10;
    private static int PER_THREAD = TEST_SIZE / THREADS;

    private static LockFreeQueue instance;

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
        instance = new LockFreeQueue();
        long nanos = System.currentTimeMillis();
        Thread[] myThreads = new Thread[THREADS];
        for (int i = 0; i < THREADS; i++) {
            myThreads[i] = new AddThread(i * PER_THREAD);
            //  myThreads[i + THREADS] = new RemoveThread(i * PER_THREAD);
        }
        for (int i = 0; i < THREADS; i++) {
            myThreads[i].start();
        }
        for (int i = 0; i < THREADS; i++) {
            myThreads[i].join();
        }
        return System.currentTimeMillis() - nanos;
    }

    static class AddThread extends Thread {
        int value;

        AddThread(int i) {
            value = i;
        }

        public void run()  {
            try {
                for (int i = 0; i < PER_THREAD / 2; i++) {
                    if (value + i == 128) {
                        int z = 0;
                    }
                    instance.enq(value + i);
                    instance.deq();
                }
            } catch (Exception e) { }
        }
    }

}
