import anchor.*;
import java.util.Arrays;
import java.util.Random;

class Benchmark {
  Queue queue;

  long enq(int warmups, int total) {
    Pair p = new Pair(0, 1);
    for (int i = 0; i < warmups; i++) {
      boolean result;
      do {
        result = queue.enqueue(p);
      } while (!result);
    }
    long time = System.currentTimeMillis();
    for (int i = 0; i < total; i++) {
      boolean result;
      do {
        result = queue.enqueue(p);
      } while (!result);
    }
    return (System.currentTimeMillis() - time);
  }

  void deq(int warmups, int total) {
    for (int i = 0; i < warmups; i++) {
      Pair p;
      do {
        p = queue.dequeue();
      } while (p == null);
    }
    for (int i = 0; i < total; i++) {
      Pair p;
      do {
        p = queue.dequeue();
      } while (p == null);
    }
  }

  long result = 0;

  long bench(int warmups, int total) throws Exception {
    this.queue = new Queue();
    Thread e = new Thread() {
      public void run() {
        result = enq(warmups, total);
      }
    };
    Thread d = new Thread() {
      public void run() {
        deq(warmups, total);
      }
    };
    e.start();
    d.start();
    e.join();
    d.join();
    return result;
  }

  public static void main(String args[]) throws Exception {
    int warmup = 5000000;
    int total = 50000000;
    int N = Integer.parseInt(args[0]);
    Benchmark b = new Benchmark();
    long s = 0;
    double d[] = new double[N];
    for (int i = 0; i < N; ++i) {
      long l = b.bench(warmup, total);
      System.out.println(l);
      s += l;
      d[i] = l;
    }
    System.out.println("-----");
    System.out.println(s / N);
    bootstrap(d);
  }


  ////


  /**
   * <code>Bootstrap</code> - Basic bootstrap code for resampling means and computing confidence intervals.
   *
   * @author <a href="mailto:tneller@gettysburg.edu">Todd W. Neller</a>
   */
  public static class Bootstrap {
    private final double EPSILON = 1.0e-14;
    public Random rng = new Random();
    private int resamples = 1000000; // number of resamples performed in bootstrapping
    private double[] resampledStats = new double[resamples]; // collected statistics on resamples
    private boolean statsSorted = false; // whether or not resample statistics have been sorted yet

    public Bootstrap() {
    }

    public Bootstrap(int resamples) {
      this.resamples = resamples;
    }

    public int getResamples() {
      return resamples;
    }

    public void setResamples(int v) {
      this.resamples = v;
      resampledStats = new double[resamples];
    }

    public void resampleMean(double[] data) {
      statsSorted = false;
      int n = data.length;
      double[] resampledData = new double[n];
      for (int i = 0; i < resamples; i++) {
        double total = 0;
        for (int j = 0; j < n; j++) {
          resampledData[j] = data[rng.nextInt(n)];
          total += resampledData[j];
        }
        resampledStats[i] = total / n;
      }
    }

    public double getResampledStatMean() {
      double total = 0;
      for (int i = 0; i < resamples; i++)
        total += resampledStats[i];
      return total / resamples;
    }

    /**
     * <code>getResampledStatPercentile</code> - return indicated
     * percentile of resampled stats
     *
     * @param pct a <code>double</code> value - fraction of samples below
     *            return value (e.g. .1 -> 10%)
     * @return a <code>double</code> value - computed percentile value
     */
    public double getResampledStatPercentile(double pct) {
      if (!statsSorted) {
        Arrays.sort(resampledStats);
        statsSorted = true;
      }
      double exactIndex = pct * resamples;
      int index = (Math.abs(exactIndex - Math.round(exactIndex)) <= EPSILON)
              ? (int) Math.round(exactIndex)
              : (int) Math.floor(pct * (resamples + 1));
      // according to approximate procedure used in bootstrap-t
      // interval (pp. 160--161) in "An Introduction to the
      // Bootstrap" by Efron and Tibshirani
      index = Math.max(0, Math.min(resamples - 1, index - 1));
      return resampledStats[index];
    }

    public double[] getConfInt(double pct) {
      double lowPct = (1 - pct) / 2;
      double highPct = 1 - lowPct;
      return new double[]{getResampledStatPercentile(lowPct), getResampledStatPercentile(highPct)};
    }
  }

  public static void bootstrap(double data[]) {
    double confIntPct = .95; // confidence interval: .9 means "90% confidence", .95 means "95% confidence", etc.
    int numResamples = 1000000; // number of resamples for the bootstrap
    Bootstrap bs = new Bootstrap(numResamples); // Create bootstrap object

    bs.resampleMean(data);
    System.out.println("Resampled Mean: " + bs.getResampledStatMean() + ", "
            + (100 * confIntPct) + "% Confidence Interval: "
            + Arrays.toString(bs.getConfInt(confIntPct)));
  }
}
