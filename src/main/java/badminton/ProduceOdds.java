package badminton;

import com.google.common.collect.ImmutableMap;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVRecord;

import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.net.URL;
import java.util.AbstractMap;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.stream.StreamSupport;

import static java.util.stream.Collectors.toMap;

public class ProduceOdds {
    private static final ImmutableMap<String, Integer> rating = new ImmutableMap.Builder<String, Integer>()
            .put("Skubic Sr.", 2842)
            .put("Stanič", 2814)
            .put("Dovgan", 2758)
            .put("Urša", 2757)
            .put("Skubic Jr.", 2756)
            .put("Krkač", 2728)
            .put("Rebolj", 2671)
            .put("Kulaš", 2620)
            .put("Perpar", 2614)
            .put("Lečnik", 2593)
            .put("Rapoša", 2575)
            .put("Boštic", 2571)
            .put("Ana", 2545)
            .put("Suša", 2523)
            .put("Mekuč", 2479)
            .put("Jerič", 2457)
            .put("Lovenjak", 2439)
            .put("Arnšek", 2434)
            .put("Božiček", 2427)
            .put("Uršič", 2391)
            .put("Trobec", 2374)
            .put("Žnidaršič", 2334)
            .put("Vidmar", 2238)
            .put("Logožar", 2230)
            .put("Grzinčič", 2207)
            .put("Gergar", 2047)
            .build();

    public static void main(String[] args) throws IOException {
        Map<String, Map<String, Double>> h2hOdds = h2hOdds();
        Map<String, Map<String, Double>> ratingOdds = ratingOdds();

        Map<String, Map<String, Double>> finalOdds = zip(
                h2hOdds,
                ratingOdds,
                (innerLeft, innerRight) -> zip(
                        innerLeft,
                        innerRight,
                        (h2hOdd, ratingOdd) -> (h2hOdd + ratingOdd) / 2
                )
        );

        //Output:
        finalOdds.forEach(
                (left, right) -> {
                    System.out.println(left);

                    right.entrySet().stream().forEach(System.out::println);
                }
        );
    }

    private static <V> Map<String, V> zip(Map<String, V> left, Map<String, V> right, BiFunction<V, V, V> zipperF) {
        return left
                .entrySet()
                .stream()
                .collect(toMap(
                        Map.Entry::getKey,
                        leftE -> zipperF.apply(leftE.getValue(), right.get(leftE.getKey()))
                ));
    }

    //Head-to-head odds

    private static Map<String, Map<String, Double>> h2hOdds() throws IOException {
        URL resource = ProduceOdds.class.getClassLoader().getResource("h2h-odds.csv");

        try (Reader in = new FileReader(resource.getFile())) {
            Iterable<CSVRecord> records = CSVFormat.DEFAULT.parse(in);

            Map<String, Map<String, Double>> h2hOdds = new HashMap<>();
            boolean isHeader = true; String[] players = null;
            for (CSVRecord rec : records) {
                if (isHeader) {
                    players = StreamSupport.stream(rec.spliterator(), false).toArray(String[]::new);
                    isHeader = false;
                } else {
                    Iterator<String> recIt = rec.iterator();
                    String leftPlayer = recIt.next();

                    Map<String, Double> playerOdds = new HashMap<>();
                    int idx = 1;
                    while (recIt.hasNext()) {
                        String rightPlayer = players[idx++];
                        String oddsText = recIt.next();
                        if (oddsText == null || oddsText.isEmpty()) {
                            playerOdds.put(rightPlayer, 2.0);
                        } else if (oddsText.equals("#DIV/0!")) {
                            playerOdds.put(rightPlayer, -1.0);
                        } else {
                            playerOdds.put(rightPlayer, Double.parseDouble(oddsText));
                        }
                    }

                    h2hOdds.put(leftPlayer, playerOdds);
                }
            }

            return h2hOdds;
        }
    }
    //Rating odds

    private static Map<String, Map<String, Double>> ratingOdds() {
        return rating
                .entrySet()
                .stream()
                .map(ProduceOdds::outerMerge)
                .collect(toMap(Map.Entry::getKey, Map.Entry::getValue));
    }

    private static Map.Entry<String, Map<String, Double>> outerMerge(Map.Entry<String, Integer> left) {
        return new AbstractMap.SimpleImmutableEntry<>(
                left.getKey(),
                rating
                        .entrySet()
                        .stream()
                        .map(right -> innerMerge(left, right))
                        .collect(toMap(Map.Entry::getKey, Map.Entry::getValue))
        );
    }

    private static Map.Entry<String, Double> innerMerge(
            Map.Entry<String, Integer> left,
            Map.Entry<String, Integer> right
    ) {
        return new AbstractMap.SimpleImmutableEntry<>(
                right.getKey(),
                1 / probability(left.getValue(), right.getValue())
        );
    }

    static double probability(int rating0, int rating1) {
        int diff = rating1 - rating0;
        double prob = probabilityPercent(Math.abs(diff)) / 100;

        return diff >= 0 ? prob : 1 - prob;
    }

    //=50.2 - 0.247*'elo-diffs'!F6 + 0.000152 * (-1 + 3 * 'elo-diffs'!F6^2) - 0.0000000667 * (-3 * 'elo-diffs'!F6 + 5 * 'elo-diffs'!F6^3) + 0.00000000000161 * (3 - 30 * 'elo-diffs'!F6^2 + 35*'elo-diffs'!F6^4)
    static double probabilityPercent(int diff) {
        return 50.2
                - 0.247*diff
                + 0.000152*(-1 + 3*Math.pow(diff, 2))
                - 0.0000000667*(-3*diff + 5*Math.pow(diff, 3))
                + 0.00000000000161*(3 - 30*Math.pow(diff, 2) + 35*Math.pow(diff, 4));
    }
}
