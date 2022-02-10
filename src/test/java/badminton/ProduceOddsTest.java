package badminton;

import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class ProduceOddsTest {
    @Test
    public void ratingToProbability() {
        List<Double> actual = Arrays.asList(
                ProduceOdds.probabilityPercent(650), //1
                ProduceOdds.probabilityPercent(500), //2.5
                ProduceOdds.probabilityPercent(390), //5
                ProduceOdds.probabilityPercent(280), //10
                ProduceOdds.probabilityPercent(215), //15
                ProduceOdds.probabilityPercent(165), //20
                ProduceOdds.probabilityPercent(130), //25
                ProduceOdds.probabilityPercent(100), //30
                ProduceOdds.probabilityPercent(70),  //35
                ProduceOdds.probabilityPercent(47),  //40
                ProduceOdds.probabilityPercent(22),  //45
                ProduceOdds.probabilityPercent(0)    //50
        );

        actual.forEach(System.out::println);

//        double actual = ProduceOdds.probabilityPercent(650);
//
//        assertEquals(1.0, actual, 0.1);
    }

}
