package gpp.exp

import chalk.lang.eng.PorterStemmer
import chalk.lang.eng.Twokenize
import nak.data._
import nak.util.ConfusionMatrix
import nak.liblinear.LiblinearConfig
import nak.NakContext._
import scala.xml.Elem

/** The business classifier used to classify snippets of New York Times articles and their expected investment return. */
object WordRelevance {

    import gpp.exp.{English, Polarity, AlphaNumericTokenizer}
    lazy val stemmer = new PorterStemmer
    lazy val polarity = new Polarity()

    /** Runs the business classifier
      *
      * @param train a list of XML elements containing the training data
      * @param eval a list of XML elements containing the testing data
      * @param costValue the cost value of the classifier
      * @param detailed boolean to display verbose output
      * @param classifierFile string of the file name the classifier should save to
               classifier only saves to file if the length of classifierFile is greater than 0
      */
    def apply(train: List[Elem], eval: List[Elem]) {
        val trainLabels = (for(file <- train) yield
            (file \\ "item").map(item => (item \ "@label").text).toList
        ).flatten
        val trainText = (for(file <- train) yield
            (file \\ "content").map(_.text).toList
        ).flatten
        
        val evalLabels = (for(file <- eval) yield
            (file \\ "item").map(item => (item \ "@label").text).toList.map(_.toDouble)
        ).flatten
        val evalText = (for(file <- eval) yield
            (file \\ "content").map(_.text).toList
        ).flatten

        val trainExamples = for ((label, text) <- trainLabels.zip(trainText))
            yield (label, text)

        val classifier = new WordRelevanceClassifier()
        classifier.train(trainExamples)
        val predictions = classifier.test(evalText)
        val totalError = (for ((gold, predict) <- evalLabels.zip(predictions)) yield Math.abs(gold-predict)).sum
        val averageError = totalError / predictions.length

        println("Total Error: " + totalError + " Average Error: " + averageError)

        // Add later
        val cm = ConfusionMatrix(getLabels(evalLabels), getLabels(predictions), evalText)
        println(cm)
    }

    def getLabels(scores: List[Double]): List[String] = {
        val labels = for (score <- scores) yield {
            if (score > 0.005)
                "Good"
            else if (score > -0.005)
                "OK"
            else
                "Bad"
        }
        labels
    }
}

class WordRelevanceClassifier() {

    lazy val stemmer = new PorterStemmer
    val wordRelevance = scala.collection.mutable.Map[String, Double]().withDefaultValue(0.0)

    def train(trainExamples: List[(String, String)]) {
        // hold document counts for each word for normalization
        val documentCounts = scala.collection.mutable.Map[String, Int]().withDefaultValue(0)

        // get word relevance scores from all training examples
        for ((label, text) <- trainExamples) {
            val stockReturn = label.toDouble
            val tokens = Twokenize(text).map(_.toLowerCase).map(stemmer(_)).filterNot(English.stopwords.contains(_))
            val wordScores = tokens.groupBy(x=>x).mapValues(_.length).toList
            val totalScore = wordScores.map(_._2).sum
            for ((word, score) <- wordScores) {
                wordRelevance(word) += (score * stockReturn / totalScore)
                documentCounts(word) += 1
            }
        }

        // normalize word relevance scores
        for ((word, relevance) <- wordRelevance) {
            wordRelevance(word) /= documentCounts(word)
        }
    }

    def test(testExamples: List[String]): List[Double] = {
        val scores = for (text <- testExamples) yield {
            val tokens = Twokenize(text).map(_.toLowerCase).map(stemmer(_))
            val wordScores = tokens.groupBy(x=>x).mapValues(_.length).toList
            val textScore = (for ((word, score) <- wordScores) yield (score * wordRelevance(word))).sum
            textScore
        }
        scores
    }
}
