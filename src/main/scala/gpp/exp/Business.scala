package gpp.exp

import chalk.lang.eng.PorterStemmer
import chalk.lang.eng.Twokenize
import nak.data._
import nak.util.ConfusionMatrix
import nak.liblinear.LiblinearConfig
import nak.NakContext._
import scala.xml.Elem

/** The business classifier used to classify snippets of New York Times articles and their expected investment return. */
object Business {

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
    def apply(train: List[Elem], eval: List[Elem], costValue: Double, detailed: Boolean) {
        val trainLabels = (for(file <- train) yield
            (file \\ "item").map(item => (item \ "@label").text).toList
        ).flatten
        val trainText = (for(file <- train) yield
            (file \\ "content").map(_.text).toList
        ).flatten
        
        val evalLabels = (for(file <- eval) yield
            (file \\ "item").map(item => (item \ "@label").text).toList
        ).flatten
        val evalText = (for(file <- eval) yield
            (file \\ "content").map(_.text).toList
        ).flatten

        lazy val featurizer = new Featurizer[String, String] {
            def apply(input: String) = {
                val originalTokens = Twokenize(input)
                val tokens = originalTokens.map(_.toLowerCase).map(stemmer(_))
                val wordCounts = tokens.groupBy(x=>x).mapValues(_.length).toList
                val basicFeatures = for ((word, count) <- wordCounts)
                    yield FeatureObservation(word+"="+count)
                val polarity = List(FeatureObservation("polarity="+getSentiment(input)))
                val dollarSign = List(FeatureObservation("dollar="+input.count(_ == '$')))
                (basicFeatures ++ polarity ++ dollarSign)
            }
        }

        val trainExamples = for ((label, text) <- trainLabels.zip(trainText))
            yield Example(label, text)

        val config = LiblinearConfig(cost=costValue)
        val classifier = trainClassifier(config, featurizer, trainExamples)

        def maxLabelPpa = maxLabel(classifier.labels) _
        
        val predictions = for(text <- evalText) yield maxLabelPpa(classifier.evalRaw(text))

        val cm = ConfusionMatrix(evalLabels, predictions, evalText)
        println(cm)
        if(detailed)
            println(cm.detailedOutput)
    }

    /** Determines the sentiment polarity of provided text
      *
      * @param text string containing the text, which the method determines the sentiment of
      * @return string of the sentiment polarity of the text
      */
    def getSentiment(text: String): Double = {
        val words = AlphaNumericTokenizer(text)
        var numPos = if(polarity.posWords.contains(words(0))) 1 else 0
        var numNeg = if(polarity.negWords.contains(words(0))) 1 else 0
        for(wordSet <- words.sliding(2)){
            val negate = English.negationWords.contains(wordSet(0))
            if (polarity.posWords.contains(wordSet(1))){
                if(negate) numNeg += 1
                else numPos += 1
            }
            if (polarity.negWords.contains(wordSet(1))){
                if(negate) numPos += 1
                else numNeg += 1
            }
        }
        val sentiment = if (numPos != 0) { numPos.toDouble / (numPos + numNeg) } else 0
        sentiment
    }
}
