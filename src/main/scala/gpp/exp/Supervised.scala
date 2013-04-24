package gpp.exp

import chalk.lang.eng.PorterStemmer
import chalk.lang.eng.Twokenize
import nak.data._
import nak.util.ConfusionMatrix
import nak.liblinear.LiblinearConfig
import nak.NakContext._
import scala.xml.Elem

object Supervised {

    lazy val stemmer = new PorterStemmer

    def apply(train: List[Elem], eval: List[Elem], costValue: Double, extended: Boolean, detailed: Boolean) {
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

        lazy val basicFeaturizer = new Featurizer[String, String] {
            def apply(input: String) = {
                val tokens = Twokenize(input)
                val features = tokens.groupBy(x=>x).mapValues(_.length).toList
                for ((word, count) <- features)
                    yield FeatureObservation(word+"="+count)
            }
        }

        lazy val extendedFeaturizer = new Featurizer[String, String] {
            def apply(input: String) = {
                val originalTokens = Twokenize(input)
                val tokens = originalTokens.map(_.toLowerCase).map(stemmer(_))
                val wordCounts = tokens.groupBy(x=>x).mapValues(_.length).toList
                val basicFeatures = for ((word, count) <- wordCounts)
                    yield FeatureObservation(word+"="+count)
                val polarity = List(FeatureObservation("polarity="+getSentiment(input)))
                (basicFeatures ++ polarity)
            }
        }

        val featurizer = if (extended) extendedFeaturizer else basicFeaturizer

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

    def getSentiment(text: String): String = {
        val tokens = Twokenize(text)
        val polarity = English.getPolarity(tokens)
        return polarity match {
            case 0 => "positive"
            case 1 => "negative"
            case 2 => "neutral"
        }
    }
}
