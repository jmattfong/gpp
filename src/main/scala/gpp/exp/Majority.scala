package gpp.exp

import nak.util.ConfusionMatrix
import scala.xml.Elem

object Majority {

    def apply(train: List[Elem], eval: List[Elem], detailed: Boolean) {
        val trainLabels = (for(file <- train)
            yield (file \\ "item").map(item => (item \ "@label").text).toList
        ).flatten
        val trainText = (for(file <- train)
            yield (file \\ "content").map(_.text).toList
        ).flatten
        
        val majorityLabel = trainLabels.groupBy(x=>x).mapValues(_.length).toList.sortBy(-_._2).head._1
        println(majorityLabel)
        
        
    }
}
