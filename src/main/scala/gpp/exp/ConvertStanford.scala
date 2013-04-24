package gpp.exp

object ConvertStanford {

    val labels = List("label", "tweetid", "", "target", "username")

    def main(args : Array[String]) {
        val lines = scala.io.Source.fromFile(args(0)).getLines
        println("<?xml version=\"1.0\"?>\n<dataset>")
        lines.foreach { line =>
            val parts = line.split(";;")
            val label = parts(0) match {
                case "0" => "negative"
                case "2" => "neutral"
                case _   => "positive"
            }
            parts(0) = label
            print("<item ")
            val itemLabels = (for((s, i) <- labels.zipWithIndex) yield {
                if(i != 2)
                    print(s + "=\"" + parts(i) + "\" ")
            })
            println(">")
            println("<content>" + parts(5) + "</content>")
            println("</item>")
            
        }
        println("</dataset>")
    }
}