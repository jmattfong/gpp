package gpp.exp

object ConvertEmoticon {
    def main(args: Array[String]) {
        val tweetRE = """^(\d+)\s(\d+)\s(.*)$""".r

        val happyLines = scala.io.Source.fromFile(args(0)+"/happy.txt").getLines
        val sadLines = scala.io.Source.fromFile(args(0)+"/sad.txt").getLines
        val neutralLines = scala.io.Source.fromFile(args(0)+"/neutral.txt").getLines

        println("""<?xml version="1.0"?>""")
        println("<dataset>")

        for (tweetRE(_, _, tweet) <- happyLines) {
            println("\t" + """<item label="positive">""")
            print("\t\t<content>")
            print(tweet.replaceAll("&", "&amp;"))
            println("</content>")
            println("\t</item>")
        }

        for (tweetRE(_, _, tweet) <- sadLines) {
            println("\t" + """<item label="negative">""")
            print("\t\t<content>")
            print(tweet.replaceAll("&", "&amp;"))
            println("</content>")
            println("\t</item>")
        }

        for (tweetRE(_, _, tweet) <- neutralLines) {
            println("\t" + """<item label="neutral">""")
            print("\t\t<content>")
            print(tweet.replaceAll("&", "&amp;"))
            println("</content>")
            println("\t</item>")
        }

        println("</dataset>")
    }
}
