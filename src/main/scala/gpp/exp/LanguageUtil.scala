package gpp.exp

/**
 * Copyright 2013 Jason Baldridge
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * A helper object for creating Scala Source instances given a
 * the location of a resource in the classpath, which includes
 * files in the src/main/resources directory.
 */
object Resource {
  import java.util.zip.GZIPInputStream
  import java.io.DataInputStream

  /**
   * Read in a file as a Source, ensuring that the right thing
   * is done for gzipped files.
   */
  def asSource(location: String) = {
    val stream = this.getClass.getResourceAsStream(location)
    if (location.endsWith(".gz"))
      io.Source.fromInputStream(new GZIPInputStream(stream))
    else
      io.Source.fromInputStream(stream)
  
  }

  def asStream(location: String) = {
    val stream = this.getClass.getResourceAsStream(location)
    val stream2 = if (location.endsWith(".gz")) new GZIPInputStream(stream) else stream
    new DataInputStream(stream2)
  }
}

/**
 * A parent class for specific languages. Handles some common
 * functions.
 */
abstract class Language(code: String) {
  def stopwords: Set[String]
  def vocabulary: Set[String]

  lazy val resourceDir = "/lang/" + code
  def appendPath(subdir: String) = resourceDir + subdir
  def getLexicon(filename: String) = 
    Resource.asSource(appendPath("/lexicon/"+filename))
      .getLines
      .filterNot(_.startsWith(";")) // filter out comments
      .toSet

}

/**
 * English information.
 */
object English extends Language("eng") {

  lazy val random = new scala.util.Random

  lazy val stopwords = getLexicon("stopwords.english")
  lazy val vulgar = getLexicon("vulgar.txt.gz")
  lazy val stopwords_bot = getLexicon("stopwords.bot")
  lazy val vocabularyTWSS = getLexicon("TWSSVocab.txt");
  lazy val vocabulary = getLexicon("masc_vocab.txt.gz") ++ stopwords
  lazy val thesaurus = Thesaurus.read(Resource.asStream("/lang/eng/lexicon/oo_eng_thesaurus.gz"))
  // words that might indicate the polarity of the words that follow should be flipped
  // example: no, not, *n't
  lazy val negationWords = getLexicon("negationWords.txt")
  lazy val posWords = getLexicon("positive-words.txt.gz")
  lazy val negWords = getLexicon("negative-words.txt.gz")

  def isEnglish(text: String) = {
    val words = SimpleTokenizer(removeNonLanguage(text).toLowerCase)
    val count = words.count(vocabulary) 
    count > 1 && count.toDouble/words.length > .3
  }

  def removeNonLanguage(text: String) =
    text.replaceAll("[@#][A-Za-z_]+","")
      .replaceAll("""http[^\s]+""","")
      .replaceAll("\\s+"," ")

  def isSafe(text: String) =  {
    val words = SimpleTokenizer(removeNonLanguage(text).toLowerCase)
    words.count(vulgar) == 0
  }

  // Use this to pull punctuation back next to the word before it.
  lazy val SpacePuncRE = """ ([\.!?,])""".r

  def synonymize(text: String) = {
    val synTokens = SimpleTokenizer(text).map { token => {
      if (!stopwords(token)) {
        val syns = thesaurus.synonyms(token)
        val numSyns = syns.size
        if (numSyns > 0) syns.take(random.nextInt(numSyns)+1).last
        else token
      } else {
        token
      }
    }}
    SpacePuncRE.replaceAllIn(synTokens.mkString(" "), "$1")
  }

  // Use this to get a Set of synonyms for a given word.
  def synonymize(token: String, length: Int):Set[String] = {
    val synTokens = if (!stopwords(token)) {
        val syns = thesaurus.synonyms(token)
        val numSyns = syns.size
        if (numSyns > 0) syns.take(length)
        else Set(token)
      } else {
        Set(token)
      }
    synTokens
  }

  def getPolarity(words: List[String]) = {
    val numPositive = words.count(posWords.contains(_))
    val numNegative = words.count(negWords.contains(_))
    if(numPositive > numNegative) 0 else if (numPositive < numNegative) 1 else 2
  }
}

/**
 * A thesaurus, mapping words to thesaurus entries.
 */
class Thesaurus(val entries: Map[String,ThesaurusEntry]) {

  /**
   * Get the ThesaurusEntry associated with a word.
   */ 
  def apply(word: String) = entries.get(word)

  /**
   * Get all the synonyms associated with a word.
   */ 
  def synonyms(word: String): Set[String] = entries.get(word) match {
    case Some(entry) => entry.senseGroups.flatMap(_.words).toSet
    case None => Set[String]()
  }

  /**
   * Get all the synonyms associated with a word and given part-of-speech.
   */ 
  def synonyms(word: String, pos: String) = entries.get(word) match {
    case Some(entry) => entry.senseGroups.filter(_.pos == pos).flatMap(_.words).toSet
    case None => Set[String]()
  }

}

class English extends Language("eng") {
  lazy val stopwords = getLexicon("stopwords.english")
  lazy val vocabulary = getLexicon("masc_vocab.txt.gz") ++ stopwords
}
 
/**
 * Companion object to help with creating, writing and reading thesauruses.
 */
object Thesaurus {

  import java.io._
  import java.util.zip._

  /**
   * Create a thesaurus from a sequence of thesaurus entries.
   */
  def apply(entries: Seq[ThesaurusEntry]) = {
    new Thesaurus(entries.map(e => (e.word -> e)).toMap)
  }

  /**
   * Save a thesaurus compactly to disk.
   */
  def write(thesaurus: Thesaurus, filename: String) {
    import java.io._
    import java.util.zip._
    val output = new DataOutputStream(new GZIPOutputStream(new FileOutputStream(filename)))
    output.writeInt(thesaurus.entries.size)
    thesaurus.entries.values.foreach { entry => {
      output.writeUTF(entry.word)
      output.writeInt(entry.senseGroups.length)
      entry.senseGroups.foreach { group => {
        output.writeUTF(group.pos)
        output.writeInt(group.words.size)
        group.words.foreach(output.writeUTF)
      }}
    }}
    output.close
  }

  /**
   * Read a compactly stored thesaurus.
   */
  def read(input: DataInputStream) = {
    val numEntries = input.readInt
    val entries = for (entryId <- 0 until numEntries) yield {
      val word = input.readUTF
      val numSenses = input.readInt
      val senseGroups = for (senseId <- 0 until numSenses) yield {
        val pos = input.readUTF
        val numWords = input.readInt
        val words = for (wordId <- 0 until numWords) yield input.readUTF
        SenseGroup(pos, words.toSet)
      }
      ThesaurusEntry(word, senseGroups)
    }
    apply(entries)
  }

  def main (args: Array[String]) {
    println(English.synonymize("the dog and the cat walk past the tree"))
  }

}

/**
 * A thesaurus entry: the word and a sequence of groups of synonyms for
 * each sense of the word.
 */
case class ThesaurusEntry(word: String, senseGroups: Seq[SenseGroup])

/**
 * A group of synonyms that share a word sense and part-of-speech.
 */
case class SenseGroup(pos: String, words: Set[String])


/**
 * Take the raw Open Office thesaurus, filter it to retain only words in
 * the known vocabulary, and write to disk in compressed format.
 */
object OpenOfficeThesaurusConverter {

  import collection.mutable.ListBuffer
  val vocab = English.vocabulary

  // Map the OO pos tags to standard course-grained tags.
  def mapPos (ooPos: String) = ooPos match {
    case "(noun)" => "N"
    case "(verb)" => "V"
    case "(adj)" => "A"
    case "(adv)" => "R"
    case _ => throw new Exception("Unknown part-of-speech: " + ooPos)
  }

  def main(args: Array[String]) = {
    
    val Array(thesaurusFile, outputFile) = args
    val thesaurusLines = io.Source.fromFile(thesaurusFile).getLines

    val entries = new ListBuffer[ThesaurusEntry]()
    while (thesaurusLines.hasNext) {
      val wordLine = thesaurusLines.next
      val Array(word, countString) = wordLine.split("\\|")
      val count = countString.toInt
      var senseCounter = 0
      val senseList = new ListBuffer[SenseGroup]()
      while (senseCounter < count) {
        val senseLine = thesaurusLines.next
        val pos :: synonyms = senseLine.split("\\|").map(_.trim).toList
        senseCounter += 1
        senseList += SenseGroup(mapPos(pos), synonyms.toSet)
      }

      if (vocab(word))
        entries += ThesaurusEntry(word, senseList.toSeq)
    }
    val thesaurus = Thesaurus(entries.toSeq)
    Thesaurus.write(thesaurus, outputFile)
  }

}

abstract class OtherLexica (code: String) {

  lazy val resourceDir = "/lang/" + code
  def appendPath(subdir: String) = resourceDir + subdir
  def getLexicon(filename: String) = 
    Resource.asSource(appendPath("/lexicon/"+filename))
      .getLines
      .filterNot(_.startsWith(";")) // filter out comments
      .toSet

}

class Polarity extends OtherLexica("eng") {
  lazy val posWords = getLexicon("positive-words.txt.gz")
  lazy val negWords = getLexicon("negative-words.txt.gz")
}
object TWSSModel {
  import de.bwaldvogel.liblinear._; 
  import java.io.File
  lazy val model:Model = Model.load(new File("src/main/resources/TWSS/TWSS.model"))
  def apply() : Model = model
}

/**
 * An object that manages information from resource files nasdaq.csv and nyse.csv
 */
object CompanyData {
    import gpp.exp.AlphaNumericTokenizer
    import scala.collection.mutable
    import com.github.tototoshi.csv._
    import java.io.File

    lazy val resourceDir = "/companies/"

    def readCSVFile(filename: String): (Map[String, Int], List[List[String]]) = {
        val reader = CSVReader.open(new File("data/companies/" + filename))
        val file = reader.all()
        reader.close()
        val header = file.head.zipWithIndex.toMap
        val body = file.tail
        (header, body)
    }

    def compMap(header: Map[String, Int], body: List[List[String]], key: String, valName: String): Map[String, String] = {
        (for(line <- body) yield {
            val k = line(header.getOrElse(key, 0))
            val v = line(header.getOrElse(valName, 0))
            (k, v)
        }).toMap ++ List((key, key)).toMap
    }

    lazy val (amexHeader, amexFile) = readCSVFile("AMEX.csv")
    lazy val (nasdaqHeader, nasdaqFile) = readCSVFile("NASDAQ.csv")
    lazy val (nyseHeader, nyseFile) = readCSVFile("NYSE.csv")
    lazy val header = nyseHeader // trust that they are all the same
    lazy val file = amexFile ++ nasdaqFile ++ nyseFile

    // A map of stock symbols to company names
    lazy val symToComp: mutable.Map[String, String] = mutable.Map() ++ compMap(header, file, "Symbol", "Name")
    // A map that takes a word to a list of stock symbols with that word in their
    // corresponding company's name
    lazy val invertedIndex: mutable.Map[String, Set[String]] = mutable.Map() ++
        (for ((sym, comp) <- symToComp) yield {
            val fixedSymbol = sym.split("\\^")(0).split("/")(0).trim()
            val symMap = List((fixedSymbol.toLowerCase, fixedSymbol))
            val splitCompName = AlphaNumericTokenizer(comp)
            (for (word <- splitCompName) yield (word.toLowerCase(), fixedSymbol)) ++ symMap
        }).flatten
        .groupBy(_._1)
        .mapValues(_.map(_._2).toSet)
        .filter(_._2.size < 100)
        .toMap

    /**
     * Updates our inverted index with new information
     * @param symbol the stock symbol of the company
     * @param compName the name of the company
     * @return A list of the updates that were made to invertedIndex
     */
    def updateIndex(symbol: String, compName: String): List[(String, Set[String])] = {
        val splitCompName = AlphaNumericTokenizer(compName)
        val indexUpdates = (for(word <- splitCompName) yield {
            val update = (word, invertedIndex.getOrElse(word, Set[String]()) + symbol)
            invertedIndex += update
            update })
        symToComp += ((symbol, compName))
        return indexUpdates
    }
    
    /**
     * Main method used for testing
     */
    def main (args: Array[String]) {
        println(invertedIndex.toList.sortBy(_._1).mkString(", "))
    }
}
