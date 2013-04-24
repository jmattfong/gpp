package gpp.exp

import org.apache.log4j.Level
import org.apache.log4j.Logger
import nak.util.ConfusionMatrix
import scala.xml.XML


object Gpp {

    def main(args: Array[String]) {
        // parse options and automatically handle help option
        val opts = GppOpts(args)
        
        // handle version option
        if(opts.version()){
            println("Gpp version 0.1")
            exit(0)
        }
        
        // handle verbose option
        val logLevel = if (opts.verbose()) Level.DEBUG else Level.INFO
        Logger.getRootLogger.setLevel(logLevel)
        
        // handle train option
        val trainFiles = opts.train().map(XML.loadFile)
        // handle eval option
        val evalFiles = opts.eval().map(XML.loadFile)
        
        // not handled yet!!!!!!!!
        opts.cost()
        opts.detailed()
        opts.extended()
        
        // handle method option
        opts.method() match {
            case "majority" => Majority(trainFiles, evalFiles, opts.detailed())
            case "lexicon" =>
            case _ =>
        }
    }
}

/**
 * An object that sets of the configuration for command-line options using
 * Scallop and returns the options, ready for use.
 * 
 * Classification application.
 * 
 * For usage see below:
 * 
 *   -c, --cost  <arg>       The cost parameter C. Bigger values means less
 *                           regularization (more fidelity to the training set).
 *                           (default = 1.0)
 *   -d, --detailed          
 *   -e, --eval  <arg>...    The files containing evalualation events.
 *   -x, --extended          Use extended features.
 *   -m, --method  <arg>     The type of solver to use. Possible values: majority,
 *                           lexicon, or any liblinear solver type.
 *                           (default = L2R_LR)
 *   -t, --train  <arg>...   The files containing training events.
 *   -v, --verbose           
 *       --help              Show this message
 *       --version           Show version of this program
 */
object GppOpts {

    import org.rogach.scallop._
  
    def apply(args: Array[String]) = new ScallopConf(args) {
        banner("""
Classification application.

For usage see below:
	     """)

    val cost = opt[Double]("cost", short='c', default=Some(1.0), descr="The cost parameter C. Bigger values means less regularization (more fidelity to the training set).")
    val detailed = opt[Boolean]("detailed", short='d')
    val eval = opt[List[String]]("eval", short='e', descr="The files containing evalualation events.")
    val extended = opt[Boolean]("extended", short='x', default=Some(false), descr="Use extended features.")
    val method = opt[String]("method", short='m', default=Some("L2R_LR"), descr="The type of solver to use. Possible values: majority, lexicon, or any liblinear solver type.")
    val train = opt[List[String]]("train", short='t', descr="The files containing training events.")
    val verbose = opt[Boolean]("verbose", short='v', default=Some(false))
    val help = opt[Boolean]("help", noshort=true, descr="Show this message")
    val version = opt[Boolean]("version", noshort=true, descr="Show version of this program")
  }
}