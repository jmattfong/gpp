package gpp.exp

object Gpp {

    def main(args: Array[String]) {
        val opts = GppOpts(args)
    }
  
}


/**
 * An object that sets of the configuration for command-line options using
 * Scallop and returns the options, ready for use.
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