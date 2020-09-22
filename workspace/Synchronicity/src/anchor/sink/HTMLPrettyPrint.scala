//
//
//package anchor.fe
//
////import anchor.transforms.CanonicalProgram
//
//class HTMLPrettyPrint extends PrettyPrint {
//  override def newLine(): String = {
//      """</span>
//        |<span class="pl-c">""".stripMargin + ("  " * indentLevel)
//  }
//
//}
//
//
//object HTMLPrettyPrint {
//  def pp(x: Program): String = {
//    """
//      |<link rel="stylesheet" href="github-highlight.css" />
//      |<div class="highlight highlight-source-swift"><pre>
//      |<span class="pl-c">""".stripMargin +
//      (new HTMLPrettyPrint().pp(x)) +
//      """
//        |</pre></div>
//      """.stripMargin
//  }
//
//  def pp(x: MethodDecl): String = {
//    """
//      |<link rel="stylesheet" href="github-highlight.css" />
//      |<div class="highlight highlight-source-swift"><pre>
//      |<span class="pl-c">""".stripMargin +
//      (new HTMLPrettyPrint().pp(x)) +
//      """
//        |</pre></div>
//      """.stripMargin
//  }
//}
