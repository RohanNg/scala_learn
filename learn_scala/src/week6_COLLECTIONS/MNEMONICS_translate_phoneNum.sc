package week6_COLLECTIONS
import scala.io.Source

object x {
	// Get text file containing all the words in dic
  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
                                                  //> in  : scala.io.BufferedSource = non-empty iterator
  // The list of all word which contain letter a -> z
  val words = in.getLines.toList filter (x => x forall (y => y.isLetter))
                                                  //> words  : List[String] = List(Aarhus, Aaron, Ababa, aback, abaft, abandon, ab
                                                  //| andoned, abandoning, abandonment, abandons, abase, abased, abasement, abasem
                                                  //| ents, abases, abash, abashed, abashes, abashing, abasing, abate, abated, aba
                                                  //| tement, abatements, abater, abates, abating, Abba, abbe, abbey, abbeys, abbo
                                                  //| t, abbots, Abbott, abbreviate, abbreviated, abbreviates, abbreviating, abbre
                                                  //| viation, abbreviations, Abby, abdomen, abdomens, abdominal, abduct, abducted
                                                  //| , abduction, abductions, abductor, abductors, abducts, Abe, abed, Abel, Abel
                                                  //| ian, Abelson, Aberdeen, Abernathy, aberrant, aberration, aberrations, abet, 
                                                  //| abets, abetted, abetter, abetting, abeyance, abhor, abhorred, abhorrent, abh
                                                  //| orrer, abhorring, abhors, abide, abided, abides, abiding, Abidjan, Abigail, 
                                                  //| Abilene, abilities, ability, abject, abjection, abjections, abjectly, abject
                                                  //| ness, abjure, abjured, abjures, abjuring, ablate, ablated, ablates, ablating
                                                  //| , ablation, ablative, ab
                                                  //| Output exceeds cutoff limit.
	// mnemonic in a phone
  val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI",
    '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS",
    '8' -> "TUV", '9' -> "WXYZ")                  //> mnem  : scala.collection.immutable.Map[Char,String] = Map(8 -> TUV, 4 -> GHI
                                                  //| , 9 -> WXYZ, 5 -> JKL, 6 -> MNO, 2 -> ABC, 7 -> PQRS, 3 -> DEF)
	
	// map letter to number associated with an a phone
  val charCode: Map[Char, Char] =
    for ((num, letters) <- mnem; letter <- letters) yield letter -> num
                                                  //> charCode  : Map[Char,Char] = Map(E -> 3, X -> 9, N -> 6, T -> 8, Y -> 9, J -
                                                  //| > 5, U -> 8, F -> 3, A -> 2, M -> 6, I -> 4, G -> 4, V -> 8, Q -> 7, L -> 5,
                                                  //|  B -> 2, P -> 7, C -> 2, H -> 4, W -> 9, K -> 5, R -> 7, O -> 6, D -> 3, Z -
                                                  //| > 9, S -> 7)
  
  /** Map a word to a ditgit string it can represent  For exanple: scala -> 72252*/
  def wordCode(word: String): String = word.toUpperCase map charCode
                                                  //> wordCode: (word: String)String
	
	/** A map from a digit string a list of word that represent them. For example: 72252 -> List("scala","rcala","pcala","rcala") */
	val wordsForNum: Map[String, Seq[String]] =
		words groupBy wordCode withDefaultValue Seq()
  /** Encode a number to the meaningful string representing it */
	def encode(number: String): Set[List[String]] =
		if (number.isEmpty) Set(List())
		else {
			for {
				split <- 1 to number.length
				word	<- wordsForNum(number take split)
				rest 	<- encode(number drop split)
			} yield word::rest
		}.toSet
  
  def translate(number: String): List[String] =
  	(encode(number) map (_ mkString " ")).toList
  
  translate("7225247386")
}