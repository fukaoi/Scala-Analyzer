/**
 * Analyzer.scala
 * This class Morphological analysis, occurrence frequency
 *                                   of words does display
 * 形態素解析、単語出現リスト表示クラス
 * 
 * @author fukaoi
 * @version 0.1 
 */
package blojsomscala

import net.moraleboost.mecab.{ Node }
import net.moraleboost.mecab.impl.StandardTagger
import scala.util.matching.Regex
import scala.io.Source
import com.sun.jna._

object Analyzer {

  /**
   * Main method
   * メインメソッド
   */
  def main(args: Array[String]) {
    val words = Source.fromFile(args(0)).getLines.mkString
    val tagger = new StandardTagger("UTF-8", "")
    val node = tagger.parse(words)
    val regex = """名詞""".r

    var mapWordLists = Map[Int, String]()
    var i = 0

    while (node.hasNext) {
      val feature = node.next
      regex findPrefixOf node.feature match {
        case Some(v) => mapWordLists += (i -> feature)
        case None => 
      }
      i = i + 1
    }
    val sortWordLists = getMostShowWord(mapWordLists)
    display(sortWordLists)
  }

  /**
   * Get Occurrence frequency of word in Scala List
   * 単語の出現頻度をリストにして返す
   */
  def getMostShowWord(wordList: Map[Int, String]): Seq[(String, Int)] = {
    wordList.groupBy(x => x._2).
      map(y => (y._1, y._2.size)).
      toSeq.sortWith(_._2 > _._2)
  }

  /**
   * Setting in JNA for C Library
   * CライブラリをJava JNAにセットする
   */
  def setNativeLibrary(colorName: String) {
    val lib = NativeLibrary.getInstance("libfont_style.so")
    val func = lib.getFunction("font_color")
    func.invokeVoid(Array(colorName))
  }

  /**
   * Anaalytics data to display on console 
   * コンソール結果に集計した結果を出力する
   */
  def display(list: Seq[(String, Int)]) {
    val charCode = "Shift-JIS"
    var maxLength = 0
    list.map(x => if (x._1.getBytes(charCode).length > maxLength) maxLength = x._1.getBytes(charCode).length)
    list.map(x => (setNativeLibrary(getColorNumber(x._2)),
      print(x._1),
      print(" " * (maxLength - x._1.getBytes(charCode).length)),
      print("+" * x._2),
      print("\n")))
  }

  /**
   * Get color code for C Library(setNativeLibrary)
   * Cライブラリ(setNativeLibrary)に渡す
   * カラーコードを取得する
   */
  def getColorNumber(count: Int): String = {
    val colors = Map((1 -> "blue"),
      (2 -> "cyan"),
      (3 -> "green"),
      (4 -> "magenta"),
      (5 -> "red"))
    if (count >= 6)
      "yellow"
    else
      colors(count)
  }
}

