import scala.collection.mutable
import scala.io.Source
import java.util.Scanner

object apriori {

  def main(args: Array[String]) {
    val minSupportCount = 2
    val data = new mutable.MutableList[Set[String]]
    val input = new Scanner(Source.fromFile("apriori.txt").bufferedReader())

    while (input.hasNext) {
      val line = new mutable.HashSet[String]()
      val lineScanner = new Scanner(input.nextLine())
      while (lineScanner.hasNext) {
        line += lineScanner.next("\\w+")
      }
      data += line.toSet
    }
    val freqItemSets = apriori_gen(data, minSupportCount)

    freqItemSets.foreach(set =>
      println(" " + set + " subset: " + set.subsets.toList)
    )
  }

  def apriori_gen(data: Seq[Set[String]], minSupportCount: Int): Seq[Set[String]] = {
    var itemSet = data.flatten.map({str =>
      val set = new mutable.HashSet[String]()
      set += str
      set.toSet
    }).toSet
    println("Item " + itemSet.toList)

    val result = new mutable.MutableList[Set[String]]

    while (true) { // loop for 1,2,3..n length item lists

      // scan D for count of each candidate
      val count = new mutable.HashMap[Set[String], Int]
      itemSet.foreach({ item =>
        data.foreach({ transaction =>
          if (transaction.intersect(item).size == item.size) {
            count.put(item, count.getOrElse(item, 0) + 1)
          }
        })
      })
      println("Count " + count.toList) // show count of each candidate

      // compare candidate support count with minimum support count
      val freqItemSets = count.filter(frequency => frequency._2 >= minSupportCount).keys
      if (freqItemSets.isEmpty) {
        return result.filter(set => set.size > 1) // show only non-null sets
      }
      result++=freqItemSets

      // Join step: create new candidates one level deeper, only add items of the new set if the
      // new set is exactly 1 level larger than the intersection of the two previous sets
      val newItemSet = new mutable.HashSet[Set[String]]
      freqItemSets.foreach({ set1 =>
        freqItemSets.foreach({ set2 =>
          if (set1.size - set1.intersect(set2).size == 1) {
            newItemSet += set1.union(set2)
          }
        })
      })
      itemSet = newItemSet.toSet // iterative step, placed new larger set in the current set and repeat loop
    }
    null // never executed, looping forever
  }
}
