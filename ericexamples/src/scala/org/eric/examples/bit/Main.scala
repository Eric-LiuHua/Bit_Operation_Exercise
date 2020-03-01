package org.eric.examples.bit


/**
 * Created by liuhua on 2017/10/11.
 */
object Main {
  def main(args: Array[String]): Unit = {

    val x1 = 1
    val x2 = 2
    val x3 = 3
    val x4 = 6
    val x5 = 8


    println(s"isood:$x1:${BitOperation.isood(x1)},$x2:${BitOperation.isood(x2)},$x3:${BitOperation.isood(x3)}")
    println(s"log2:$x1:${BitOperation.log2(x1)},$x4:${BitOperation.log2(x4)},$x5:${BitOperation.log2(x5)}")

    println(s"-1,count1:${BitOperation.count1(-1)},1,count1:${BitOperation.count1(1)}")
    println(s"-1,count2:${BitOperation.count2(-1)},1,count2:${BitOperation.count2(1)}")

    var ax =Array[Int](1,2,3,4,2,1,3,4,5,7,7,6,5)
    println(s"集合中只出现一次的数字,oddtimesnum:${BitOperation.oddTimesNum(ax)} ")


    var bx =Array[Int](1,2,3,4,2,1,3,4,5,7,7,6,5,9)
    BitOperation.printOddTimesNum(ax)
    BitOperation.printOddTimesNum(bx)

    BitOperation.exchange
  }

}
