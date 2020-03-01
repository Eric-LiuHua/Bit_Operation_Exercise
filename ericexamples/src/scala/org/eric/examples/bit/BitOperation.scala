package org.eric.examples.bit


/**
 * 二进制操作学习
 */
object BitOperation {


  /**
   * 给定一个整数，请写一个函数判断该整数的奇偶性（✭✩✩✩✩）
   * 判断一个数是否奇数
   * 可是面试题不可能去简单就考察这么简单的解法，进而我们想到了二进制中如果
   * 一个数是偶数那么最后一个一定是 0
   * 一个数是奇数那么最后一位一定是 1；
   * 而十进制 1 在 8 位二进制中表示为 0000 0001，
   * 我们只需将一个数个 1相与（&） 得到的结果如果是 1 则表示该数为奇数，否知为偶数。
   * 所以这道题的最佳解法如下
   *
   * @param int
   * @return
   */
  def isood(int: Int): Boolean = (int & 1) != 0

  /**
   * 同样给定一个整数，请写一个函数判断该整数是不是2的整数次幂（✭✩✩✩✩）
   * 这道题仍旧考察面试者对于一个数的二进制的表示特点，一个整数如果是2的整数次幂，
   * 那么他用二进制表示完肯定有唯一一位为1其余各位都为 0，形如 0..0100...0。比如 8 是 2的3次幂，那么这个数表示为二进制位 0000 1000 。
   * 除此之外我们还应该想到，
   * 一个二进制如果表示为 0..0100...0，那么它减去1得到的数二进制表示肯定是 0..0011..1 的形式。
   * 那么这个数与自自己减一后的数相与得到结果肯定为0。
   *
   * @param int
   * @return
   */
  def log2(int: Int): Boolean = ((int - 1) & int) == 0

  /**
   * 给定一个整数，请写一个函数判断该整数的二进制表示中1的个数（✭✭✩✩✩）
   * 此题较之上一题又再进一步，判断一个整数二进制表示中1的个数，假设这个整数用32位表示，
   * 可正可负可0，那么这个数中有多少个1，就需要考虑到符号位的问题了。
   *
   * */
  def count2(int: Int): Int = {
    var r: Int = 0
    var i: Int = int
    while (i != 0) {
      r += int & 1
      i >>>= 1
    }
    r
  }

  /**
   *   给定一个整数，请写一个函数判断该整数的二进制表示中1的个数（✭✭✩✩✩）
   * 每次与比自己小1的数与那么该数的二进制表示最后一个为1位上的1将将会被抹去。
   * 其实这是一个知道有这种原理才能想到的方法，所以大家也不用哀叹说我怎么想不到，通过这次记住有这个规律下次就多一个思路也不是很么坏事。
   *
   */
  def count1(int: Int): Int = (if (int == 0) 0 else 1 + count1(((int - 1) & int)))


  /**
   * 在其他数都出现两次的数组中找到只出现一次的那个数（✭✭✩✩✩）
   *
   * 首先我们应该知道二进制异或操作，异或结果是二进制中两个位相同为0，相异为1。因此可以有个规律：
   * 任何整数 n 与 0 异或总等于其本身 n，一个数与其本身异或那么结果肯定是 0。
   *
   * 还需要知道一个规律：
   * 多个数异或操作，遵循交换律和结合律。
   *
   *
   * eO ^ (A ^ A ^ B ^ B ^ C ^ C ) ^ D = eO ^ 0 ^ D = D
   *
   */
  def oddTimesNum(xa:Array[Int]):Int= {
    var eO = 0
    xa.foreach(x => {
      eO = eO ^ x
    })
    eO
  }

  /**
   * 在其他数都出现两次的数组中找到只出现一次的那两个数（✭✭✭✩✩）
   * 我们顺着上题的思路来思考，如果有两个数获得的结果 eO 肯定是 eO = a^b,
   * 此题的关键就在于如何分别得到 a，b 这两个数。我们应该想到，任何不相同的两个除了跟自己异或外，
   * 不可能每一个位都相同，也就是说不相同的两个数 a b 异或得到结果二进制表示上肯定有一位为 1。 这是关键。
   *
   * 我们可以假设第 k 位不为 0 ，那么就说明 a 与 b 在这位上数值不相同。
   * 我们要做只是设置一个数第 k 位 为 1，其余位为 0 记为 rightOne。
   *
   * 这时需要拿 eOhasOne = 0 再异或遍历一次数组，但是需要忽略与 rightOne 相与等于 0 的数。
   * 因为相与等于 0 则代表了这个数肯定是两个数中第 k 位不为 1的那个。
   * 最终得到的 eOhasOne 就是 a b 中第 k 为为 1 的那个。
   * 那么接下来就剩下一个问题要解决了，如何找到 rightOne ，
   * 这里采用与本身补码相与的方法得到即 int rightOne = eO & (~eO + 1)
   *
   * @param xa
   */
  def printOddTimesNum(xa:Array[Int])={
    var eO,eOhasOne=0

    xa.foreach(x=>{eO = eO ^ x})

    val right =eO & (~eO +1)
    xa.foreach(x=>{if((right& x)!=0) eOhasOne =eOhasOne ^x})

    println(s"eOhasOne =$eOhasOne ,(eOhasOne ^ eO): ${ (eOhasOne ^ eO)} ")
  }

  /**
   * 交换
   */
  def exchange()= {
    var x: Int = 9
    var y: Int = 11

    println(s"pre->x:$x(${ Integer.toBinaryString(x)}),y:$y(${ Integer.toBinaryString(y)})")
    x = x ^ y

    println(s"1-> x:$x(${ Integer.toBinaryString(x)}),y:$y(${ Integer.toBinaryString(y)})")
    y = x ^ y
    println(s"2-> x:$x(${ Integer.toBinaryString(x)}),y:$y(${ Integer.toBinaryString(y)})")
    x = x ^ y

    println(s"end-> x:$x(${ Integer.toBinaryString(x)}),y:$y(${ Integer.toBinaryString(y)})")
  }
}
