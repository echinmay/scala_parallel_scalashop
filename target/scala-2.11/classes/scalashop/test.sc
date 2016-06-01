object testworksheet {
  val width = 4
  val tasks = 2

  def getranges(width: Int, tasks: Int): List[(Int, Int)] = {
    if (width < tasks) {
      List((0, width))
    }
    else if (width%tasks == 0) {
      // Exactly divisible by the number of tasks
      val x = (0 to width) by (width/tasks)
      (x zip x.tail).toList
    } else {
      // 0 to (tasks - 1)*(width/tasks)
      // Append with (task - 1)*(width/tasks), width
      val pertask = width/tasks
      val sp = (tasks - 1)*pertask

      val first = 0 to sp by pertask
      val firstset = (first zip first.tail).toList
      val last = List((sp, width))
      firstset ++ last
    }
  }

  val test1 = getranges(1,32 )
  val test2 = getranges(64,32)
  val test3 = getranges(32,32)
  test1.size
  test2.size
  test3.size
}