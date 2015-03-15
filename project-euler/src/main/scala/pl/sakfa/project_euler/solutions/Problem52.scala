package pl.sakfa.project_euler.solutions

object Problem52 extends App {

    def isValid(n: Int) = {
        val template = n.toString.sortBy(_.toInt);
        (2 to 6)
            .map(m => (m*n).toString.sortBy(_.toInt))
            .forall(s => s == template)

    }

    println {
        Stream.from(1)
            .filter(n => isValid(n))
            .head
    }

}