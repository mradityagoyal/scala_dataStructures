import scala.collection._
import scala.io.StdIn

object NetworkPacketSimulator extends App {

  override def main(args: Array[String]) = {

    val arr = StdIn.readLine().split(" ").map(_.toInt)
    val maxBufferSize = arr(0)
    val n = arr(1)

    val buffer: NetworkPacketBuffer = new NetworkPacketBuffer(maxBufferSize)

    for (i <- 0 until n) {
      val packetInfo = StdIn.readLine().split(" ").map(_.toInt)
      val p = Packet(packetInfo(0), packetInfo(1))
      println(buffer.enqueuePacket(p))
    }

  }

}

class NetworkPacketBuffer(val maxBufferSize: Int, val q: mutable.Queue[(Int, Packet)] = mutable.Queue.empty) {

  //enqueues a packet to the buffer and returns the time at which the packet will get processed. 
  def enqueuePacket(p: Packet): Int = {
    if (q.isEmpty) {
      //queue is empty.. enqueue this packet. and return processing time = arrival time. 
      q.enqueue((p.arrivalTime, p))
      p.arrivalTime
    } else {
      val (t, f) = q.front
      //if arrival time of this packet is more than the arrival time of first packet in the buffer. 
      if (p.arrivalTime >= t + f.processingTime) {
        q.dequeue()
        enqueuePacket(p)
      } else {
        if (q.size < maxBufferSize) {
          val last = q.last
          q.enqueue((last._1 + last._2.processingTime, p))
          last._1 + last._2.processingTime
        } else {
          -1
        }
      }
    }
  }
}

case class Packet(val arrivalTime: Int, val processingTime: Int)