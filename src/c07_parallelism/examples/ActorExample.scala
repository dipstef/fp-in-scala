package c07_parallelism.examples

import java.util.concurrent.Executors

import c07_parallelism.Actor

object ActorExample {
  def main(args: Array[String]) {
    val S = Executors.newFixedThreadPool(4)

    val echoer = Actor[String](S){
      msg => println(msg)
    }

    echoer ! "hello"
    echoer ! "goodbye"
    echoer ! "You are just repeating everything I say to you?"
  }
}