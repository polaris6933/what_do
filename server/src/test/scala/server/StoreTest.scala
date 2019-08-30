package store

import akka.actor.{ ActorSystem, Props }
import akka.testkit.{ ImplicitSender, TestActors, TestKit }
import akka.testkit.TestActorRef
import akka.util.ByteString
import org.scalatest.{ BeforeAndAfterAll, Matchers, WordSpecLike }
import org.scalatest.{FlatSpec, Matchers}

import Store._
import user.User

class StoreTest() extends TestKit(ActorSystem("MySpec"))
  with ImplicitSender
  with WordSpecLike
  with Matchers
  with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "a store actor" must {
    val store = system.actorOf(Props[Store])

    "error with < 4 arguments for register" in {
      store ! ClientMessage("register - some name - 2")
      expectMsg(Response(ByteString("invalid number of arguments")))
    }

    "error with < 2 arguments for show" in {
      store ! ClientMessage("show")
      expectMsg(Response(ByteString("invalid number of arguments")))
    }

    "error with < 2 arguments for strat" in {
      store ! ClientMessage("strat")
      expectMsg(Response(ByteString("invalid number of arguments")))
    }

    "error with < 4 arguments for custom_strat" in {
      store ! ClientMessage("custom_strat - time - priority")
      expectMsg(Response(ByteString("invalid number of arguments")))
    }

    "error with < 2 arguments for pick" in {
      store ! ClientMessage("pick")
      expectMsg(Response(ByteString("invalid number of arguments")))
    }

    "error with < 3 arguments for prioritize" in {
      store ! ClientMessage("prioritize - name")
      expectMsg(Response(ByteString("invalid number of arguments")))
    }

    "recognize unsupported methods" in {
      store ! ClientMessage("picj")
      expectMsg(Response(ByteString("unsupported method")))
    }

    "error with invalid integers for register" in {
      store ! ClientMessage("register - name - three - 4")
      expectMsg(Response(ByteString("invalid argument for time / priority")))
    }

    "error with invalid integers for show" in {
      store ! ClientMessage("show - three")
      expectMsg(Response(ByteString("invalid argument for time")))
    }

    "error with invalid integers for prioritize" in {
      store ! ClientMessage("prioritize - name - three")
      expectMsg(Response(ByteString("invalid argument for time")))
    }

    "error with invalid options for custom_strat" in {
      store ! ClientMessage("custom_strat - time - priority - dusk")
      expectMsg(Response(ByteString("invalid strategy ordering")))
    }

    def register(): Unit = {
      store ! ClientMessage("register - name1 - 1 - 1")
      expectMsg(Response(ByteString("ok")))
      store ! ClientMessage("register - name2 - 1 - 1")
      expectMsg(Response(ByteString("ok")))
      store ! ClientMessage("register - name3 - 1 - 1")
      expectMsg(Response(ByteString("ok")))
      store ! ClientMessage("register - name4 - 1 - 1")
      expectMsg(Response(ByteString("ok")))
    }

    def clear(): Unit = {
      store ! ClientMessage("clear")
      expectMsg(Response(ByteString("ok")))
    }

    "correctly register new entries" in {
      register

      store ! ClientMessage("catalog")
      val expected = "name1 - 1 - 1 - 0\n" +
                     "name2 - 1 - 1 - 0\n" +
                     "name3 - 1 - 1 - 0\n" +
                     "name4 - 1 - 1 - 0"
      expectMsg(Response(ByteString(expected)))
      clear
    }

    "correctly remove picked elements and add dust after pick" in {
      register
      store ! ClientMessage("pick - name2")
      expectMsg(Response(ByteString("ok")))

      store ! ClientMessage("catalog")
      val expected = "name1 - 1 - 1 - 1\n" +
                     "name3 - 1 - 1 - 1\n" +
                     "name4 - 1 - 1 - 1"
      expectMsg(Response(ByteString(expected)))
      clear
    }

    "correctly reprioritize" in {
      register
      store ! ClientMessage("prioritize - name4 - 0")
      expectMsg(Response(ByteString("ok")))

      store ! ClientMessage("catalog")
      val expected = "name1 - 1 - 1 - 0\n" +
                     "name2 - 1 - 1 - 0\n" +
                     "name3 - 1 - 1 - 0\n" +
                     "name4 - 0 - 1 - 0"
      expectMsg(Response(ByteString(expected)))
      clear
    }

    "reject duplicate names in register" in {
      register
      store ! ClientMessage("register - name1 - 1 -1")
      expectMsg(Response(ByteString("name1 is already registered")))
      clear
    }

    "reject non-existent names in pick" in {
      register
      store ! ClientMessage("pick - name0")
      expectMsg(Response(ByteString("name0 is not registered")))
      clear
    }

    "reject non-existent names in prioritize" in {
      register
      store ! ClientMessage("prioritize - name0 - 0")
      expectMsg(Response(ByteString("name0 is not registered")))
      clear
    }
  }
}
