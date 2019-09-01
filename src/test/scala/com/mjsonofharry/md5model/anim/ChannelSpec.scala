package com.mjsonofharry.md5model.anim

import org.scalatest._
import atto._, Atto._
import cats.implicits._
import atto.ParseResult.Done

class ChannelSpec
    extends FlatSpec
    with Matchers
    with GivenWhenThen
    with Inside {
  val CHANNEL =
    """channel 0 {
	joint "boundsMin"
	attribute "x"
	starttime 0.041667
	endtime 2.916667
	framerate 24.000000

	strings 0

	range 0 70
	keys 70
		 -2.9297 -2.9375 -2.9453 -2.9609 -2.9844 -3.0156 -3.0547 -3.0938
		 -3.1328 -3.1797 -3.2344 -3.2813 -3.3359 -3.3984 -3.4531 -3.5156
		 -3.5703 -3.6328 -3.6953 -3.7500 -3.8047 -3.8594 -3.9141 -3.9609
		 -4.0078 -4.0469 -4.0859 -4.1172 -4.1484 -4.1797 -4.2031 -4.2188
		 -4.2344 -4.2500 -4.2578 -4.2578 -4.2578 -4.2578 -4.2422 -4.2266
		 -4.2109 -4.1875 -4.1563 -4.1250 -4.0859 -4.0469 -4.0000 -3.9531
		 -3.8984 -3.8438 -3.7813 -3.7188 -3.6641 -3.6016 -3.5391 -3.4766
		 -3.4141 -3.3594 -3.2969 -3.2422 -3.1875 -3.1406 -3.0938 -3.0547
		 -3.0234 -2.9922 -2.9688 -2.9453 -2.9375 -2.9297
}"""

  "the channel parser" should "successfully parse a channel" in {
    Given("a channel")
    val channel = CHANNEL

    When("the channel is parsed")
    val result = Channel.parser.parseOnly(channel)

    Then("a channel object should be constructed")
    result should matchPattern { case Done(_: String, _: Channel) => }

    val expectedKeys = List(-2.9297, -2.9375, -2.9453, -2.9609, -2.9844,
      -3.0156, -3.0547, -3.0938, -3.1328, -3.1797, -3.2344, -3.2813, -3.3359,
      -3.3984, -3.4531, -3.5156, -3.5703, -3.6328, -3.6953, -3.7500, -3.8047,
      -3.8594, -3.9141, -3.9609, -4.0078, -4.0469, -4.0859, -4.1172, -4.1484,
      -4.1797, -4.2031, -4.2188, -4.2344, -4.2500, -4.2578, -4.2578, -4.2578,
      -4.2578, -4.2422, -4.2266, -4.2109, -4.1875, -4.1563, -4.1250, -4.0859,
      -4.0469, -4.0000, -3.9531, -3.8984, -3.8438, -3.7813, -3.7188, -3.6641,
      -3.6016, -3.5391, -3.4766, -3.4141, -3.3594, -3.2969, -3.2422, -3.1875,
      -3.1406, -3.0938, -3.0547, -3.0234, -2.9922, -2.9688, -2.9453, -2.9375,
      -2.9297)
    inside(result) {
      case Done(_: String, channel: Channel) =>
        inside(channel) {
          case Channel(
              index,
              joint,
              attribute,
              starttime,
              endtime,
              framerate,
              strings,
              range,
              keys
              ) => {
            index shouldBe 0
            joint shouldBe "boundsMin"
            attribute shouldBe "x"
            starttime shouldBe 0.041667
            endtime shouldBe 2.916667
            framerate shouldBe 24.0
            strings shouldBe 0
            range shouldBe (0, 70)
            keys should contain theSameElementsInOrderAs expectedKeys
          }
        }
    }
  }
}
