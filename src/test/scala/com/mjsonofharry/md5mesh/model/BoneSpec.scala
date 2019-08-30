package com.mjsonofharry.md5mesh.model

import org.scalatest._
import atto._, Atto._
import cats.implicits._
import atto.ParseResult.Done

class BoneSpec extends FlatSpec with Matchers with GivenWhenThen with Inside {
  val ORIGIN =
    """bone 0 {
	name "origin"
	bindpos 0.000000 0.000000 0.000000
	bindmat 0.000000 1.000000 0.000000 -1.000000 0.000000 0.000000 0.000000 0.000000 1.000000
}"""

  val BODY =
    """bone 1 {
	name "Body2"
	bindpos 0.217826 0.000000 51.499458
	bindmat 0.000000 1.000000 0.000000 0.000000 0.000000 1.000000 1.000000 0.000000 0.000000
	parent "origin"
}"""

  "the bone parser" should "successfully parse a bone that has no parent" in {
    Given("a bone without a parent")
    val bone = ORIGIN

    When("the bone is parsed")
    val result = Bone.parser.parseOnly(bone)

    Then("a bone object should be constructed")
    result should matchPattern { case Done(_: String, _: Bone) => }

    inside(result) {
      case Done(_: String, bone: Bone) =>
        inside(bone) {
          case Bone(index, name, bindpos, bindmat, parent) => {
            index shouldBe 0
            name shouldBe "origin"
            val p = Vector3(0, 0, 0)
            bindpos shouldBe p
            val m = (Vector3(0, 1, 0), Vector3(-1, 0, 0), Vector3(0, 0, 1))
            bindmat shouldBe m
            parent shouldBe None
          }
        }
    }
  }

  it should "successfully parse a bone that has a parent" in {
    Given("a bone with a parent")
    val bone = BODY

    When("the bone is parsed")
    val result = Bone.parser.parseOnly(bone)

    Then("a bone object should be constructed")
    result should matchPattern { case Done(input: String, bone: Bone) => }

    inside(result) {
      case Done(_: String, bone: Bone) =>
        inside(bone) {
          case Bone(index, name, bindpos, bindmat, parent) => {
            index shouldBe 1
            name shouldBe "Body2"
            val p = Vector3(0.217826, 0, 51.499458)
            bindpos shouldBe p
            val m = (Vector3(0, 1, 0), Vector3(0, 0, 1), Vector3(1, 0, 0))
            bindmat shouldBe m
            parent should contain("origin")
          }
        }
    }
  }
}
