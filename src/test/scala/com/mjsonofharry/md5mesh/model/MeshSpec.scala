package com.mjsonofharry.md5mesh.model

import org.scalatest._
import atto._, Atto._
import cats.implicits._
import atto.ParseResult.Done

class MeshSpec extends FlatSpec with Matchers with GivenWhenThen with Inside {
  val MESH = """mesh 3 {
	shader "P:/Doom/base/models/monsters/zombie/commando/com1_eye.tga"

	numverts 8
	vert 0 0.004951 0.004951 0 1
	vert 1 0.004951 0.995050 1 1
	vert 2 0.995049 0.004951 2 1
	vert 3 0.995049 0.995050 3 1
	vert 4 0.004951 0.004951 4 1
	vert 5 0.004951 0.995050 5 1
	vert 6 0.995049 0.004951 6 1
	vert 7 0.995049 0.995050 7 1

	numtris 4
	tri 0 2 1 0
	tri 1 3 1 2
	tri 2 6 5 4
	tri 3 7 5 6

	numweights 8
	weight 0 50 1.000000 -1.377079 5.809550 0.678925
	weight 1 50 1.000000 -1.409910 5.956552 -0.422397
	weight 2 50 1.000000 -2.383721 5.339245 0.646155
	weight 3 50 1.000000 -2.416551 5.486244 -0.455167
	weight 4 50 1.000000 2.399255 5.226680 0.512144
	weight 5 50 1.000000 2.402483 5.421796 -0.582162
	weight 6 50 1.000000 1.432182 5.766707 0.605577
	weight 7 50 1.000000 1.435409 5.961823 -0.488721
}"""

  "the minimized shader path" should "be extracted" in {
    Given("a full shader path")
    val shader = "P:/Doom/base/models/monsters/zombie/commando/com1_d.tga"

    When("the shader path is parsed")
    val result = Mesh.shaderParser.parseOnly(shader)

    Then("the correct part of the shader path should be returned")
    result should matchPattern { case Done(input, result) => }

    inside(result) {
      case Done(input, result) =>
        result shouldBe "models/monsters/zombie/commando/com1_d"
    }
  }

  "the mesh parser" should "successfully parse a mesh" in {
    Given("a mesh")
    val mesh = MESH

    When("the mesh is parsed")
    val result = Mesh.parser.parseOnly(mesh)

    Then("a mesh object should be constructed")
    result should matchPattern { case Done(_: String, _: Mesh) => }

    inside(result) {
      case Done(_: String, mesh: Mesh) =>
        inside(mesh) {
          case Mesh(
              index: Int,
              shader: String,
              numverts: Int,
              numtris: Int,
              numweights: Int,
              verts: List[Vert],
              tris: List[Tri],
              weights: List[Weight]
              ) => {
            index shouldBe 3
            shader shouldBe "P:/Doom/base/models/monsters/zombie/commando/com1_eye.tga"
            numverts shouldBe 8
            numtris shouldBe 4
            numweights shouldBe 8
            verts.size shouldBe numverts
            tris.size shouldBe numtris
            weights.size shouldBe numweights
          }
        }
    }
  }
}
