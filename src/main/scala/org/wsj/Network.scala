package org.wsj

/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 1/22/13
 * Time: 3:06 PM
 * To change this template use File | Settings | File Templates.
 */

class Link // simplest
case class Junction[T<:Link](inLinks: Iterable[T], outLinks: Iterable[T]) // simplest

// need to specify the junction type and how to get the junctions
trait Network[T<:Link] {
  type NetworkJunction <: Junction[T]
  def junctions: Iterable[NetworkJunction]
  val links = junctions.flatMap(j => j.inLinks ++ j.outLinks).toSet
}