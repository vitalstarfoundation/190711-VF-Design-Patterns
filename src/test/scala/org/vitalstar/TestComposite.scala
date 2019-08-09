package org.vitalstar

import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.scalatest.junit.JUnitRunner
import org.junit.Assert._
import org.junit.runner.RunWith

trait Valuable {
  def cost: Double
}

class Food(_price: Double) extends Valuable {
  def price: Double = _price
  def discounted80: Double = discountPrice(0.8)
  def discountPrice(_discount: Double) = price * _discount
  def cost = 1
}

class Meal(a: Food, b: Food, c: Food)  extends Valuable {
  def price: Double = a.price + b.price + c.price
  def discountPrice(_discount: Double) = a.discountPrice(_discount) + b.discountPrice(_discount) + c.discountPrice(_discount)
//  def discountPrice(_discount: Double) = price * _discount
  def cost = a.cost + b.cost + c.cost
}


@RunWith(classOf[JUnitRunner])
class TestComposite extends FunSuite with BeforeAndAfterAll {

  test("Encapsulation") {
    val fry = new Food(1.95)
    val burger = new Food(3.40)
    val drink = new Food(1.5)
    val breakfast = new Meal(fry, burger, drink)

    assertEquals(1.95, fry.price, 10e-5)
    assertEquals(1.56, fry.discounted80, 10e-5)
    assertEquals(6.85, breakfast.price, 10e-5)

    assertEquals(1, fry.cost, 10e-5)
    assertEquals(3, breakfast.cost, 10e-5)
  }

}
