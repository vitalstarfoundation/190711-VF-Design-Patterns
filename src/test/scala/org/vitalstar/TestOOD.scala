package org.vitalstar

class TestOOD {
  
}

/*

| Encapsulation

class Food(_price: Double) {
  def price: Double = _price
  def discounted80: Double = discountPrice(0.8)
  def discountPrice(_discount: Double) = price * _discount
}

val fry = new Food(1.95)
val burger = new Food(3.40)
val drink = new Food(1.5)

fry.price

| Abstraction

class Food(_price: Double) extends Valuable {
  def price: Double = _price
  def discounted80: Double = discountPrice(0.8)
  def discountPrice(_discount: Double) = price * _discount
  def cost = 1
}

class Meal(a: Food, b: Food, c: Food)  extends Valuable {
  def price: Double = a.price + b.price + c.price
  def discountPrice(_discount: Double) = price * _discount
  def cost = 2
}

| Inheritence

class Toy extends Valuable {
  def cost = 0
}

class Receipt(_a: Food, _b: Food, _c: Food, _d:Toy) extends Meal(_a,_b,_c) with Valuable {
  def afterTax(tax: Double = 0.09): Double = price * (1+tax)
  def couponCode: String = ???
  def cost =  4
}

class HappyMeal(a: Food, b: Food, c: Food) extends Meal {
  def cost = 5
}


class Order(a: Food, b: Food, c: Food) extends Meal {
  def afterTax(tax: Double = 0.09): Double = price * (1+tax)
}

| Polymorphism

trait Valuable {
  def cost: Double 
}


*/