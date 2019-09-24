package org.vitalstar

import org.scalatest.{FunSuite, BeforeAndAfterAll}
import org.scalatest.junit.JUnitRunner
import org.junit.Assert._
import org.junit.runner.RunWith

import collection.mutable.ListBuffer
// We define a concept, which is behavior
trait Item {
  def name: String = ""
  def ingredient: String = ""
  def sold: Boolean = false
  def consumed: Boolean = sold
  def calories: Double = 0.0
  def addItem(item: Item) = {}
  def getItems: List[Item] = List()
  def price: Double
  def isSingle: Boolean = true
  def doYouLikeMe(customer: Customer): Boolean = false
}

// This is the implementation
class ItemBase(_name: String ="", _ingredient: String = "", val _price: Double = 0.0)
    extends Item {

  override val name: String = _name
  val bag: ListBuffer[Item] = new ListBuffer[Item]()

  override def addItem(item: Item): Unit = bag.append(item)
  override def getItems: List[Item] = bag.toList
  override def price: Double = {
    if (isSingle)
        _price
    else
      bag.foldLeft(0.0){(price: Double, i: Item) => i.price + price}
  }
  override def ingredient: String = {
    if (isSingle)
        _ingredient.trim()
    else
      bag.foldLeft(""){ (e1,e2) => e1 + " " + e2.ingredient}.trim()
  }

  override def doYouLikeMe(customer: Customer): Boolean = {
    customer.doYouLike(this)
//    customer.doYouLike(_ingredient)
  }

  override def toString: String = name
}

class Food(name:String, _ingredient: String = "", _price: Double) extends ItemBase(name, _ingredient, _price) {
  override def addItem(item: Item): Unit = {}
  override def getItems: List[Item] = List(this)
}

class Meal(name: String)  extends ItemBase(name) {
  override def isSingle = false
  override def toString = name + " " + getItems.toString
}

class Toy(name: String = "", _price: Double = 0.0)
  extends ItemBase(name, "plastics", _price) {
}

trait Store {
  def order(name: String): Item = ???
}

class StoreBase(address: String) {
  val shelf: ListBuffer[Item] = new ListBuffer[Item]()
  def replenish(items: List[Item]): Unit = shelf.appendAll(items)
  def numberOfItems = shelf.length
  def order(name: String): Item = {
    val items = shelf.filter{e => e.name == name}
    if (!items.isEmpty) {
      val item = items.head
      shelf -= item
      item
    } else null
  }

  override def toString = shelf.length + " items in" + address
}

class World {
  private var aug: StoreBase = null

  def getStore: StoreBase = {
    if (aug == null) {
      aug = new StoreBase("1500 Augustine drive")
    }
    aug
  }

  def reset = {
    aug = null
  }

  def born(character:String = "", allergy: String= "xxx"): Customer = {
    null
  }
}

trait Customer {
  def doYouLike(item: Item): Boolean = false
}

class CustomerBase(allergy: String = "") extends Customer {
  protected def isAllergic(item: Item): Boolean = {
    item.ingredient.contains(allergy)
  }

  protected def canYouAfford(item: Item): Boolean = {
    item.price < 5
  }

}

class Patient(allergy: String = "xxx") extends CustomerBase(allergy) {
  override def doYouLike(item: Item): Boolean = {
    !isAllergic(item)
  }
}

class Kid(allergy: String = "xxx") extends CustomerBase(allergy) {
  override def doYouLike(item: Item): Boolean = {
    canYouAfford(item)
  }
}

class KidPatient(allergy: String = "xxx") extends CustomerBase(allergy) {
  override def doYouLike(item: Item): Boolean = {
    canYouAfford(item) && !isAllergic(item)
  }
}

class KidPatient1(allergy: String = "xxx") extends CustomerBase(allergy) {
  val a: Kid = new Kid(allergy)
  val b: Patient = new Patient(allergy)
  override def doYouLike(item: Item): Boolean = {
    a.doYouLike(item) && b.doYouLike(item)
  }
}

trait FoodDelivery extends Store {
}

class FoodDeliveryBase(store: Store) extends FoodDelivery {
  override def order(name: String): Item = {
    store.order(name)
  }
}

class UberEat(store: Store) extends FoodDeliveryBase(store) {

}

class DoorDash(store: Store) extends FoodDeliveryBase(store) {

}

@RunWith(classOf[JUnitRunner])
class TestComposite extends FunSuite with BeforeAndAfterAll {
  val earth = new World()

  // Create some items
  val fry = new Food("fry","potatos",1.95)
  val burger = new Food("double double","beef gluten bread condiment pickle tomato onion",3.40)
  val drink = new Food("soda", "sugar water co2",1.5)
  val ironman = new Toy("ironman",0)
  val breakfast = new Meal("snack")
  breakfast.addItem(fry)
  breakfast.addItem(burger)
  breakfast.addItem(drink)

  val john = new Patient("pickle")
  val matthew = new Kid("")
  val jonathan = new KidPatient("gluten")
  val amour = new Patient("onion")

  val matthew1: Customer = earth.born("kid", "xxx")
  val john1: Customer = earth.born("patient", "pickle")
  val jonathan1 = earth.born("kid patient boss wife husband", "gluten")
  val amour1 = earth.born("patient", "onion")

  test("Encapsulation") {
    val ironman = new Toy
    breakfast.addItem(ironman)

    assertEquals(1.95, fry.price, 10e-5)
    assertEquals(6.85, breakfast.price, 10e-5)
  }

  test("create store") {
    // Create a store
    val store = new StoreBase("1500 Augustine drive")
    store.replenish(List.fill(10)(fry))
    store.replenish(List.fill(10)(burger))
    store.replenish(List.fill(10)(drink))
    store.replenish(List.fill(10)(ironman))
    store.replenish(List.fill(10)(breakfast))
    assertEquals(50, store.numberOfItems)

    val food = store.order("fry")
    assertEquals(49, store.numberOfItems)

    val meal = store.order("snack")
    assertEquals(48, store.numberOfItems)
  }

  test("singleton") {
    // demostrate the singleton
    var aug = earth.getStore
    assertEquals(0, aug.numberOfItems)

    aug = earth.getStore
    aug.replenish(List.fill(10)(fry))
    assertEquals(10, aug.numberOfItems)

    aug = earth.getStore
    aug.replenish(List.fill(10)(burger))
    assertEquals(20, aug.numberOfItems)

    // demonstrate the reset
    earth.reset
    aug = earth.getStore
    assertEquals(0, aug.numberOfItems)

    aug = earth.getStore
    aug.replenish(List.fill(10)(drink))
    assertEquals(10, aug.numberOfItems)

    aug = earth.getStore
    aug.replenish(List.fill(10)(breakfast))
    assertEquals(20, aug.numberOfItems)

    // demonstrate the order
    earth.getStore.order("soda")
    assertEquals(19, aug.numberOfItems)
  }

  test("ingredient") {
    val breakfast = new Meal("snack")
      breakfast.addItem(fry)
      breakfast.addItem(burger)
      breakfast.addItem(drink)

    assertEquals("potatos beef bread condiment pickle tomato onion sugar water co2", breakfast.ingredient)
  }

  test("builder pattern") {
/*
  class Potatos extends Ingredient
  val potatos = new Potatos

   val fry = new Food("fry","potatos salt oil",1.95)
   val fry = (new Food)
                  .addIngredient("salt")
                  .addIngredient("oil")
                  .setName("fry")
                  .addIngredient(potatos)
                  .addIngredient(poison)
                  .setPrice(1.95)
                  .prepare
   val fry = (new Food)
                  .setName("fry")
                  .addIngredient("potatos salt oil")
                  .setPrice(1.95)
                  .prepare
*/
  }

  test("iterator pattern") {
/*
     val fry = (new Food)
                  .addIngredient("salt")
                  .addIngredient("oil")
                  .setName("fry")
                  .addIngredient(potatos)
                  .addIngredient(poison)
                  .setPrice(1.95)
                  .prepare

    val ingd: IngredientIterator = fry.createIngredientIterator
    val i1 = ingd.next
    val i2 = ingd.next
    val i3 = ingd.next
    val i4 = ingd.next

    class IngredientIterator {
      val item: Food
      val counter: Int
      def next: IngredientIterator = ...
      def hasNext: Boolean = ...
      def reset
      def map()
    }
*/
  }

  test("poison food detector") {
/*
    class Milk extends Ingredient
    class Vinegar extends Ingredient

    val milk = Milk()
    val vinegar = Vinegar()
    val potatos = ..
    val salt = ...

   val mm: Food = (new Food)
            .addIngredient("???")
            .addIngredient("oil")
            .setName("fry")
            .addIngredient(potatos)
            .addIngredient(poison)
            .setPrice(1.95)
            .prepare

    val mm = new Food
    val detector = poisonDetector()

    val poisonous = detector.detect(mm)
    val poisonous =  mm.use(detector)

    class Food {
      def use(detector: Detector): Boolean = {
        val i = createIngredientIterator
        i.map{e => detector.detecte(e)}.hasTrue    // [false, false, false, false]
      }

    }

    class Detector {
      def detect(mm: Item): Boolean = {
        mm.use(this)
      }
    }

    class MVDetector extends Detector {
      val milk = Milk()
      val vinegar = Vinegar()
      val deathly: Set = Set(milk, vinegar)

      def detect(i: Ingredient): Boolen = {
        deadthly.remove(i)
        deathly.isEmpty
      }

      class glutenDetector extends Detector
    }
*/
  }

}
