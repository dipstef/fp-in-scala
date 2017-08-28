package c11_monads

import c08_testing.exaustive.Gen

case class Order(item: Item, quantity: Int)

case class Item(name: String, price: Double)

object Examples extends App {

  val genOrder: Gen[Order] = for {
    name <- Gen.stringN(3)
    price <- Gen.uniform.map(_ * 10)
    quantity <- Gen.choose(1, 100)
  } yield Order(Item(name, price), quantity)


  val genItem: Gen[Item] = for {
    name <- Gen.stringN(3)
    price <- Gen.uniform.map(_ * 10)
  } yield Item(name, price)


  val genOrder2: Gen[Order] = for {
    item <- genItem
    quantity <- Gen.choose(1, 100)
  } yield Order(item, quantity)


  /*
    Let’s expand both implementations of genOrder into calls to map and flatMap to better see what’s going on. In the
    former case, the translation is straightforward:

    Gen.nextString.flatMap(name =>
      Gen.nextDouble.flatMap(price =>
        Gen.nextInt.map(quantity => Order(Item(name, price), quantity))))

     But the second case looks like this (inlining the call to genItem):

    Gen.nextString.flatMap(name =>
      Gen.nextInt.map(price =>
        Item(name, price))).flatMap(item =>
          Gen.nextInt.map(quantity => Order(item, quantity)


    Once we expand them, it’s clear that those two implementations aren’t identical. And yet when we look at the
    for-comprehension, it seems perfectly reasonable to assume that the two implementations do exactly the same thing.

    In fact, it would be surprising and weird if they didn’t.

    It’s because we’re assuming that flatMap obeys an associative law:

      x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))

    And this law should hold for all values x, f, and g of the appropriate types—not just for Gen but for Parser,
    Option, and any other monad.

   */


  /*
     Let’s prove that this law holds for Option. All we have to do is substitute None or Some(v) for x in the preceding
     equation and expand both sides of it.

     We start with the case where x is None, and then both sides of the equal sign are

     None:

     None.flatMap(f).flatMap(g) == None.flatMap(a => f(a).flatMap(g))


     Since None.flatMap(f) is None for all f, this simplifies to

     None == None


      Thus, the law holds if x is None. What about if x is Some(v) for an arbitrary choice of v? In that case, we have

      x.flatMap(f).flatMap(g)         ==          x.flatMap(a => f(a).flatMap(g))

      Some(v).flatMap(f).flatMap(g)   ==          Some(v).flatMap(a => f(a).flatMap(g))

                                                  Apply definition of Some(v).flatMap(...)
      f(v).flatMap(g)                 ==          (a => f(a).flatMap(g))(v)

                                                  Simplify function application (a => ..)(v)
      f(v).flatMap(g)                 ==          f(v).flatMap(g)

   */
}

