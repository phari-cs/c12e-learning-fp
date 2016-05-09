package com.c12e.learn
package test


import org.scalacheck.Properties


abstract class Spec(name: String) extends Properties(name) with ArbitraryInstances {

  def checkAll(props: Properties) =
    for ((name, prop) <- props.properties) yield {
      property(name) = prop
    }

}
