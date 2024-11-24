#! /bin/bash

scala test.scala
scala --native-mode release-full test.scala
