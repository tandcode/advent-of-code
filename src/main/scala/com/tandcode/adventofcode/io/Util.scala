package com.tandcode.adventofcode.io

import java.io.File
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.Using

object Util {
  def strFromResource(fileName: String): String = Using(Source.fromResource(fileName))(_.mkString).getOrElse("")
  
  def strFromFile(file: File): String = Using(Source.fromFile(file))(_.mkString).getOrElse("")
  
  def linesFromResource(fileName: String): Seq[String] = Using(Source.fromResource(fileName))(_.getLines().toSeq)
    .getOrElse(Nil)
  
  def strToLines(string: String): Seq[String] = string.split("[\r\n]+").toSeq

  def listFilesByPrefix(prefix: String): Seq[File] = {
    val rex = "(.*)/?(.*)".r
    val folderPath = prefix.split("/")
    val allInFolder = listResources(folderPath.init.mkString("/"))
    val filtered = allInFolder.filter(f => f.getName.startsWith(folderPath.last))
    filtered
  }

  def listResources(packageName: String): Seq[File] = {
    val classLoader = Thread.currentThread().getContextClassLoader
    val packagePath = packageName.replace('.', '/')
    val resources = Option(classLoader.getResources(packagePath)).map(_.asScala.toSeq).getOrElse(Seq.empty)

    resources.flatMap { url =>
      val protocol = url.getProtocol
      protocol match {
        case "file" =>
          val directory = new java.io.File(url.toURI)
          directory.listFiles().toSeq
        case _ =>
          Seq.empty
      }
    }
  }

}
