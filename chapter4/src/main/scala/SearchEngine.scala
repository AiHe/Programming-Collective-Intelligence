import java.net.URL

import scala.io.Source
import scala.util.{Failure, Success, Try}
import scala.xml.NodeSeq
import scala.xml.parsing.XhtmlParser

/**
 * Created by aihe on 11/23/15.
 */
object SearchEngine {

  /**
   * Initialize the crawler with the name of database
   * @param dbName
   */
  class Crawler(dbName: String) {

    //    def __del__(self): pass

    def dbCommit() = ???

    /**
     * Auxiliary function for getting an entry id and adding it if it's not present
     * @param table
     * @param field
     * @param value
     * @param createNew
     * @tparam T
     */
    def getEntryId[T](table: String, field: String, value: T, createNew: Boolean = true) = ???

    /**
     * Index an individual page
     * @param url
     * @param soup
     * @return
     */
    def addToIndex(url: String, soup: NodeSeq) = {
      println(s"Indexing $url")
    }

    /**
     * Extract the text from an HTML page (no tags)
     * @param soup
     */
    def getTextOnly(soup: NodeSeq): String = ""


    /**
     * Separate the words by any non-whitespace character
     * @param text
     */
    def separateWords(text: String) = ???

    /**
     * Return true if this url is already indexed
     * @param url
     */
    def isIndexed(url: String): Boolean = false


    /**
     * Add a link between two pages
     * @param urlFrom
     * @param urlTo
     * @param linkText
     * @return
     */
    def addLinkRef(urlFrom: String, urlTo: String, linkText: String) = ???

    /**
     * Starting with a list of pages, do a breadth first search to the given depth, indexing pages as we go
     * @param pages
     * @param depth
     * @return
     */
    def crawl(pages: Set[String], depth: Int = 2): Unit = {
      if (depth > 0) {
        var newPages = Set.empty[String]
        pages.foreach(page => {
          val contents: Seq[Option[String]] = Try(Source.fromURL(page)) match {
            case Success(s) =>
              val node = XhtmlParser(s)
              addToIndex(page, node)
              (node \\ "a").map { case link => {
                val mergedUrl = new URL(new URL(page), page + (link \ "@href").text).toString
                println(mergedUrl)
                if (mergedUrl.indexOf("'") == -1) {
                  val url = mergedUrl.split('#').head
                  addLinkRef(page, url, getTextOnly(link))
                  if (url.take(4).toLowerCase == "http" && !isIndexed(url)) {
                    Some(url)
                  } else None
                } else None
              }
              }
            case Failure(ex) => {
              println(s"Can not open $page")
              Seq(None)
            }
          }
          for {
            content <- contents
            c <- content
          } yield {
            newPages = newPages + c
          }
          dbCommit()
        })
        crawl(newPages, depth - 1)
      }
    }
  }

  /**
   * Create the database tables
   */
  def createIndexTables() = ???

}