object ex1 extends App {
    import dynamictypes._
    
    val searchLinks = List(
        "Google" -> "http://www.google.com",
        "Bing" -> "http://www.bing.com",
        "Ask.com" -> "http://www.ask.com",
        "Yahoo!" -> "http://www.yahoo.com",
        "Alta Vista" -> "http://www.altavista.com",
        "Excite" -> "http://www.excite.com",
        "HotBot" -> "http://www.hotbot.com")

    val pageStyle = """
#searchList {
    padding: 15px 25px;
    border: solid 2px black;   
    width: 300px;
}

#searchList li {
    margin: 3px;
    list-style: none;
}

p.title {
    margin: 0px;
    padding: 30px;
    font-size: 3em;
}
"""

    val xml = new rubyXmlBuilder
    xml.setDoctype("PUBLIC", "\"-//W3C//DTD HTML 4.01//EN\"", "\"http://www.w3.org/TR/html4/strict.dtd\"")
    xml html {
        xml head {
            xml title "Search Links - Build with Ruby Like Xml Builder in Scala"
            xml style pageStyle
        }
        xml body {
            xml p ("Search Links:", "class" -> "title")
            xml ul ({
                for ((name, link) <- searchLinks)
                    xml li {
                        xml a (name, "href" -> link)
                    }
            }, "id" -> "searchList")
        }
    }

    println(xml.getXml)
}