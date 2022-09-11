import scala.xml.XML
val rootPath = """C:\WorkData\Scala_Projects\Pet\Horizon\src\main\scala\xml"""
val root = XML.load(rootPath+"""\root.graphml""")
println(root)

XML.save( rootPath+"""\root1.graphml""",root)
