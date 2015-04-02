/// <reference path="../astar-example/astar.ts" />

var schweden = new Graph();

var kiruna = new GraphNode(0, 5, 2, "Kiruna");
var lulea = new GraphNode(1, 10, 7, "Luleå");
var umea = new GraphNode(2, 10, 17, "Umeå");
var tanndalen = new GraphNode(3, 1, 23, "Tänndalen");
var sthlm = new GraphNode(4, 15, 28, "Stockholm");
var gbg = new GraphNode(5, 1, 33, "Gôteborg");
var kalmar = new GraphNode(6, 12, 34, "Kalmar");
var malmo = new GraphNode(7, 4, 40, "Malmö");

schweden.addNode(kiruna);
schweden.addNode(lulea);
schweden.addNode(umea);
schweden.addNode(tanndalen);
schweden.addNode(sthlm);
schweden.addNode(gbg);
schweden.addNode(kalmar);
schweden.addNode(malmo);

schweden.addEdge(new Edge(10, kiruna, lulea));
schweden.addEdge(new Edge(25, tanndalen, kiruna));
schweden.addEdge(new Edge(10, kiruna, lulea));
schweden.addEdge(new Edge(12, lulea, umea));
schweden.addEdge(new Edge(14, umea, tanndalen));
schweden.addEdge(new Edge(16, umea, sthlm));
schweden.addEdge(new Edge(19, tanndalen, sthlm));
schweden.addEdge(new Edge(10, tanndalen, gbg));
schweden.addEdge(new Edge(19, gbg, sthlm));
schweden.addEdge(new Edge(9, sthlm, kalmar));
schweden.addEdge(new Edge(12, gbg, kalmar));
schweden.addEdge(new Edge(10, malmo, gbg));
schweden.addEdge(new Edge(14, kalmar, malmo));

//aStar.aStar(schweden, malmo, kiruna);
var finalNode = aStar.aStar(schweden, malmo, kiruna);
var path = finalNode.getPath();
var node = path.firstNode;
while(node != null) {
	console.log(node.element.getFromNode().getName() + " " + node.element.getEndNode().getName()) 
	node = node.next
}