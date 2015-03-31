/// <reference path="../typescript-collections/collections.ts" />

class Graph {
	private edges = new collections.Set<Edge>();
	private nodes = new collections.Set<GraphNode>();

	constructor(argument) {
		// code...
	}

	addEdge(newEdge : Edge) {
		if (this.nodes.contains(newEdge.getFromNode()) && this.nodes.contains(newEdge.getEndNode())) {
			this.edges.add(newEdge);
		} else {
			throw "can't place an edge between nonexistent nodes :(";
			
		}
	}

	addNode(newNode : GraphNode) {
		this.nodes.add(newNode);
	}

	contains(node : GraphNode) : boolean {
		return this.nodes.contains(node); 
	}

	getNeighborsTo(node : GraphNode) : GraphNode[] {
		var neighbors = new collections.Set<GraphNode>();
		for(var e in this.edges) {
			if(e.fromNode == node) {
				neighbors.add(e.toNode);
			} else if(e.toNode == node) {
				neighbors.add(e.fromNode);
			}
		}
		return neighbors.toArray();
	}
}

class GraphNode {
	private id : number;
	private xPos : number;
	private yPos : number;

	constructor(id : number, xPos : number, yPos : number) {
		this.id = id;
		this.xPos = xPos;
		this.yPos = yPos;
	}

	distanceTo(to : GraphNode) : number {
		return Math.sqrt(Math.pow(this.xPos-to.xPos, 2)+Math.pow(this.yPos-to.yPos, 2));
	}

	getId() : number {
		return this.id;
	}

	getX() : number {
		return this.xPos;
	}

	getY() : number {
		return this.yPos;
	}
}

class Edge {
	private cost : number;
	private fromNode : GraphNode;
	private endNode : GraphNode;

	constructor(cost : number, fromNode : GraphNode, toNode : GraphNode) {
		this.cost = cost;
		this.fromNode = fromNode;
		this.endNode = toNode;
	}

	compareTo(otherEdge : Edge) : number {
		return this.cost-otherEdge.cost;
	}

	getFromNode() : GraphNode {
		return this.fromNode;
	}

	getEndNode() : GraphNode {
		return this.endNode;
	}
}