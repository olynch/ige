#ifndef _GRAPHS_H
#define _GRAPHS_H

typedef struct Node {
  char * label;
  int x;
  int y;
} Node;

typedef struct Edge {
  char * start_label;
  char * end_label;
} Edge;

typedef struct Graph {
  Node * nodes;
  int len_nodes;
  Edge * edges;
  int len_edges;
} Graph;

Graph * graph_alloc(int, int);
Node * graph_get_node(Graph *, char *);
void graph_dealloc(Graph *);

void node_init(Node *, char *, int, int);
void edge_init(Edge *, char *, char *);

#endif
