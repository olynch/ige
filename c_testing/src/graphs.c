#include <stdlib.h>
#include <string.h>
#include "graphs.h"

Graph * graph_alloc(int len_nodes, int len_edges) {
  Graph * g = malloc(sizeof(Graph));
  g->nodes = malloc(sizeof(Node) * len_nodes);
  g->len_nodes = len_nodes;
  g->edges = malloc(sizeof(Edge) * len_edges);
  g->len_edges = len_edges;
  return g;
}

void graph_dealloc(Graph * g) {
  free(g->nodes);
  free(g->edges);
  free(g);
}

Node * graph_get_node(Graph * g, char * label) {
  for (int i = 0; i < g->len_nodes; i++) {
    Node * n = g->nodes + i;
    if (strcmp(n->label, label) == 0) {
      return n;
    }
  }
  return NULL;
}

void node_init(Node * n, char * label, int x, int y) {
  n->label = label;
  n->x = x;
  n->y = y;
}

void edge_init(Edge * n, char * start_label, char * end_label) {
  n->start_label = start_label;
  n->end_label = end_label;
}
