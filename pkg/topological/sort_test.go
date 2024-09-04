package topological_test

import (
	"maps"
	"slices"
	"testing"

	"github.com/rhino1998/aeon/pkg/topological"
	"github.com/stretchr/testify/require"
)

type Graph struct {
	nodes map[int]struct{}
	edges map[int]map[int]struct{}
}

func NewGraph() *Graph {
	return &Graph{
		nodes: make(map[int]struct{}),
		edges: make(map[int]map[int]struct{}),
	}
}

func (g *Graph) Nodes() []int {
	nodes := slices.Collect(maps.Keys(g.nodes))
	slices.Sort(nodes)
	return nodes
}

func (g *Graph) NodeEdges(a int) []int {
	edges := slices.Collect(maps.Keys(g.edges[a]))
	slices.Sort(edges)
	return edges
}

func (g *Graph) Add(a, b int) {
	g.nodes[a] = struct{}{}
	g.nodes[b] = struct{}{}
	if _, ok := g.edges[a]; !ok {
		g.edges[a] = make(map[int]struct{})
	}
	g.edges[a][b] = struct{}{}
}

func TestTopologicalSort_Empty(t *testing.T) {
	g := NewGraph()

	r := require.New(t)

	l, err := topological.Sort(g.Nodes(), g.NodeEdges)
	r.NoError(err)
	r.Equal(0, len(l))
}

func TestTopologicalSort_Basic(t *testing.T) {
	g := NewGraph()
	g.Add(1, 2)
	g.Add(2, 3)

	r := require.New(t)

	l, err := topological.Sort(g.Nodes(), g.NodeEdges)
	r.NoError(err)
	r.Equal(3, len(l))
	r.Equal([]int{1, 2, 3}, l)
}

func TestTopologicalSort_Complex(t *testing.T) {
	g := NewGraph()
	g.Add(1, 2)
	g.Add(2, 3)
	g.Add(2, 4)
	g.Add(2, 5)

	g.Add(3, 6)
	g.Add(4, 6)
	g.Add(5, 6)

	r := require.New(t)

	l, err := topological.Sort(g.Nodes(), g.NodeEdges)
	r.NoError(err)
	r.Equal(6, len(l))
	r.Equal([]int{1, 2, 3, 4, 5, 6}, l)
}

func TestTopologicalSort_Cycle(t *testing.T) {
	g := NewGraph()
	g.Add(1, 1)

	r := require.New(t)

	_, err := topological.Sort(g.Nodes(), g.NodeEdges)
	r.Error(err)
}

func TestTopologicalSort_ComplexCycle(t *testing.T) {
	g := NewGraph()
	g.Add(1, 2)
	g.Add(2, 3)
	g.Add(2, 4)
	g.Add(2, 5)

	g.Add(3, 6)
	g.Add(4, 6)
	g.Add(5, 6)
	g.Add(6, 1)

	r := require.New(t)

	_, err := topological.Sort(g.Nodes(), g.NodeEdges)
	r.Error(err)
}
