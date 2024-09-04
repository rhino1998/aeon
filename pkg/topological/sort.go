package topological

import (
	"fmt"
	"maps"
	"slices"

	"golang.org/x/exp/constraints"
)

var ErrCycleDetected = fmt.Errorf("cycle detected")

func has[M ~map[K]V, K comparable, V any](m M, key K) bool {
	_, ok := m[key]
	return ok
}

func sortSlice[T constraints.Ordered](s []T) []T {
	slices.Sort(s)
	return s
}

func Sort[T constraints.Ordered](values []T, depFunc func(T) []T) ([]T, error) {
	return SortFunc(values, func(val T) T { return val }, depFunc)
}

func SortFunc[T any, K constraints.Ordered](values []T, keyFunc func(T) K, depFunc func(T) []T) ([]T, error) {
	valuesByKey := make(map[K]T)
	graph := make(map[K]map[K]struct{})
	inverseGraph := make(map[K]map[K]struct{})

	edgelessSet := make(map[K]struct{})

	for _, val := range values {
		key := keyFunc(val)
		valuesByKey[key] = val

		deps := make(map[K]struct{})
		for _, dep := range depFunc(val) {
			deps[keyFunc(dep)] = struct{}{}
		}

		if len(deps) > 0 {
			graph[key] = deps
		}

		edgelessSet[key] = struct{}{}
	}

	for key, _ := range valuesByKey {
		for dep := range graph[key] {
			if inverseGraph[dep] == nil {
				inverseGraph[dep] = make(map[K]struct{})
			}
			inverseGraph[dep][key] = struct{}{}

			delete(edgelessSet, dep)
		}
	}

	edgeless := sortSlice(slices.Collect(maps.Keys(edgelessSet)))

	list := make([]T, 0)

	for len(edgeless) > 0 {
		var key K
		key, edgeless = edgeless[0], edgeless[1:]
		list = append(list, valuesByKey[key])

		for _, dep := range sortSlice(slices.Collect(maps.Keys(graph[key]))) {
			delete(inverseGraph[dep], key)
			if len(inverseGraph[dep]) == 0 {
				delete(inverseGraph, dep)
				edgeless = append(edgeless, dep)
			}
		}
		values = append(values, valuesByKey[key])
	}

	if len(inverseGraph) > 0 {
		return nil, ErrCycleDetected
	}

	return list, nil
}
