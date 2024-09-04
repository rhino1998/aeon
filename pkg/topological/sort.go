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
	dependencies := make(map[K]map[K]struct{})
	dependents := make(map[K]map[K]struct{})

	dependencylessSet := make(map[K]struct{})

	for _, val := range values {
		key := keyFunc(val)
		valuesByKey[key] = val
	}

	for key, val := range valuesByKey {
		deps := make(map[K]struct{})
		for _, dep := range depFunc(val) {
			depKey := keyFunc(dep)
			if has(valuesByKey, depKey) {
				deps[depKey] = struct{}{}
			}
		}

		if len(deps) > 0 {
			dependencies[key] = deps
		}

		dependencylessSet[key] = struct{}{}
	}

	for key, _ := range valuesByKey {
		for dep := range dependencies[key] {
			if dependents[dep] == nil {
				dependents[dep] = make(map[K]struct{})
			}
			dependents[dep][key] = struct{}{}
		}
	}

	for key, _ := range valuesByKey {
		for dep := range dependents[key] {
			delete(dependencylessSet, dep)
		}
	}

	dependencyless := sortSlice(slices.Collect(maps.Keys(dependencylessSet)))

	list := make([]T, 0)

	for len(dependencyless) > 0 {
		var key K
		key, dependencyless = dependencyless[0], dependencyless[1:]
		list = append(list, valuesByKey[key])

		for _, dep := range sortSlice(slices.Collect(maps.Keys(dependents[key]))) {
			delete(dependencies[dep], key)
			if len(dependencies[dep]) == 0 {
				delete(dependencies, dep)
				dependencyless = append(dependencyless, dep)
			}
		}
		values = append(values, valuesByKey[key])
	}

	if len(dependencies) > 0 {
		return nil, ErrCycleDetected
	}

	return list, nil
}
