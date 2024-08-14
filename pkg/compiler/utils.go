package compiler

import (
	"cmp"
	"slices"

	"golang.org/x/exp/maps"
)

func sortedMap[S ~string, T any, K cmp.Ordered](m map[S]T, f func(T) K) []T {
	vals := maps.Values(m)

	slices.SortStableFunc(vals, func(a, b T) int {
		return cmp.Compare(f(a), f(b))
	})

	return vals
}

func sortedMapByKey[S ~string, T any](m map[S]T) []T {
	keys := maps.Keys(m)
	slices.SortStableFunc(keys, cmp.Compare)

	ret := make([]T, 0, len(keys))
	for _, key := range keys {
		ret = append(ret, m[key])
	}

	return ret
}

func sortedMapKeysByKey[S ~string, T any](m map[S]T) []S {
	keys := maps.Keys(m)
	slices.SortStableFunc(keys, cmp.Compare)

	return keys
}
