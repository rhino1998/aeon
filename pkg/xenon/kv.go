package xenon

import (
	"bytes"
	"cmp"
	"fmt"
	"io"
	"reflect"
	"slices"
	"unicode"
)

var (
	tupleType = reflect.TypeOf(Tuple{})
	mapType   = reflect.TypeOf(Map{})
	sliceType = reflect.TypeOf(Slice{})
)

func marshalXenon(v reflect.Value) ([]byte, error) {
	t := v.Type()

	if marshaller, ok := v.Interface().(XenonMarshaller); ok {
		return marshaller.MarshalXenon()
	}

	switch t.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return []byte(fmt.Sprintf("%d", v.Interface())), nil
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		return []byte(fmt.Sprintf("%d", v.Interface())), nil
	case reflect.Float32, reflect.Float64:
		return []byte(fmt.Sprintf("%f", v.Interface())), nil
	case reflect.String:
		return []byte(v.String()), nil
	case reflect.Bool:
		if v.Bool() {
			return []byte("1"), nil
		} else {
			return []byte("0"), nil
		}
	case reflect.Pointer:
		return marshalXenon(v.Elem())
	case reflect.Map:
		var buf bytes.Buffer

		type pair struct {
			key string
			val reflect.Value
		}
		var pairs []pair

		iter := v.MapRange()
		for iter.Next() {
			key, val := iter.Key(), iter.Value()

			var keyStr string
			switch key.Type().Kind() {
			case reflect.String:
				keyStr = key.String()
			case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
				keyStr = fmt.Sprintf("%d", key.Interface())
			default:
				return nil, fmt.Errorf("unsupport key type %v", key.Type())
			}

			pairs = append(pairs, pair{keyStr, val})
		}

		slices.SortFunc[[]pair, pair](pairs, func(a, b pair) int {
			return cmp.Compare(a.key, b.key)
		})

		for _, pair := range pairs {

			if unicode.IsNumber([]rune(pair.key)[0]) {
				pair.key = "_" + pair.key
			}

			valBytes, err := marshalXenon(pair.val)
			if err != nil {
				return nil, fmt.Errorf("failed to marshal value for key %v: %w", pair.key, err)
			}

			_, _ = io.WriteString(&buf, fmt.Sprintf(".%s{%s}", pair.key, valBytes))
		}

		return buf.Bytes(), nil
	case reflect.Struct:
		val := make(map[string]any)

		for i := 0; i < v.NumField(); i++ {
			fieldType := t.Field(i)
			fieldVal := v.Field(i)

			var key string
			if tag := fieldType.Tag.Get("xc"); tag != "" {
				key = tag
			} else {
				key = fieldType.Name
			}

			val[key] = fieldVal.Interface()
		}

		return MarshalXenon(val)
	case reflect.Interface:
		return MarshalXenon(v.Interface())
	default:
		return nil, fmt.Errorf("unhandled type %v", t)
	}
}

func MarshalXenon(val any) ([]byte, error) {
	v := reflect.ValueOf(val)

	return marshalXenon(v)
}

type XenonMarshaller interface {
	MarshalXenon() ([]byte, error)
}
