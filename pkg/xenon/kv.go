package xenon

import (
	"bytes"
	"fmt"
	"io"
	"reflect"
	"unicode"
)

var (
	tupleType = reflect.TypeOf(Tuple{})
	mapType   = reflect.TypeOf(Map{})
	sliceType = reflect.TypeOf(Slice{})
)

func marshalKV(v reflect.Value) ([]byte, error) {
	t := v.Type()

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
		return marshalKV(v.Elem())
	case reflect.Map:
		var buf bytes.Buffer

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

			if unicode.IsNumber([]rune(keyStr)[0]) {
				keyStr = "_" + keyStr
			}

			valBytes, err := marshalKV(val)
			if err != nil {
				return nil, fmt.Errorf("failed to marshal value for key %v: %w", keyStr, err)
			}

			_, _ = io.WriteString(&buf, fmt.Sprintf(".%s{%s}", keyStr, valBytes))
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

		return MarshalKV(val)
	case reflect.Interface:
		return MarshalKV(v.Interface())
	default:
		return nil, fmt.Errorf("unhandled type %v", t)
	}
}

func MarshalKV(val any) ([]byte, error) {
	v := reflect.ValueOf(val)

	return marshalKV(v)
}
