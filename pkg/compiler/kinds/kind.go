package kinds

type Kind int

const (
	Unknown Kind = iota
	Void
	Nil
	Bool
	Int
	Float
	String
	Pointer
	Map
	Struct
	Tuple
	Slice
	Array
	Function
	Interface
	Type
	Builtin
	Variadic
)

func (k Kind) IsOperand() bool {
	return k == Bool || k == Int || k == Float || k == String || k == Pointer
}

func (k Kind) IsPrimitive() bool {
	// TODO: maybe pointers here too
	return k == Bool || k == Int || k == Float
}

func (k Kind) String() string {
	switch k {
	case Bool:
		return "bool"
	case Int:
		return "int"
	case Float:
		return "float"
	case String:
		return "string"
	case Pointer:
		return "pointer"
	case Map:
		return "map"
	case Struct:
		return "struct"
	case Tuple:
		return "tuple"
	case Slice:
		return "slice"
	case Array:
		return "array"
	case Function:
		return "function"
	case Interface:
		return "interface"
	case Type:
		return "type"
	case Void:
		return "<void>"
	case Nil:
		return "<nil>"
	case Variadic:
		return "<...>"
	default:
		return "<unknown>"
	}
}

func (k Kind) IsNumeric() bool {
	return k == Int || k == Float
}
