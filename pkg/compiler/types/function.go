package types

import (
	"fmt"
	"strings"

	"github.com/rhino1998/aeon/pkg/compiler/kinds"
)

type Function struct {
	Receiver   Type
	Parameters []Type
	Return     Type
}

func NewFunction(recv Type, parameters []Type, ret Type) *Function {
	return &Function{
		Receiver:   recv,
		Parameters: parameters,
		Return:     ret,
	}
}

func (t *Function) ToFunction() *Function {
	if t.Receiver == Void {
		return t
	}

	return &Function{
		Receiver:   Void,
		Parameters: append([]Type{t.Receiver}, t.Parameters...),
		Return:     t.Return,
	}
}

func (t *Function) MethodEqual(o *Function) bool {
	if len(t.Parameters) != len(o.Parameters) {
		return false
	}

	for i := range len(t.Parameters) {
		if !Equal(t.Parameters[i], o.Parameters[i]) {
			return false
		}
	}

	return Equal(t.Return, o.Return)
}

func (t *Function) Kind() kinds.Kind { return kinds.Function }

func (t *Function) String() string {
	params := make([]string, 0, len(t.Parameters))
	for _, param := range t.Parameters {
		params = append(params, string(param.GlobalName()))
	}

	var recvStr string
	if t.Receiver != Void {
		recvStr = fmt.Sprintf("(%s) ", t.Receiver)
	}

	var retStr string
	if t.Return != Void {
		retStr = fmt.Sprintf(" %s", t.Return)
	}

	return fmt.Sprintf("%sfunc(%s)%s", recvStr, strings.Join(params, ", "), retStr)
}

func (t *Function) GlobalName() Name {
	params := make([]string, 0, len(t.Parameters))
	for _, param := range t.Parameters {
		params = append(params, string(param.GlobalName()))
	}

	var recvStr string
	if t.Receiver != Void {
		recvStr = fmt.Sprintf("(%s) ", string(t.Receiver.GlobalName()))
	}

	var retStr string
	if t.Return != Void {
		retStr = fmt.Sprintf(" %s", string(t.Return.GlobalName()))
	}

	return Name(fmt.Sprintf("%sfunc(%s)%s", recvStr, strings.Join(params, ", "), retStr))
}
